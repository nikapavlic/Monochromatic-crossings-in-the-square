library(rjson)
library(dplyr)
library(ggplot2)
library(stringr)


#a in b v radianih, a in b iz intervala [-pi/2, pi/2]
barvanje <- function(a,b,naklon){
  k1 <- tan(a)
  k2 <- tan(b)
  barva <- ifelse(k1 < naklon & naklon < k2, "M", "R")
  barva
}



#recimo
#a <- -pi/4
#b <- pi/4

#c$barva1 <- barvanje(a,b,c$k1)
#c$barva2 <- barvanje(a,b,c$k2)
#c$krom <- ifelse(c$barva1 == c$barva2, "mono", "bi")

#ggplot(c, aes(X,Y, colour = krom))+geom_point()



krom <- function(datoteka,a,b){
  c <- data.frame(fromJSON(file = datoteka))
  c$barva1 <- barvanje(a,b,c$k1)
  c$barva2 <- barvanje(a,b,c$k2)
  c$krom <- ifelse(c$barva1 == c$barva2, "mono","bi")
  ggplot(c, aes(X,Y, colour = krom)) + geom_point()
  c
}






krom("presecisca/presecisca_7.json",-pi/4,pi/4)






#ideje za grafe:


proba <- krom("presecisca/presecisca_30.json", 0, pi/4)
proba %>% ggplot(aes(X,Y, colour = krom)) + geom_point()+coord_fixed(ratio = 1)+xlim(0,1)+ylim(0,1)


proba %>% filter(krom == "mono") %>% ggplot()+aes(X,Y)+ 
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
  xlim(0,1) + ylim(0,1) + coord_fixed(ratio = 1)


proba %>% filter(krom == "bi") %>% ggplot()+aes(X,Y)+
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
  xlim(0,1)+ylim(0,1) +coord_fixed(ratio = 1)

proba %>% filter(krom == "mono") %>% ggplot()+aes(X,Y)+ geom_hex() +
  theme_bw()+xlim(0,1)+ylim(0,1)+coord_fixed(ratio = 1)




proba_krog <- krom("presecisca_krog/presecisca_20_krog.json", -pi/8, pi/2)

proba_krog %>% filter(krom == "mono") %>% ggplot()+aes(X,Y)+ geom_hex() +
  theme_bw() +xlim(-1,1)+ylim(-1,1)+coord_fixed(ratio = 1)

proba_krog %>% filter(krom == "bi") %>% ggplot()+aes(X,Y)+ geom_hex() +
  theme_bw() +xlim(-1,1)+ylim(-1,1)+coord_fixed(ratio = 1)

