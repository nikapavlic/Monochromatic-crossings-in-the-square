library(rjson)
library(dplyr)
library(ggplot2)
library(stringr)


#funkcija, ki JSON presecisc da v data frame



#a in b v radianih, a in b iz intervala [-pi/2, pi/2]
barvanje <- function(a,b,naklon){
  k1 <- tan(a)
  k2 <- tan(b)
  barva <- ifelse(k1 < naklon & naklon < k2, "M", "R")
  barva
}



#recimo
a <- -pi/4
b <- pi/4

c$barva1 <- barvanje(a,b,c$k1)
c$barva2 <- barvanje(a,b,c$k2)
c$krom <- ifelse(c$barva1 == c$barva2, "mono", "bi")

ggplot(c, aes(X,Y, colour = krom))+geom_point()





krom <- function(datoteka,a,b){
  graf <- poln_graf(datoteka)
  c <- presecisca(graf)
  c$barva1 <- barvanje(a,b,c$k1)
  c$barva2 <- barvanje(a,b,c$k2)
  c$krom <- ifelse(c$barva1 == c$barva2, "mono","bi")
  ggplot(c, aes(X,Y, colour = krom)) + geom_point()
  c
}







proba <- krom("podatki/tocke_50.json", 0, pi/4)

ggplot(proba, aes(X,Y, colour = krom))+geom_point()

proba %>% filter(krom == "mono") %>% ggplot()+aes(X,Y)+ stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)


proba %>% filter(krom == "bi") %>% ggplot()+aes(X,Y)+ stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)

proba_krog <- krom("podatki_krog/tocke_40_krog.json", -pi/8, pi/2)

proba_krog %>% filter(krom == "mono") %>% ggplot()+aes(X,Y)+ geom_hex() +
  theme_bw()

proba_krog %>% filter(krom == "bi") %>% ggplot()+aes(X,Y)+ geom_hex() +
  theme_bw()

