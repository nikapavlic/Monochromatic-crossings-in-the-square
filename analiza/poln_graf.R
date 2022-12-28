
library(rjson)
library(dplyr)
library(ggplot2)


#Naredi poln graf:


poln_graf <- function(datoteka){
  dat <- data.frame(fromJSON(file = datoteka))                     #prebere JSON
  graf <- merge(dat,dat, by = NULL)                                #kartezicni produkt
  colnames(graf) <- c("x1","y1","x2","y2")                        #spremeni imena
  graf <- filter(graf, graf$x1!= graf$x2 & graf$y1 != graf$y2)
  #graf$naklon <- (graf$y2-graf$y1)/(graf$x2-graf$x1) #zracuna naklon
  graf$temp <- apply(graf, 1, function(x) paste(sort(x), collapse=""))
  graf <- graf[!duplicated(graf$temp), 1:4]                       #ostanejo daljice, ki se ne ponavljajo
}

  

graf <- poln_graf(datoteka = "podatki/tocke_100.json")



# funkcija za leve zavoje:

Z <- function(x1,y1,x2,y2,x3,y3){
  x1*(y2-y3)+x2*(y3-y1)+x3*(y1-y2)
  #det(matrix(c(1,1,1,x1,x2,x3,y1,y2,y3),3,3))
}

sekata <- function(a,b,c,d){ifelse(a*b < 0 & c*d <0, TRUE, FALSE)}

naklon <- function(x1,y1,x2,y2){(y2-y1)/(x2-x1)}


presecisca <- function(graf){
  pr <- merge(graf, graf, by = NULL)
  colnames(pr) <- c("x1","y1","x2","y2","x3","y3","x4","y4")
  pr$temp <- apply(pr, 1, function(x) paste(sort(x), collapse=""))
  pr <- pr[!duplicated(pr$temp), 1:8]
  pr <- filter(pr, !(pr$x1 == pr$x3 & pr$x2 == pr$x4))             #ostanejo nepodvojene kombinacije daljic, ki bi se lahko sekale
  
  pr$Z123 <- Z(pr$x1,pr$y1,pr$x2,pr$y2, pr$x3,pr$y3)
  pr$Z124 <- Z(pr$x1,pr$y1,pr$x2,pr$y2, pr$x4,pr$y4)
  pr$Z134 <- Z(pr$x1,pr$y1,pr$x3,pr$y3, pr$x4,pr$y4)
  pr$Z234 <- Z(pr$x2,pr$y2,pr$x3,pr$y3, pr$x4,pr$y4)
  pr$sekata <- sekata(pr$Z123, pr$Z124, pr$Z134,pr$Z234)
  
  pr <- filter(pr, pr$sekata == TRUE)
  pr$k1 <- naklon(pr$x1, pr$y1, pr$x2, pr$y2)
  pr$k2 <- naklon(pr$x3, pr$y3, pr$x4, pr$y4)
  pr$X <- (pr$y3-pr$y1+pr$k1*pr$x1-pr$k2*pr$x3)/(pr$k1-pr$k2) #preseèišèe X-koordinata
  pr$Y <- pr$y1 + pr$k1*(pr$X-pr$x1)
  pr
}

c <-presecisca(graf)



#a in b v radianih
barvanje <- function(a,b,naklon){
  k1 <- tan(a)
  k2 <- tan(b)
  barva <- ifelse(k1 < naklon & naklon < k2, "M", "R")
  barva
}

#recimo
a <- pi/4
b <- pi/2

c$barva1 <- barvanje(a,b,c$k1)
c$barva2 <- barvanje(a,b,c$k2)
c$krom <- ifelse(c$barva1 == c$barva2, "mono", "bi")

ggplot(c, aes(X,Y, colour = krom))+geom_point()

