
library(rjson)
library(dplyr)



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

  

graf <- poln_graf(datoteka = "podatki_krog/tocke_10_krog.json")



# funkcija za leve zavoje:

Z <- function(x1,y1,x2,y2,x3,y3){
  det(matrix(c(1,1,1,x1,x2,x3,y1,y2,y3),3,3))
}


presecisca <- function(graf){
  pr <- merge(graf, graf, by = NULL)
  colnames(pr) <- c("x1","y1","x2","y2","x3","y3","x4","y4")
  pr$temp <- apply(pr, 1, function(x) paste(sort(x), collapse=""))
  pr <- pr[!duplicated(pr$temp), 1:8]
  pr <- filter(pr, !(pr$x1 == pr$x3 & pr$x2 == pr$x4))             #ostanejo nepodvojene kombinacije daljic, ki bi se lahko sekale
  #pr$Z123 <- det(matrix(c(1,1,1,pr$x1,pr$y1,pr$x2,pr$y2, pr$x3,pr$y3),3,3))
  #pr$Z123 <- Z(pr$x1,pr$y1,pr$x2,pr$y2, pr$x3,pr$y3)
  pr
}

c <-presecisca(graf)
c$Z123 <- Z(c$x1,c$y1,c$x2,c$y2,c$x3,c$y3)  #nekje napaka



barvanje <- function(a,b,naklon){
  k1 <- tan(b)
  k2 <- tan(a)
  barva <- ifelse(k1 < naklon & naklon < k2, "M", "R")
  barva
}





