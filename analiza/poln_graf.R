
library(rjson)
library(dplyr)
library(ggplot2)
library(stringr)


#Naredi poln graf:

poln_graf <- function(datoteka) {
  dat <- data.frame(fromJSON(file = datoteka))                     #prebere JSON
  dat <- mutate(dat, i = 1:nrow(dat))                              #doda indekse
  graf <- merge(dat, dat, by = NULL)                               #kartezicni produkt
  colnames(graf) <- c("x1", "y1", "i", "x2", "y2", "j")            #spremeni imena
  graf <- filter(graf, i < j)
  graf <- graf %>% select(-i, -j) %>% mutate(i = 1:nrow(graf))
}


# funkcija za leve zavoje:

Z <- function(x1,y1,x2,y2,x3,y3){
  x1*(y2-y3)+x2*(y3-y1)+x3*(y1-y2)
  #det(matrix(c(1,1,1,x1,x2,x3,y1,y2,y3),3,3))
}

sekata <- function(a,b,c,d){ifelse(a*b < 0 & c*d <0, TRUE, FALSE)}

naklon <- function(x1,y1,x2,y2){(y2-y1)/(x2-x1)}


presecisca <- function(graf){
  pr <- merge(graf, graf, by = NULL)
  
  colnames(pr) <- c("x1", "y1", "x2", "y2", "i", "x3", "y3", "x4", "y4", "j")
  pr <- filter(pr, i < j)
  
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


graf <- poln_graf(datoteka = "podatki/tocke_5.json")
c <-presecisca(graf)


plot(c(graf$x1, graf$x2), c(graf$y1, graf$y2))
segments(graf$x1, graf$y1, graf$x2, graf$y2)




#funkcija ki data frame presecisc prepise v JSON, (X;Y;k1;k2):

pr <- function(datoteka){
  ime <- str_extract_all(datoteka,"(\\d){1,2}(_){0,1}[krog]{0,4}")
  ob <- ifelse(str_extract_all(ime,"_{0,1}[krog]{4}")=="character(0)", "",str_extract_all(ime,"_{0,1}[krog]{4}"))
  graf <- poln_graf(datoteka)
  pr <- presecisca(graf)
  pr <- pr[,16:19]
  json <- toJSON(list(k1 = pr$k1, k2=pr$k2,X=pr$X,Y=pr$Y))
  json
  write(json, str_glue("presecisca{ob}/presecisca_{ime}.json"))
}


pr("podatki/tocke_5.json")
pr("podatki_krog/tocke_7_krog.json")



for (i in c(5:50)) {
  dat <- str_glue("podatki/tocke_{i}.json")
  pr(dat)
}


for (i in c(5:50)) {
  dat <- str_glue("podatki_krog/tocke_{i}_krog.json")
  pr(dat)
  }

