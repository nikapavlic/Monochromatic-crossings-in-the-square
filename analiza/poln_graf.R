
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
  pr$X <- (pr$y3-pr$y1+pr$k1*pr$x1-pr$k2*pr$x3)/(pr$k1-pr$k2) #presecisce X-koordinata
  pr$Y <- pr$y1 + pr$k1*(pr$X-pr$x1)
  pr$dol_1 <- ((pr$x2-pr$x1)^2+(pr$y2-pr$y1)^2)^0.5
  pr$dol_2 <- ((pr$x4-pr$x3)^2+(pr$y4-pr$y3)^2)^0.5
  pr
}


graf <- poln_graf(datoteka = "podatki/tocke_20.json")
c <- presecisca(graf)

windows()
plot(c(graf$x1, graf$x2), c(graf$y1, graf$y2))
segments(graf$x1, graf$y1, graf$x2, graf$y2)

krog <- poln_graf(datoteka = "podatki_krog/tocke_20_krog.json")
c_krog <- presecisca(krog)
razdalje <- ((krog$x1-krog$x2)^2 + (krog$y1-krog$y2)^2)^0.5

windows()
plot(c(krog$x1, krog$x2), c(krog$y1, krog$y2))
segments(krog$x1, krog$y1, krog$x2, krog$y2)

#funkcija ki data frame presecisc prepise v JSON, (X;Y;k1;k2):

pr <- function(datoteka){
  ime <- str_extract_all(datoteka,"(\\d){1,2}(_){0,1}[krog]{0,4}")
  ob <- ifelse(str_extract_all(ime,"_{0,1}[krog]{4}")=="character(0)", "",str_extract_all(ime,"_{0,1}[krog]{4}"))
  graf <- poln_graf(datoteka)
  pr <- presecisca(graf)
  pr <- pr[,16:21]
  json <- toJSON(list(k1 = pr$k1, k2=pr$k2,X=pr$X,Y=pr$Y,dol_1=pr$dol_1,dol_2=pr$dol_2))
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
barvanje_dol <- function(a,b,dolzina){

  barva <- ifelse(a < dolzina & dolzina < b, "M", "R")
  barva
}

racunanje_kotov <- function(data_presecisc){
  koti <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
  f <- 1

  for (i in 0:20){

    for (j in (i+1):20){

      koti[f,1] <- i
      koti[f,2] <- j

      lev_i <- -pi/2 + pi*0.05*i
      des_i <- -pi/2 + pi*0.05*j

      data_presecisc$barva1 <- barvanje(lev_i,des_i,data_presecisc$k1)
      data_presecisc$barva2 <- barvanje(lev_i,des_i,data_presecisc$k2)
      data_presecisc$krom <- ifelse(data_presecisc$barva1 == data_presecisc$barva2, "mono", "bi")

      koti[f,5] <- table(data_presecisc$krom)[1]
      koti[f,3] <- lev_i
      koti[f,4] <- des_i

      f <- f + 1
    }
  }
  return(koti)
}

racunanje_min_kotov <- function(data_presecisc){
  koti <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
  f <- 1

  for (i in 0:20){

    for (j in (i+1):20){

      koti[f,1] <- i
      koti[f,2] <- j

      lev_i <- -pi/2 + pi*0.05*i
      des_i <- -pi/2 + pi*0.05*j

      data_presecisc$barva1 <- barvanje(lev_i,des_i,data_presecisc$k1)
      data_presecisc$barva2 <- barvanje(lev_i,des_i,data_presecisc$k2)
      data_presecisc$krom <- ifelse(data_presecisc$barva1 == data_presecisc$barva2, "mono", "bi")

      koti[f,5] <- table(data_presecisc$krom)[1]
      koti[f,3] <- lev_i
      koti[f,4] <- des_i

      f <- f + 1
    }
  }
  minimumi <- koti[which(koti$obarvanost == min(koti$obarvanost)),]

  razlike <- minimumi$del2 - minimumi$del1
  interval <- minimumi[which.max(razlike),]

  return(interval)
}

#iskanje_min <- function(podatki_koti){
#  minimumi <- podatki_koti[which(podatki_koti$obarvanost == min(podatki_koti$obarvanost)),]

  #razlike <- minimumi$del2 - minimumi$del1
  #interval <- minimumi[which.max(razlike),]

#  return(minimumi)
#}

dolzina_pres_bar <- function(data_presecisc){
  data_presecisc$barva1 <- barvanje_dol(0.3,0.5,data_presecisc$dol_1)
  data_presecisc$barva2 <- barvanje_dol(0.3,0.5,data_presecisc$dol_2)
  data_presecisc$krom <- ifelse(data_presecisc$barva1 == data_presecisc$barva2, "mono", "bi")
  return(data_presecisc)
}
print(dolzina_pres_bar(c))

racunanje_dolzin <- function(data_presecisc){
  dolzine <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
  f <- 1

  for (i in 0:20){
    for (j in (i+1):20){
      dolzine[f,1] <- i
      dolzine[f,2] <- j

      lev_i <- i*0.05*2^0.5
      des_i <- j*0.05*2^0.5

      data_presecisc$barva1 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_1)
      data_presecisc$barva2 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_2)
      data_presecisc$krom <- ifelse(data_presecisc$barva1 == data_presecisc$barva2, "mono", "bi")

      dolzine[f,5] <- table(data_presecisc$krom)[1]
      dolzine[f,3] <- lev_i
      dolzine[f,4] <- des_i

      f <- f + 1
    }
  }
  return(dolzine)
}

racunanje_max_dolzin <- function(data_presecisc){
  dolzine <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
  f <- 1

  for (i in 0:20){
    for (j in (i+1):20){
      dolzine[f,1] <- i
      dolzine[f,2] <- j

      lev_i <- i*0.05*2^0.5
      des_i <- j*0.05*2^0.5

      data_presecisc$barva1 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_1)
      data_presecisc$barva2 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_2)
      data_presecisc$krom <- ifelse(data_presecisc$barva1 == data_presecisc$barva2, "mono", "bi")

      dolzine[f,5] <- table(data_presecisc$krom)[1]
      dolzine[f,3] <- lev_i
      dolzine[f,4] <- des_i

      f <- f + 1
    }
  }
  drugi_max <- dolzine[which(dolzine$obarvanost == max(dolzine$obarvanost[dolzine$obarvanost!=max(dolzine$obarvanost)])),]

  razlike <- drugi_max$del2 - drugi_max$del1
  interval <- drugi_max[which.max(razlike),]

  return(interval)
}
print(racunanje_dolzin(c))

racunanje_dolzin_krog <- function(data_presecisc){
  dolzine_krog <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
  f <- 1

  for (i in 0:20){
    for (j in (i+1):20){
      dolzine_krog[f, 1] <- i
      dolzine_krog[f, 2] <- j

      lev_i <- i*0.05*2
      des_i <- j*0.05*2

      data_presecisc$barva1 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_1)
      data_presecisc$barva2 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_2)
      data_presecisc$krom <- ifelse(data_presecisc$barva1 == data_presecisc$barva2, "mono", "bi")

      dolzine_krog[f,5] <- table(data_presecisc$krom)[1]
      dolzine_krog[f,3] <- lev_i
      dolzine_krog[f,4] <- des_i

      f <- f + 1
    }
  }
  return(dolzine_krog)
}

racunanje_max_dolzin_krog <- function(data_presecisc){
  dolzine_krog <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
  f <- 1

  for (i in 0:20){
    for (j in (i+1):20){
      dolzine_krog[f, 1] <- i
      dolzine_krog[f, 2] <- j

      lev_i <- i*0.05*2
      des_i <- j*0.05*2

      data_presecisc$barva1 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_1)
      data_presecisc$barva2 <- barvanje_dol(lev_i,des_i,data_presecisc$dol_2)
      data_presecisc$krom <- ifelse(data_presecisc$barva1 == data_presecisc$barva2, "mono", "bi")

      dolzine_krog[f,5] <- table(data_presecisc$krom)[1]
      dolzine_krog[f,3] <- lev_i
      dolzine_krog[f,4] <- des_i

      f <- f + 1
    }
  }
  drugi_max <- dolzine_krog[which(dolzine_krog$obarvanost == max(dolzine_krog$obarvanost[dolzine_krog$obarvanost!=max(dolzine_krog$obarvanost)])),]

  razlike <- drugi_max$del2 - drugi_max$del1
  interval <- drugi_max[which.max(razlike),]

  return(interval)
}

skupni_min <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
skupni_max_dolzin <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))


for (i in c(5:50)) {

  podatki_graf <- str_glue("podatki/tocke_{i}.json")
  zac_graf <- poln_graf(datoteka = podatki_graf)
  pres_graf <- presecisca(zac_graf)

  iti_min_kot <- racunanje_min_kotov(pres_graf)
  iti_max_dolzina <- racunanje_max_dolzin(pres_graf)

  skupni_min <- rbind(skupni_min, iti_min_kot)
  skupni_max_dolzin <- rbind(skupni_max_dolzin, iti_max_dolzina)

}
skupni_min <- skupni_min[-c(1,2),]
skupni_max_dolzin <- skupni_max_dolzin[-c(1,2),]


skupni_min_krog <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))
skupni_max_dolzin_krog <- data.frame(del1 = c(1:2), del2 = c(1:2), levi_int = c(1:2), desni_int = c(1:2), obarvanost = c(1:2))

for (i in c(5:50)) {

  podatki_krog <- str_glue("podatki_krog/tocke_{i}_krog.json")
  zac_krog <- poln_graf(datoteka = podatki_krog)
  pres_krog <- presecisca(zac_krog)
  iti_min_krog <- racunanje_min_kotov(pres_krog)
  iti_max_krog_dolzin <- racunanje_max_dolzin_krog(pres_krog)

  skupni_min_krog <- rbind(skupni_min_krog, iti_min_krog)
  skupni_max_dolzin_krog <- rbind(skupni_max_dolzin_krog, iti_max_krog_dolzin)

}

skupni_min_krog <- skupni_min_krog[-c(1,2),]
skupni_max_dolzin_krog <- skupni_min_dolzin_krog[-c(1,2),]


frekvencni <- data.frame(int = c(1:20), pojavitve = rep(0, 20))
frekvencni_dolzin <- data.frame(int = c(1:20), pojavitve = rep(0, 20))

for (j in c(1:20)){
  frekvencni[j,1] <- -pi/2 + pi*0.05*(j-1)
  frekvencni_dolzin[j,1] <- 0.05*(j-1)*2^0.5
}
for (i in c(1:length(skupni_min))){
  for (k in c(1:20)){
    if (frekvencni[k, 1] >= skupni_min[i, 3] & frekvencni[k, 1] < skupni_min[i, 4]){
      frekvencni[k, 2] <- frekvencni[k, 2] + 1
    }
    if (frekvencni_dolzin[k, 1] >= skupni_min_dolzin[i, 3] & frekvencni_dolzin[k, 1] < skupni_min_dolzin[i, 4]){
      frekvencni_dolzin[k, 2] <- frekvencni_dolzin[k, 2] + 1
    }
  }
}

frekvencni_krog <- data.frame(int = c(1:20), pojavitve = rep(0, 20))
frekvencni_krog_dolzin <- data.frame(int = c(1:20), pojavitve = rep(0, 20))

for (j in c(1:20)){
  frekvencni_krog[j,1] <- -pi/2 + pi*0.05*(j-1)
  frekvencni_krog_dolzin[j,1] <- 0.05*2*(j-1)
}

for (i in c(1:length(skupni_min_krog))){
  for (k in c(1:20)){
    if (frekvencni_krog[k, 1] >= skupni_min_krog[i, 3] & frekvencni_krog[k, 1] < skupni_min_krog[i, 4]){
      frekvencni_krog[k, 2] <- frekvencni_krog[k, 2] + 1
    }
    if (frekvencni_krog_dolzin[k, 1] >= skupni_min_dolzin_krog[i, 3] & frekvencni_krog_dolzin[k, 1] < skupni_min_dolzin_krog[i, 4]){
      frekvencni_krog_dolzin[k, 2] <- frekvencni_krog_dolzin[k, 2] + 1
    }
  }
}
