library(spatstat)
#library(jsonlite)
library(rjson)
library(stringr)

tocke_krog <- function(n){
  k <- runifdisc(n)
  X <- k$x
  Y <- k$y
  plot(X,Y)
  json <- toJSON(list(X = X,Y = Y))#, indent = 1)
  write(json,str_glue("tocke_{n}_krog.json"))
  
}

set.seed(23)
for (i in seq(10,100,10)) {tocke_krog(i)}
for(i in seq(100,1000,100)){tocke_krog(i)}
for(i in seq(1000,10000,1000)){tocke_krog(i)}


data.frame(fromJSON(file = "tocke_10_krog.json"))
