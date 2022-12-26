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
tocke_krog(10)
for(i in seq(100,10000,100)){tocke_krog(i)}


