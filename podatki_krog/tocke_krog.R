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
for(i in c(5:50)){tocke_krog(i)}

