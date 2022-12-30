library(spatstat)
#library(jsonlite)
library(rjson)
library(stringr)

tocke <- function(n){
  t <- rpoint(n)
  X <- t$x
  Y <- t$y
  plot(X,Y)
  json <- toJSON(list(X = X,Y = Y))#, indent = 1)
  write(json,str_glue("tocke_{n}.json"))
  
}


set.seed(23)
for(i in c(5:50)){tocke(i)}
