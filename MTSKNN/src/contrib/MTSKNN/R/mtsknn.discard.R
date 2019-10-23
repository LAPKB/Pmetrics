mtsknn.discard <- function (x, y, k)
{
 x <- as.matrix(x)
 y <- as.matrix(y)
 if(ncol(x)!=ncol(y))return("The dimensions of two samples must match!!!")

 d <- ncol(x)
 n1 <- nrow(x)
 n2 <- nrow(y)

 if(n1 > n2){
    temp <- x
    x <- y
    y <- temp
    n1 <- nrow(x)
    n2 <- nrow(y)
 }
 sub <- sample(1:n2)[1:n1]
 #pval <- mtsknn(x,y[sub,],k)$P
 #return(pval)
  output <- mtsknn(x,y[sub,],k)

  return(output)
}



