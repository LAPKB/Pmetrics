#' @title Compare discrete distributions
#' @description 
#' `r lifecycle::badge("stable")`
#' Multivariate two-sample test based on k-nearest neighbors
#'
#' @details
#' This function tests whether two samples share the same underlying 
#' distribution based on k-nearest-neighbors approach.
#' Matrices or data frames x and y are the two samples to be tested. 
#' Each row consists of the coordinates of a data point. 
#' The integer k is the number of nearest neighbors to choose in the 
#' testing procedure. This approach is robust in the unbalanced case.
#' 
#' @param x A matrix or data frame.
#' @param y A matrix or data frame.
#' @param k k An integer.
#' @param clevel The confidence level. Default value is 0.05.
#' @param getpval Logic value. If it is set to be TRUE the p value of 
#' test will be calcuated and reported; if it is set to be false the 
#' p value will not be calculated.
#' @param print Boolean value. If it is set to be TRUE the test 
#' result will be reported; if it is set to be FALSE the test 
#' result will not be reported.
#' @return A list consists of the test statistics, 
#' normalized Z score and corresponding P value.  
#' @author Lisha Chen (Yale), Peng Dai (Stonybrook) and Wei Dou (Yale)
#' @references Schilling, M. F. (1986). Multivariate two-sample tests based on nearest neighbors. \emph{J. Amer. Statist. Assoc.}, 81 799-806.
#' Henze, N. (1988). A multivariate two-sample test based on the number of nearest neighbor type coincidences.\emph{Ann. Statist.}, 16 772-783.
#' Chen, L. and Dou W. (2009). Robust multivariate two-sample tests based on k nearest neighbors for unbalanced designs. \emph{manuscripts}.  
#' @useDynLib Pmetrics knn

mtsknn.eq <- function(x,y,k,clevel=0.05,getpval=TRUE, print=TRUE)
{
# x, y are matrices or data frame with each row containing the coordinates of data points
# and with the last colume being the flags.

# preparation:

 x <- as.matrix(x)
 y <- as.matrix(y)
 if(ncol(x)!=ncol(y)) return("The dimensions of two samples must match!!!")

 d <- ncol(x)
 n1 <- nrow(x)
 n2 <- nrow(y)


 b <- log(1/(1-clevel))

 if(n1 > n2){
    temp <- x
    x <- y
    y <- temp
    n1 <- nrow(x)
    n2 <- nrow(y)
 }


 q <- as.integer(n2/n1)
 m <- as.integer(n2/q)
 r <- n2-m*q
 starts <- seq(1,(q*m+1),by=m)
 if(r>0)starts <- c(starts[1:(q-r+1)],(starts[(q-r+2):(q+1)]+seq(1,r,by=1)))

 adjust.cl <- b/q

 y.permuted <- as.matrix(y[sample(c(1:n2)),])

 x <- cbind(x,rep(1,n1))

 K <- 0
 reject <- 0
 Zmax <- -Inf
 for(i in 1:q){
        y.sub <- as.matrix(y.permuted[starts[i]:(starts[i+1]-1),])
        n2.sub <- nrow(y.sub)
        y.sub <- cbind(y.sub,rep(2,n2.sub))
        Set <- rbind(x,y.sub)
        tSet <- t(Set)
        n <- n1+n2.sub
        output <- rep(0,n)
        C.out <- .C("knn", as.double(tSet), as.integer(n), as.integer(d), as.integer(k), as.integer(output))
        counts <- C.out[[5]]
        Tk <- sum(counts[(n1+1):n])/(n2.sub*k)
        # Z scores and P values
        #V <- 3/8  ?
        V <- (n1*(n2.sub-1))/((n-1)*(n-2)) + ((n2.sub-1)*n1*(n1-1))/((n)*(n-2)*(n-3))
        Z <- sqrt(n2.sub*k)*(Tk-(n2.sub-1)/(n-1))/sqrt(V)
        P <- pnorm(Z, lower.tail=FALSE)

        if (getpval==TRUE){
          if(P < adjust.cl) {
            if (reject==0){
              K.out <- K
              reject <- 1

            }
          }
          K <- K+1

          if (Z>Zmax) Zmax <- Z
        } else{#getpval==FALSE
            if (P<adjust.cl){
              reject <- 1
              K.out <- K
              break
            }
            K <- K+1
        }

      }
 pval <- NULL
 if (getpval==TRUE)
   pval <- 1- exp(-q*(1-pnorm(Zmax,lower.tail=TRUE)))
 if (reject==0) K.out <- q

 if (print)  print(paste("proc1: q=",q,"  K=",K.out))

 return(list (pval=pval,reject=reject))
}


mtsknn = function(x,y,k)
{
  # x, y are matrices or data frame with each row containing the coordinates of data points 
  # and with the last colume being the flags.  
  
  # preparation:
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(ncol(x)!=ncol(y))return("The dimensions of two samples must match!!!")
  
  d <- ncol(x)
  n1 <- nrow(x)
  n2 <- nrow(y)
  n <- n1+n2
  lam1 <- n1/(n1+n2)
  lam2 <- n2/(n1+n2)
  
  # add flags 
  x <- cbind(x,rep(1,n1))
  y <- cbind(y,rep(2,n2))
  Set <- rbind(x,y)
  tSet <- t(Set)
  
  output <- rep(0,n)   
  C.out <- .C("knn", as.double(tSet), as.integer(n), as.integer(d), as.integer(k), as.integer(output))
  counts <- C.out[[5]]
  Tk <- sum(counts)/(n*k)
  # Z scores and P values
  #V <- lam1*lam2+4*lam1^2*lam2^2
  V <- (n1-1)*(n2-1)/(n-1)^{2}+4*((n1-1)*(n1-2)/((n-1)*(n-2)))*((n2-1)*(n2-2)/((n-1)*(n-2)))
  #Z <- (n*k)^(1/2)*(Tk-lam1^2-lam2^2)/sqrt(V)
  Z <- (n*k)^(1/2)*(Tk-(n1-1)*(n1-2)/((n-1)*(n-2))-(n2-1)*(n2-2)/((n-1)*(n-2)))/sqrt(V)
  P <- pnorm(Z, lower.tail=F)
  
  output <- list(Tk,Z,P)
  names(output) <- c("T Statistics","Z Score","P Value")
  
  return(output)
}





