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
  
  
  
  
  
  


