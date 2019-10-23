mtsknn.neq <- function(x,y,k,delta=1.05,clevel=0.05,seed=12345,getpval=TRUE, print=TRUE, max.loop=20, level.seq="decrease")
{
# x, y are matrices or data frame with each row containing the coordinates of data points
# and with the last colume being the flags.
# delta is :  q= (delta^p) - 1, the subgroup i has size ~ n1*delta^i
# level.seq =c("decrease","equal")
# max.loop means after loop 20, the test statistics will be generated from a standard normal instead of being computed from the sample

 root= uniroot(func,c(0.001,1), a=clevel)$root
# preparation:
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

 b <- log(1/(1-clevel))

 q <- as.integer(n2/n1)

 p <- as.integer(log((delta-1)*q+1,  delta))  #beta^p -1 ==q

 end=NULL

 end.temp=0
 for (i in 1:(p)){
   if( i==1){
     start =1
   } else {start = c(start, floor(end.temp)+1)}

   end.temp= (end.temp+ n1*delta^(i-1))
   end= c(end,floor(end.temp))


 }
 end[p]= n2




 adjust.cl <- b/p
 set.seed(seed)
 y.permuted <- as.matrix(y[sample(1:n2),])

 K <- 0
 reject <- 0
 y.temp <- NULL

 Zmax <- -Inf

 for(i in 1:p){
   if (i<max.loop){
   x.sub <- rbind(x,y.temp)
   n1.sub <- nrow(x.sub)
   x.sub <- cbind(x.sub,rep(1,n1.sub)) # this line is corrected Nov 23, 2009

   y.sub <- as.matrix(y.permuted[start[i]:end[i],])
   y.temp <- rbind(y.temp, y.sub)
   n2.sub <- nrow(y.sub)
   y.sub <- cbind(y.sub,rep(2,n2.sub))

   Set <- rbind(x.sub,y.sub)
   tSet <- t(Set)
   n.sub <- n1.sub+n2.sub
   output <- rep(0,n.sub)
   C.out <- .C("knn", as.double(tSet), as.integer(n.sub), as.integer(d), as.integer(k), as.integer(output))
   counts <- C.out[[5]]
   Tk <- sum(counts)/(n.sub*k)

   # Z scores and P values
   #V <- lam1*lam2+4*lam1^2*lam2^2
   lam1 <- (n1.sub-1)/(n.sub-1)
   lam2 <- (n2.sub-1)/(n.sub-1)
   V <-  lam1*lam2 + 4*lam1^2 * lam2^2
   #temp <- (n1.sub-n2.sub)^2/(n.sub-2) + 1
   #temp1 <- 4*(n1.sub-1)/(n.sub-2)*(n2.sub-1)*k/n.sub
   #temp2 <- ( (n1.sub-n2.sub)/(n.sub-2)*(n1.sub-n2.sub) -1) *k*k/n.sub
   #V <- k*n1.sub/(n.sub-1)*n2.sub * ( 1-k/(n.sub-1)*temp)   +n1.sub/(n.sub-3)*n2.sub*(temp1+temp2)


   demean <- Tk  - (n1.sub/n.sub*(n1.sub-1)+ n2.sub/n.sub*(n2.sub-1))  /(n.sub-1)

   #Z <- n.sub*k/sqrt(V) * demean
   Z <- sqrt(n.sub*k)/sqrt(V)*demean
   } else{ Z <- rnorm(1)}
   P <- pnorm(Z, lower.tail=F)
   #print (paste("i=",i))
   #print (paste("Tk=",round(Tk,4), "demean=",round(demean,4),"V=",round(V,4),"Z=",round(Z,4),"prob=",round(P,4),"b/p=",round(adjust.cl,4)))
   if ( level.seq=="equal" ) {
       level.i <- adjust.cl
     } else {level.i <- (root/i)^2}

   if (getpval==TRUE) {
     if (Z>Zmax) Zmax <- Z
      if(P < level.i) {
       if (reject ==0) {
         K.out <- K
         reject <- 1
       }
     }
     K <- K+1
   }else{ # if getpval==FALSE
     if(P < level.i) {
       reject <- 1
       K.out <- K
       break
     }
     K <- K+1
   }

 }# end for


 if (reject==0) K.out <- p
 if (print==TRUE)
   print(paste("proc2: P=",p,"  K=",K.out))

 #if(K.out==p) {
 #  reject <- 0
 #} else{ reject <- 1}
 if (getpval==TRUE)
   pval <- 1- exp(-p*(1-pnorm(Zmax,lower.tail=TRUE)))
 else pval <- NULL
 set.seed(ceiling(runif(1, min=0,max=100000)))
 return(list(pval=pval , reject=reject))
}
