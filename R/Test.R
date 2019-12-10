#library(Rcpp)
#library(flip)
#sourceCpp("/Users/Angela Andreella/Documents/Rpackage/signFlip/src/signFlip.cpp")

#X <- matrix(rnorm(10*100000,5,sd=5),100000,10)
#a <- signFlip(X,1000)

#source("/Users/Angela Andreella/Documents/Rpackage/signFlip/R/oneSample.R")

#t1 <- system.time(out1 <- oneSample(X,1000))
#X = t(X)
#t2 <- system.time(out2 <- flip(X,perms = 1000,flipReturn = list(permP=TRUE,permT=TRUE,data=TRUE)))
#t3 <- system.time(out3<-sapply(1:ncol(X), function(i) {
#  mt.sample.teststat(X[,i], c(rep(0,nrow(X)/2,),rep(1,nrow(X)/2)),test="t.equalvar",B=1000)
#}))

#flip doesn't work with large number of observation.


