oneSample <- function(X, B, 
                      alternative = c("two.sided", "less", "greater"),
                      rowTestFUN = rowWelchTests,
                      rand.p.value = FALSE, seed = NULL){
  rowVars <- function (x,na.rm = TRUE) 
  {
    sqr = function(x) x * x
    n = rowSums(!is.na(x))
    n[n <= 1] = NA
    return(rowSums(sqr(x - rowMeans(x,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
  }
  
  library(matrixStats)
  library(Rcpp)
  #sourceCpp("/Users/Angela Andreella/Documents/Rpackage/signFlip/src/signFlip.cpp")
  alternative <- match.arg(alternative)
  
  n <- ncol(X) #number of variables
  m <- nrow(X) #number of observations
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ## observed test statistics and p-values
  T <- rowMeans(X)/(sqrt(rowVars(X)/n))
  p <- switch(alternative, 
              #"two.sided" = 2*(1 - pnorm(abs(T))),
              "two.sided" = 2*(pnorm(abs(T), lower.tail=FALSE)),
              #"greater" = 1 - pnorm(T),
              "greater" = pnorm(T, lower.tail=FALSE),
              #"less" = pnorm(T))
              "less" = 1-pnorm(T, lower.tail=FALSE))
  ## test statistics under H0
  T0 <- signFlip::signFlip(X,B)
  p0 <- switch(alternative, 
               #"two.sided" = 2*(1 - pnorm(abs(T0))),
               "two.sided" = 2*(pnorm(abs(T0), lower.tail=FALSE)),
               #"greater" = 1 - pnorm(T0),
               "greater" = pnorm(T0, lower.tail=FALSE),
               "less" = 1-pnorm(T0, lower.tail=FALSE))
  #"less" = pnorm(T0))
  res <- list(T = T, T0 = T0, p = p, p0 = p0)
  
  if (rand.p.value) {
    ## get m x (B+1) matrix of pvalues under the null (+ original)
    ## by sorting null test statistics as proposed by Ge et al (2003)
    TT <- cbind(T0, T)
    pB <- switch(alternative, 
                 "two.sided" = rowRanks(-abs(TT)) / (B+1),
                 "greater" = rowRanks(-TT) / (B+1),
                 "less" = rowRanks(TT) / (B+1))
    
    res$rand.p <- pB[, B+1]
    res$rand.p0 <- pB[, -(B+1), drop = FALSE]
  }
  return(res)
}