#' @title Permutatation-based one sample t-test
#' @description Performs sign-flipping, i.e. permutation, one-sample t-tests
#' @usage oneSample(X, B = 1000, alternative = "two.sided", seed = NULL)
#' @param X data where rows represents the variables and columns the observations
#' @param B number of permutations to perform, default is 1000.
#' @param alternative character referring to the alternative hypothesis, "two.sided", "greater" or "less". Default is "two.sided"
#' @param rand logical. Should p values computed by permutation distribution?
#' @param seed specify seed, default is 1234.
#' @author Angela Andreella
#' @return Returns a list with the following objects: \code{Test} observed one sample t-test, \code{Test_H0} Test statistics under H0, \code{pv} observed p-values, \code{pv_H0} p-values under H0
#' @export
#' @importFrom stats pnorm
#' @importFrom matrixStats rowRanks

oneSample <- function(X, B = 1000, 
                      alternative = c("two.sided", "lower", "greater"),
                      rand = FALSE, seed = 1234){

  alternative_set <- c("two.sided", "greater", "lower")
  
  set.seed(seed)
  
  alternative <- match.arg(tolower(alternative), alternative_set)
  
  ## number of obeservation
  n <- ncol(X)
  # number of variables
  m <- nrow(X)
  
  ## Observed test statistics
  rowV <- rowVariance(X)
  Test <- ifelse(rowV==0,0, rowMeans(X)/(sqrt((rowV)/n)))
  
  ## Test statistics under H0
  
  Test_H0 <- signFlip(X,B)
  Test_H0 <- ifelse(is.na(Test_H0), 0 , Test_H0)
  
  if(!rand){
    pv <- switch(alternative, 
                 "two.sided" = 2*(pnorm(abs(Test), lower.tail=FALSE)),
                 "greater" = pnorm(Test, lower.tail=FALSE),
                 "lower" = 1-pnorm(Test, lower.tail=FALSE))
    
    pv_H0 <- switch(alternative, 
                    "two.sided" = 2*(pnorm(abs(Test_H0), lower.tail=FALSE)),
                    "greater" = pnorm(Test_H0, lower.tail=FALSE),
                    "lower" = 1-pnorm(Test_H0, lower.tail=FALSE))
  }else{
    
    Test_matrix <- cbind(Test, Test_H0)
    pv_matrix <- switch(alternative, 
                        "two.sided" = rowRanks(-abs(Test_matrix)) / (B+1),
                        "greater" = rowRanks(-Test_matrix) / (B+1),
                        "lower" = rowRanks(Test_matrix) / (B+1))
    
    pv <- pv_matrix[, 1]
    pv_H0 <- pv_matrix[, 2:(B+1)]
  }
  
  out <- list(Test = Test, Test_H0 = Test_H0, pv = pv, pv_H0 = pv_H0)
  return(out)
}