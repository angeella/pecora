#' @title Permutatation-based one sample t-test
#' @description Performs sign-flipped one-sample t-tests.
#' @usage oneSample(X, B = 1000, alternative, seed = 1234, 
#' exact = TRUE, permReturn = TRUE)
#' @param X data matrix where columns represent the \code{m} variables and rows the \code{n} observations.
#' @param B numeric value, number of permutations to be performed, including the identity. Default is 1000.
#' @param alternative character string referring to the alternative hypothesis (\code{"greater"}, \code{"lower"}, or \code{"two.sided"}). 
#' @param seed numeric value, specify seed. Default is 1234.
#' @param exact logical value, \code{TRUE} to compute p-values by permutation distribution.
#' @param permReturn logical value, \code{TRUE} to return the t-tests and p-values permutation distribution.
#' @author Angela Andreella
#' @return Returns a list with the following objects:
#' \describe{ 
#'   \item{Test}{Matrix with dimensions \eqn{m \times B} of permuted one-sample t-tests. The first column is the observed one-sample t-tests.}
#'   \item{pv}{Matrix with dimensions \eqn{m \times B} of permuted p-values. The first column is the observed p-values.}}
#'   if \code{permReturn = TRUE} otherwise returns a list with the following objects:
#' \describe{ 
#'   \item{Test}{Vector of \eqn{m} observed one-sample t-tests}
#'   \item{pv}{Vector of \eqn{m} observed p-values}} 
#' @export
#' @importFrom Rcpp evalCpp
#' @importFrom stats pt
#' @examples 
#' X <- matrix(rnorm(100*20), nrow=20)
#' out <- oneSample(X = X, alternative = "two.sided")
#' @useDynLib pecora, .registration = TRUE

oneSample <- function(X, B = 1000, 
                      alternative,
                      seed = 1234,exact = TRUE,
                     permReturn = TRUE){

  alternative <- match.arg(tolower(alternative), c("greater", "lower", "two.sided"))
  
  set.seed(seed)

  ## number of obeservation
  n <- nrow(X)
  # number of variables
  m <- ncol(X)
  
  ## Observed test statistics
  colV <- colVariances(X)
  Test <- ifelse(colV==0,0, colMeans(X)/(sqrt((colV)/n)))
  
  ## Test statistics under H0
  Test_H0 <- signFlip(t(X),B-1)
  Test_H0 <- ifelse(is.na(Test_H0), 0 , Test_H0)
  Test_matrix <- matrix(cbind(Test, Test_H0), ncol = B)
  gdl <- n-1
  out <- t2p(Test_matrix = Test_matrix, 
             alternative = alternative, 
             exact = exact,
             permReturn = permReturn,
             gdl = gdl)
  

  
  return(out)
  
}