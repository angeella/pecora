#' @title Permutatation-based two sample t-test
#' @description Performs two-sample t-tests by permutations.
#' @usage twoSamples(X, B = 1000, alternative, seed = 1234, 
#' exact = TRUE, permReturn = FALSE, label = NULL)
#' @param X data matrix where columns represent the \code{m} variables and rows the \code{n} observations. The columns' name defines the groups' label.
#' @param B numeric value, number of permutations to be performed, including the identity. Default is 1000.
#' @param alternative character string referring to the alternative hypothesis (\code{"greater"}, \code{"lower"}, or \code{"two.sided"}).  
#' @param seed numeric value, specify seed. Default is 1234.
#' @param exact logical value, \code{TRUE} to compute p-values by permutation distribution.
#' @param permReturn logical value, \code{TRUE} to return the t-tests and p-values permutation distribution.
#' @param label by default \code{label = NULL}. Labels of the observations, if \code{NULL} the rows's name are considered.
#' @author Angela Andreella
#' @return Returns a list with the following objects:
#' \describe{ 
#'   \item{Test}{Matrix with dimensions \eqn{m x B} of permuted one-sample t-tests. The first column is the observed one-sample t-tests.}
#'   \item{pv}{Matrix with dimensions \eqn{m x B} of permuted p-values. The first column is the observed p-values.}}
#'   if \code{permReturn = TRUE} otherwise returns a list with the following objects:
#' \describe{ 
#'   \item{Test}{Vector of \eqn{m} observed one-sample t-tests}
#'   \item{pv}{Vector of \eqn{m} observed p-values}} 
#' @export
#' @importFrom stats pnorm
#' @importFrom matrixStats rowRanks
#' @importFrom stats pt
#' @examples 
#' X <- matrix(rnorm(100*20), nrow=20)
#' rownames(X) <- c(rep(0, 10), rep(1,10))
#' out<- twoSamples(X = X) 


twoSamples <- function(X, B = 1000, alternative = "two.sided", 
                      seed = 1234, exact = TRUE, permReturn = FALSE, 
                      label = NULL){
  
  if(!is.null(seed)){set.seed(seed)}
 # alternative_set <- c("greater", "lower", "two.sided")
 # alternative <- match.arg(tolower(alternative), alternative_set)
  
  if(is.null(label)){label <- rownames(X)}
  
  label <- factor(label)
  levels(label) <- c(0,1)
  ## number of obeservation
  n <- nrow(X)
  # number of variables
  m <- ncol(X)
  
  ## Observed test statistics
  id <- levels(label)
  n1 <- sum(label==id[1])
  n2 <- sum(label==id[2])
  colV1 <- colVariances(X[,label == id[1]])
  colV2 <-colVariances(X[,label == id[2]])
  colM1 <- colMeans(X[,label == id[1]])
  colM2 <-colMeans(X[,label == id[2]])
  pooled.var <- (colV1/n1 + colV2/n2)
  Test <- (colM1 - colM2)/sqrt(pooled.var * (1/n1 + 1/n2))
  Test <- ifelse(is.na(Test), 0 , Test)
  ## Test statistics under H0
  Test_H0 <- permGroup(as.matrix(t(X)),B-1,label)
  Test_H0 <- ifelse(is.na(Test_H0), 0 , Test_H0)
  
  Test_matrix <- matrix(cbind(Test, Test_H0), ncol = B)
  gdl <- ((colV1/n1 + colV2/n2)^2)/((((colV1/n1)^2)/(n1-1))+(((colV2/n2)^2)/(n2-1)))
  
  out <- t2p(Test_matrix = Test_matrix, 
             alternative = alternative, 
             exact = exact,
             permReturn = permReturn,
             gdl = gdl)
  
  return(out)
}
