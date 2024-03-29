% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t2p.R
\name{t2p}
\alias{t2p}
\title{From t-tests to p-values}
\usage{
t2p(Test, alternative = "two.sided", rankBased = TRUE, permReturn = TRUE, df = Inf)
}
\arguments{
\item{Test}{can be a matrix or a vector. 
In the first case the columns represent the B permutations and rows the m tests statistic. 
The observed test statistic is in the first column
and the permutation distribution in the remaining columns.
In the second case, it is a vector of length \eqn{m} of observed tests statistics. 
If \code{rankBased = TRUE}, you must provide the first option (matrix of permuted statistical tests).}

\item{alternative}{character string referring to the alternative hypothesis (\code{"greater"}, \code{"lower"}, or \code{"two.sided"}). is \code{"two.sided"}.}

\item{rankBased}{logical value, \code{TRUE} to compute p-values by permutation distribution. Default @TRUE.}

\item{permReturn}{logical value, \code{TRUE} to return the t-tests and p-values permutation distribution. Default @TRUE.}

\item{df}{numerical value. Degrees of freedom (\eqn{> 0}, maybe non-integer). Default \code{df = Inf}}
}
\value{
Returns an object matrix:
\describe{ 
  \item{pv}{Matrix with dimensions \eqn{m x B} of permuted one-sample p-values. The first column is the p-values for the observed one-sample t-tests.}}
if \code{permReturn = TRUE} otherwise returns:
\describe{ 
  \item{pv}{Vector of \eqn{m} p-values for the observed one-sample t-tests}}
}
\description{
Use permutation distribution of a test statistic to get p-values.
}
\examples{
X <- matrix(rnorm(100*20), nrow=20)
out <- oneSample(X = X)
pv <- t2p(Test = out)
}
\author{
Angela Andreella
}
