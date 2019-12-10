#include <RcppArmadillo.h>
#include <cmath> /* pow */
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]] 
// [[Rcpp::export]] 
arma::mat signFlip(arma::mat X, double B) {
  int m = X.n_rows;
  int n = X.n_cols;
  
  arma::mat T(m, B, arma::fill::zeros);
  
  arma::vec eps, Tb, id, Tb2, Tbp, Ts;
  
  int bb;
  for (bb=0; bb<B; bb++) {
    eps = Rcpp::rbinom(n, 1, 0.5)*2 - 1;  // signs
    id = Rcpp::rbinom(n, 1, 1)*2 - 1; //identity
    Tb = X * eps;
    Tb = Tb /n; //mean
    Tb2 = pow (Tb,2)/pow (n,2); //mean power 2
    Tbp = (pow (X,2) * id)/n; //power 2 mean
    Ts = Tb/ pow ((Tbp-Tb2)/n *n/(n-1), 0.5); //final test
    T.col(bb) = Ts;
    
  }
  return (T);
}

/*** R
#m <- 100
#n <- 10
#B <- 200
#X <- matrix(rnorm(m*n), ncol=n)
#set.seed(123)
#T <- signFlip(X, B)
#str(T)
*/