devtools::install_github("angeella/signFlip")
library(signFlip)
X <- matrix(rnorm(10*100000,5,sd=5),100000,10)

system.time(out1 <- oneSample(X,1000))
