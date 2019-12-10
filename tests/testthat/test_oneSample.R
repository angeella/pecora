

test_that("oneSample works", {
  
  X <- matrix(rnorm(10*100,5,sd=5),100000,10)
  
  expect_error(oneSample(X,100))
  

})