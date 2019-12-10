

test_that("oneSample works", {
  
  X <- matrix(rnorm(10*100,5,sd=5),10,100)
  
  expect_output(oneSample(X,100))
  

})