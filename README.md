
The **signflip** package perfoms the one sample t-test using the permutation theory, i.e., sign-flipping. 

To install it:

```{r}
devtools::install_github("angeella/signFlip")

library(signFlip)
```

A toy example:

```{r}
X <- matrix(rnorm(10*100000,5,sd=5),100000,10)

system.time(out1 <- oneSample(X,1000))
```


