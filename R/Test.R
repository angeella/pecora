# X0 <- matrix(rnorm(10*100,0,1),10,100)
# X1 <- matrix(rnorm(10*10,3,1),10,10)
# X <- cbind(X0,X1)
# H0 <- 100
# H1 <- 10
# tests <- oneSample(t(X), 200)
# dim(pvalues)
# pch <- 20
# colStat <- c(rep("red",H0),rep("black",H1))
# plot(tests$T, col=colStat, main="Test statistics", pch=pch, ylab= 'Test stat')
# legend("topleft", c("H0", "H1"), pch=pch, col=1:2)
