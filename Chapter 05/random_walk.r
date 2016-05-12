# lets first simulate a bivariate normal sample
library(MASS)
library(mvtnorm)
bigauss <- mvrnorm(50000, mu = c(0, 0), Sigma = matrix(c(1, .1, .1, 1), 2))
bigauss.estimate <- kde2d(bigauss[,1], bigauss[,2], n = 50)
contour(bigauss.estimate,nlevels=6,lty=2)

bigauss <- mvrnorm(50000, mu = c(0, 0), Sigma = matrix(c(.1, .01, .01, .1), 2))
bigauss.estimate <- kde2d(bigauss[,1], bigauss[,2], n = 50)
contour(bigauss.estimate,nlevels=6,col=2,add=TRUE)

L <- 1000
smallcov <- matrix(c(.1,.01,.01,.1),2)
x <- c(0,0)
for(i in 1:L)
{
	x2 <- mvrnorm(1, mu=x, Sigma=smallcov)
	lines(c(x[1],x2[1]), c(x[2],x2[2]), t='p',pch=20)
	x <- x2
}
