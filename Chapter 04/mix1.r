library(mixtools)

N <- 400

X <- list(
	mvrnorm(N, c(1,1), matrix(c(1,-0.5,-0.5,1),2,2)/4),
	mvrnorm(N, c(3,3), matrix(c(2,0.5,0.5,1),2,2)/4),
	mvrnorm(N, c(5,5), matrix(c(1,-0.5,-0.5,4),2,2)/4))

plot(0,0,xlim=c(-1,7),ylim=c(-1,7),type='n')
for(i in 1:3)
	points(X[[i]],pch=18+i, col=1+i)
