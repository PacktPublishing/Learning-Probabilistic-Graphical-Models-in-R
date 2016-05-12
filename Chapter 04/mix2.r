library(mixtools)

N <- 400

X <- list(
	mvrnorm(100, c(1,1), matrix(c(1,-0.5,-0.5,1),2,2)/4),
	mvrnorm(200, c(3,3), matrix(c(2,0.5,0.5,1),2,2)/4),
	mvrnorm(300, c(5,5), matrix(c(1,-0.5,-0.5,4),2,2)/4))
x <- do.call(rbind,X)
