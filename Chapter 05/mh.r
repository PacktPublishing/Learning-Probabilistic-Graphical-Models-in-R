p = function(x)
{
	dnorm(x,0,1)
}

mh = function(x,alpha)
{
	xt <- runif(1,x-alpha,x+alpha)
	if( runif(1) > p(xt) / p(x) )
		xt <- x

	return(xt)
}

sampler = function(L,alpha)
{
	x <- numeric(L)
	for(i in 2:L)
		x[i] <- mh(x[i-1],alpha)

	return(x)
}

par(mfrow=c(2,2))
for(l in c(10,100,1000,10000))
{
	hist(sampler(l,1),main=paste(l,"iterations"),breaks=50,freq=F,xlim=c(-4,4),ylim=c(0,1))
	lines(x0,p(x0))
}

par(mfrow=c(2,2))
for(a in c(0.1,0.5,1,10))
{
	hist(sampler(50000,a),main=paste("alpha=",a),breaks=50,freq=F,xlim=c(-4,4),ylim=c(0,1))
	lines(x0,p(x0))
}

#x0 <- seq(-4,4,0.1)
#plot( x0, p(x0), lty=2 , t='l')
