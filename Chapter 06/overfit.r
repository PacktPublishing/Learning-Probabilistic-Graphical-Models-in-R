N <- 30
true_beta <- c(10, -3, 0, 8, 0, 0, 0, 0, 0)

x <- runif(N, -2, 2)
X <- cbind(rep(1, N), x, x^2, x^3, x^4, x^5, x^6, x^7, x^8)
matplot(X, t='l')

sigma <- 10
eps <- rnorm(N, mean = 0, sd = sigma)
y <- X %*% true_beta + eps
plot(y,t='l')

model <- lm(y~., data=data.frame(X[,2:ncol(X)]))
beta_hat <- model$coefficients

plot( beta_hat, t='o', col=2, pch='x')
lines(true_beta, t='o', col=1)

# Prior precision
dimension <- length(true_beta)
lambda <- 0.1*diag(0.1, dimension, dimension)

# Posterior covariance
posterior_sigma <- sigma^2 * solve(t(X) %*% X + sigma^2 * lambda)
posterior_beta  <- sigma^(-2) * as.vector(posterior_sigma %*% (t(X) %*% y))

t <- seq(-2,2,0.01)
T <- cbind(rep(1, N), t, t^2, t^3, t^4, t^5, t^6, t^7, t^8)
plot(x,y, xlim=c(-2,2), ylim=range(y, T%*%true_beta))
lines(t,T%*%true_beta, col='black', lwd=3)
lines(t,T%*%beta_hat,  col='blue',  lwd=3)
lines(t,T%*%posterior_beta, col='red', lwd=3)
legend('topleft', c('True function', 'OLS estimate', 'Bayesian estimate'), col=c('black','blue','red'), lwd=3)

pred_sigma <- sqrt(sigma^2 + apply((T%*%posterior_sigma)*T, MARGIN=1, FUN=sum))
upper_bound <- T%*%posterior_beta + qnorm(0.95)*pred_sigma
lower_bound <- T%*%posterior_beta - qnorm(0.95)*pred_sigma

plot(c(0,0),xlim=c(-2,2), ylim=range(y,lower_bound,upper_bound),col='white')
polygon( c(t,rev(t)), c(upper_bound,rev(lower_bound)), col='grey', border=NA)
points(x,y)
lines(t,T%*%true_beta, col='black', lwd=3)
lines(t,T%*%beta_hat,  col='blue',  lwd=3)
lines(t,T%*%posterior_beta, col='red', lwd=3)
legend('topleft', c('True function', 'OLS estimate', 'Bayesian estimate'), col=c('black','blue','red'), lwd=3)
