x1=runif(40,0,10)
x2=runif(40,10,20)

e1 = rnorm(20,0,2)
e2 = rnorm(20,0,3)

y1 = 1+2.5*x1 + e1
y2 = 35+-1.5*x2 + e2

xx=c(x1,x2)
yy=c(y1,y2)

x0 = seq(0,20,.1)
m0 = lm(yy~xx)
m1 = lm(y1~x1)
m2 = lm(y2~x2)

