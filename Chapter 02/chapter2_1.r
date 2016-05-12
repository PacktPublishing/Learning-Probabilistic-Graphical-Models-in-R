# Sum-product and beliefs update examples
A=matrix(c(.8,.2),2,1)
B=matrix(c(.6,.4,.3,.7),2,2)
C=matrix(c(.5,.5,.8,.8),2,2)
D=matrix(c(.3,.7,.4,.6),2,2)

Bs = t(A) %*% t(B)
Cs = Bs %*% t(C)
Ds = Cs %*% t(D)
Ds
