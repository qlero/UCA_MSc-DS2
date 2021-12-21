rm(list=ls())
#Exercise 1

#a)
f_h=rep(1,100)
h=0.15
x=rexp(100,2)
u=c(h,x)
for(sim in 1:100){
  n=1:100
  K=(exp*(((-x)^2)*2)/(sqrt(2*pi)))
  f_h[sim]=(1/n*h)*sum(K*((u-X_i)/h))
}

plot(density(f_h),bty='n')

#Exercise 2

#a
n=50 #number of simulations 
theta_0=2 #number of observations
X=rexp(n,theta_0) #simulation
