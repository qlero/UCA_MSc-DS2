rm(list=ls())


### 1a)

n=100
p=20

X=matrix(0,ncol=2*p+1,nrow=n)
X[,1]=rep(1,n)
u=2*pi*(0:(n-1))/n
for(i in 1:p)
{
  X[,2*i]=cos(i*u)
  X[,2*i+1]=sin(i*u)
}


### 1b) for the scalar product, it sufficient to compute

A=t(X)%*%X

### remark how all the non diagonal terms are almost zero

sum((A-diag(diag(A),nrow=2*p+1,ncol=2*p+1))^2)

Xprime=X%*%diag((diag(A))^(-1/2),nrow=2*p+1,ncol=2*p+1)

B=t(Xprime)%*%Xprime
diag(B)
sum((B-diag(diag(B),nrow=2*p+1,ncol=2*p+1))^2)

### 1c)

Four_estim=function(Y,d,X=Xprime)
{
  n=length(Y)
  est=rep(0,n)
  for(i in 1:(2*d+1))
  {
    est=est+sum(X[,i]*Y)*X[,i]
  }
  return(est)
}

### 1d)

Y=16+14*cos(u)+5*rnorm(n)

plot(u,Y)
lines(u,16+14*cos(u),col='black',main='Fourier projection estimator for various d')
lines(u,Four_estim(Y,1),col='red')
lines(u,Four_estim(Y,2),col='green')
lines(u,Four_estim(Y,10),col='blue')
lines(u,Four_estim(Y,20),col='cyan')
legend('bottomright',c('truth','d=1','d=2','d=10','d=20'),lty=c(1,1,1,1,1),col=c('black','red','green','blue','cyan'))

Y=10*exp(-(u-pi)^2/(0.2))+1*rnorm(n)

plot(u,Y)
lines(u,10*exp(-(u-pi)^2/(0.2)),col='black',main='Fourier projection estimator for various d')
lines(u,Four_estim(Y,1),col='red')
lines(u,Four_estim(Y,5),col='green')
lines(u,Four_estim(Y,10),col='blue')
lines(u,Four_estim(Y,20),col='cyan')
legend('bottomright',c('truth','d=1','d=5','d=10','d=20'),lty=c(1,1,1,1,1),col=c('black','red','green','blue','cyan'))

### 1e)


MallowsCp=function(Y,sigma,X=Xprime)
{
  p=floor(ncol(X)/2)
  Crit=rep(0,p)
  for(d in 1:p)
  {
    est=Four_estim(Y,d,X)
    Crit[d]=sum((Y-est)^2)+2*sigma^2*(2*d+1)
  }
  ind=which.min(Crit)
  return(list(crit=Crit,est=Four_estim(Y,ind,X)))
}

par(mfrow=c(1,2))

Y=16+14*cos(u)+5*rnorm(n)

plot(u,Y)
lines(u,16+14*cos(u),col='black',main='Fourier projection estimator for various d')
lines(u,MallowsCp(Y,5)$est,col='red')
legend('bottomright',c('truth','Mallows'),lty=c(1,1),col=c('black','red'))

plot(1:p,MallowsCp(Y,5)$crit,main='Mallows Cp',ylab='')

par(mfrow=c(1,2))

Y=10*exp(-(u-pi)^2/(0.2))+1*rnorm(n)

plot(u,Y)
lines(u,10*exp(-(u-pi)^2/(0.2)),col='black',main='Fourier projection estimator for various d')
lines(u,MallowsCp(Y,1)$est,col='red')
legend('bottomright',c('truth','Mallows'),lty=c(1,1),col=c('black','red'))

plot(1:p,MallowsCp(Y,1)$crit,main='Mallows Cp',ylab='')

### 1f)

Thresh=function(Y,sigma=1,X=Xprime,gamma=1)
{
  p=floor(ncol(X)/2)
  n=length(Y)
  coeff=rep(0,2*p+1)
  est=rep(0,n)
  for(i in 1:(2*p+1))
  {
    if(abs(sum(Y*X[,i]))>sqrt(gamma*log(n)*sigma^2))
    {
      print(i)  
      coeff[i]=sum(Y*X[,i])
      est=est+sum(Y*X[,i])* X[,i]
    }
  }
  return(list(est=est,coeff=coeff)
}


Y=16+14*cos(u)+5*rnorm(n)

plot(u,Y)
lines(u,16+14*cos(u),col='black',main='Fourier projection estimator for various d')
lines(u,Thresh(Y,5,gamma=1)$est,col='red')
legend('bottomright',c('truth','BIC'),lty=c(1,1),col=c('black','red'))

Y=10*exp(-(u-pi)^2/(0.2))+1*rnorm(n)

plot(u,Y)
lines(u,10*exp(-(u-pi)^2/(0.2)),col='black',main='Fourier projection estimator for various d')
lines(u,Thresh(Y,1,gamma=1)$est,col='red')
legend('bottomright',c('truth','BIC'),lty=c(1,1),col=c('black','red'))

