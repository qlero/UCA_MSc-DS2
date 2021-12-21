##### question 1 a)

kern_est=function(X,h,u)
{
  n=length(X)
  p=length(u)
  bigU=matrix(rep(u,n),ncol=n)
  bigX=t(matrix(rep(X,p),ncol=p))
  est=rowMeans(dnorm((bigU-bigX)/h))/h
  return(est)
}

X=rexp(100,1)
u=seq(0,10,0.01)
h=0.3
plot(u,exp(-u),type='l',main='kernel estimator',ylab='density',xlab='u')
rug(X)
lines(u,kern_est(X,h,u),col='red')
legend('topright',c('truth','kernel estimator'),lty=c(1,1),col=c('black','red'))

### question 1c)

est=function(u){kern_est(X,h,u)}
est2=function(u){kern_est(X,h,u)^2}
integrate(est2,0,Inf)

contrast=function(Y,X,h)
{
  est=function(u){kern_est(X,h,u)}
  est2=function(u){kern_est(X,h,u)^2}
  return(-2*mean(est(Y))+integrate(est2,0,Inf)$value)
}

### question 1d)

hgrid=seq(0.01,1,0.01)
holdout=function(X,hgrid)
{
  nh=length(hgrid)
  cont=rep(0,nh)
  n=length(X)
  indice=sample(1:n, floor(n/2),replace=FALSE)
  Xest=X[indice]
  Xval=X[-indice]
  for(ih in 1:nh)
  {
    cont[ih]=contrast(Xval,Xest,hgrid[ih])
  }
  hchosen=hgrid[which.min(cont)]
  return(list(contrast=cont,h=hchosen ))
}

X=rexp(100,1)
u=seq(0,10,0.01)
h=holdout(X,hgrid)$h
print(h)
plot(u,exp(-u),type='l',main='kernel estimator',ylab='density',xlab='u')
rug(X)
lines(u,kern_est(X,h,u),col='red')
legend('topright',c('truth','kernel estimator'),lty=c(1,1),col=c('black','red'))

### question 1e)

Vfold=function(X,hgrid,V)
{
  nh=length(hgrid)
  cont=rep(0,nh)
  n=length(X)
  new_indice=sample(1:n, n,replace=FALSE) # to cut in V at random, I permute all indexes and then I cut in the classical order
  L=floor(n/V)
  for(ih in 1:nh)
  {
    for(v in 1:V)
    {
      indiceval=((v-1)*L+1):(v*L)
      Xval=X[indiceval]
      Xest=X[-indiceval]
      cont[ih]=cont[ih]+contrast(Xval,Xest,hgrid[ih])
    }
  }
  cont=cont/V
  
  hchosen=hgrid[which.min(cont)]
  return(list(contrast=cont,h=hchosen ))
}

X=rexp(100,1)
u=seq(0,10,0.01)
h=Vfold(X,hgrid,5)$h
print(h)
plot(u,exp(-u),type='l',main='kernel estimator',ylab='density',xlab='u')
rug(X)
lines(u,kern_est(X,h,u),col='red')
legend('topright',c('truth','kernel estimator'),lty=c(1,1),col=c('black','red'))

### Question 2a)

theta_0=2
n=50
X=rexp(n,theta_0)

### Question 2b)
hat_theta_obs=1/mean(X)

### Question 2c) Ideal confidence interval

Nsimu=10000
dist=rep(0,Nsimu)
for(i in 1:Nsimu)
{
  X_surrogate=rexp(n,theta_0)
  hat_theta=1/mean(X_surrogate)
  dist[i]=abs(hat_theta-theta_0)
}

epsilon=quantile(dist,0.95)
IC1=c(hat_theta_obs-epsilon,hat_theta_obs+epsilon)

### Question 2d) Parametric bootstrap confidence interval
Nvect=seq(10,10000,100)

IC=matrix(0,ncol=length(Nvect),nrow=2)
for(iN in 1:length(Nvect))
{
  N=Nvect[iN]
  dist_boot=rep(0,N)
  for(i in 1:N)
  {
    X_boot=rexp(n,hat_theta_obs)
    hat_theta=1/mean(X_boot)
    dist_boot[i]=abs(hat_theta-hat_theta_obs)
  }

  epsilon_boot=quantile(dist_boot,0.95)
  IC2=c(hat_theta_obs-epsilon_boot,hat_theta_obs+epsilon_boot)
  IC[,iN]=IC2
}

#### for what remains, let us do a function

bootstrap_IC=function(X,N=5000, level=0.05)
{
  hat_theta_obs=1/mean(X)
  n=length(X)
  
  dist_boot=rep(0,N)
  for(i in 1:N)
  {
    X_boot=rexp(n,hat_theta_obs)
    hat_theta=1/mean(X_boot)
    dist_boot[i]=abs(hat_theta-hat_theta_obs)
  }
  
  epsilon_boot=quantile(dist_boot,1-level)
  return(c(hat_theta_obs-epsilon_boot,hat_theta_obs+epsilon_boot))
} 

### question 2e)
plot(Nvect,IC[1,],col='black',type='l',ylim=c(0,4))
lines(Nvect,IC[2,],col='blue')
lines(Nvect, rep(IC1[1],length(Nvect)),col='red')
lines(Nvect, rep(IC1[2],length(Nvect)),col='magenta')

### question 2f)

library(STAR)

data(e070528citronellal)
delays_pre=c()

level=0.05/3 # because we will compare 3 IC and we want to control the global error so we use Bonferroni correction

for(i in 1:15)
{
  spike=as.vector(e070528citronellal[["neuron 1"]][[i]])
  index=which(spike<6.14)
  delays_pre=c(delays_pre,diff(spike[index]))
}


IC_pre=bootstrap_IC(delays_pre,N=5000,level=level)

delays_puff=c()

for(i in 1:15)
{
  spike=as.vector(e070528citronellal[["neuron 1"]][[i]])
  index=which((spike>6.14)&(spike<6.64))
  delays_puff=c(delays_puff,diff(spike[index]))
}

IC_puff=bootstrap_IC(delays_puff,N=5000,level=level)


delays_post=c()

for(i in 1:15)
{
  spike=as.vector(e070528citronellal[["neuron 1"]][[i]])
  index=which((spike>6.64))
  delays_post=c(delays_post,diff(spike[index]))
}

IC_post=bootstrap_IC(delays_post,N=5000,level=level)

IC=cbind(IC_pre,IC_puff,IC_post)
### all three (level corrected) confidence intervals are disjoints so all three parameters are distinct

#### Question 3a)

Firing_pre=rep(0,15)
Firing_puff=rep(0,15)
Firing_post=rep(0,15)

for(i in 1:15)
{
  spike=as.vector(e070528citronellal[["neuron 1"]][[i]])
  Firing_pre[i]=length(which(spike<6.14))/6.14
  Firing_puff[i]=length(which((spike>6.14)&(spike<6.64)))/(6.64-6.14)
  Firing_post[i]=length(which(spike>6.64))/(13-6.64)
}


### Question 3b)


NP_bootstrap_IC=function(X,N=5000,level=0.05)
{
  Xbar=mean(X)
  n=length(X)
  Xbar_star=rep(0,N)
  
  for(i in 1:N)
  {
    X_star=sample(X,n,replace=TRUE) ### the Efron bootstrap sample
    Xbar_star[i]=mean(X_star)
  }
  
  q=quantile(Xbar_star-Xbar,prob=c(level/2,1-level/2))
  return(c(Xbar-q[2],Xbar-q[1]))
}

X=rnorm(100,1)
NP_bootstrap_IC(X)

X=rpois(100,5)
NP_bootstrap_IC(X)

X=rpois(15,5)
NP_bootstrap_IC(X)

## it seems that the mean is inside the interval, seems to work even with a not too big  sample size

### Question 3c)

IC=cbind(NP_bootstrap_IC(Firing_pre,level=0.05/3),NP_bootstrap_IC(Firing_puff,level=0.05/3),NP_bootstrap_IC(Firing_post,level=0.05/3))

### There ius still a big difference between the three conditions.


### Question 4 a)

setwd("~/Desktop/travail/program/ingrid-francesca")

load('action_potential.Rdata')

neuron= 1 # try 10, 20 34 

plot((1:32)*0.0001,Record[[1]][,neuron],type='l',xlab='s',ylab='',main='Action potential',ylim=c(-1000,1000))
lines((1:32)*0.0001,Record[[2]][,neuron],col='red')
lines((1:32)*0.0001,Record[[3]][,neuron],col='blue')
lines((1:32)*0.0001,Record[[4]][,neuron],col='green')
legend('topright',c('Electrode 1','Electrode 2', 'Electrode 3','Electrode 4'),lty=c(1,1,1,1),col=c('black','red','blue','green'))


### Question 4b)

half_height=rep(0,86)
peak_valley=rep(0,86)

for(neuron in 1:86)
{
  Max=c(max(Record[[1]][,neuron]),max(Record[[2]][,neuron]),max(Record[[3]][,neuron]),max(Record[[4]][,neuron]))
  elec=which.max(Max) # the electrode to look at
  trace=Record[[elec]][,neuron] # the trace we look at
  M=max(trace) # value of the peak
  pos=which(trace>(M/2))
  half_height[neuron]=(pos[length(pos)]-pos[1])*0.0001
  indM=which.max(trace)
  indm=which.min(trace[indM[1]:32]) # the valley has to appear after the peak ...
  peak_valley[neuron]=indm[1]*0.0001
}

### Question 4c)

Rhalf= (half_height-mean(half_height))/sd(half_height)
Rval=(peak_valley-mean(peak_valley))/sd(peak_valley)

x=cbind(Rhalf,Rval)

plot(hclust(dist(x),method="ward.D2"))

z=kmeans(x,2)

ind1=which(z$cluster==1)
ind2=which(z$cluster==2)
mycolor=rep(0,86)
mycolor[ind1]='black'
mycolor[ind2]='red'

plot(peak_valley, half_height,col=mycolor)

