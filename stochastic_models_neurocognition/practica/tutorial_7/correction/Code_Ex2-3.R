rm(list=ls())

### Exercise 1 qurstion c/

ThinPoisson=function(Tmax,n,type)
{
  if(type==1){M=n} # type 1,n*exp()
  if(type==2){M=2*n} # type 2 , n*(1+sin())
  
  # just another way to simulate homogeneous Poisson
  Ntot=rpois(1,lambda=(Tmax*M))
  points=runif(Ntot,max=Tmax,min=0)
  marks=runif(Ntot,max=M,min=0)
  
  accepted=c()
  for(i in 1:Ntot)
  {
    if((type==1)&(marks[i]<n*exp(-points[i])))
    {
      accepted=c(accepted,points[i])
    }
    if((type==2)&(marks[i]<n*(1+sin(points[i]))))
    {
      accepted=c(accepted,points[i])
    }
  }
  return(accepted)
}

MLE_pcw=function(points,d,Tmax)
{
   myloglik=0
   ahat=rep(0,d)
  for(i in 1:d)
  {
    Nbi=length(which((points>=((i-1)*Tmax/d))&(points<(i*Tmax/d))))
    ahat[i]=Nbi/Tmax*d
    myloglik=myloglik+log(ahat[i])*Nbi-ahat[i]*Tmax/d
  }
  return(list(coeff=ahat,logl=myloglik)) 
}

type=2
n=1000
Tmax=10
d=20 # try 2 and 200 ....

points=ThinPoisson(Tmax,n,type)

x=seq(0,Tmax,0.001)
if(type==1)
{ 
  plot(x,n*exp(-x),type='l',main='Poisson intensity reconstruction', xlab='time',ylab='intensity')
}else{
  plot(x,n*(1+sin(x)),type='l',main='Poisson intensity reconstruction', xlab='time',ylab='intensity')
}
lines((0:d)*Tmax/d,c(MLE_pcw(points,d,Tmax)$coeff,0),type='s',col='red') ### NB option type='s' make it as a step funcion, just need to be careful with beginning and end
  
### question d/

AIC=function(points,dvec,Tmax)
{
  ld=length(dvec)
  crit=rep(0,ld)
  for(i in 1:ld)
  {
    crit[i]=-MLE_pcw(points,dvec[i],Tmax)$logl + dvec[i] # 
  }
  dhat=dvec[which.min(crit)]
  return(list(dhat=dhat,crit=crit))
}

dvec=1:100
dhat=AIC(points,dvec,Tmax)$dhat

lines((0:dhat)*Tmax/dhat,c(MLE_pcw(points,dhat,Tmax)$coeff,0),type='s',col='green') ### NB option type='s' make it as a step funcion, just need to be careful with beginning and end
legend('bottomleft',c('truth',paste('d=',d),paste('dAIC=',dhat)),col=c('black','red','green'),lty=c(1,1,1))

### On STAR data

library(STAR)

data(e070528citronellal)

Spikes_N1=c()

for(i in 1:15)
{
  spikes=as.vector(e070528citronellal[["neuron 1"]][[i]])
  Spikes_N1=c(Spikes_N1,spikes)
}

Spikes_N2=c()

for(i in 1:15)
{
  spikes=as.vector(e070528citronellal[["neuron 2"]][[i]])
  Spikes_N2=c(Spikes_N2,spikes)
}

dvec=1:200
Tmax=13
dhat=AIC(Spikes_N1,dvec,13)$dhat

plot((0:dhat)*Tmax/dhat,c(MLE_pcw(Spikes_N1,dhat,Tmax)$coeff,0),type='s',col='green',main='On citronnellal data',xlab='time', ylab='intensity') ### NB option type='s' make it as a step funcion, just need to be careful with beginning and end

dhat=AIC(Spikes_N2,dvec,13)$dhat

lines((0:dhat)*Tmax/dhat,c(MLE_pcw(Spikes_N2,dhat,Tmax)$coeff,0),type='s',col='red',main='On citronnellal data',xlab='time', ylab='intensity') ### NB option type='s' make it as a step funcion, just need to be careful with beginning and end

legend('topright',c('Neuron 1','Neuron 2'),col=c('green','red'),lty=c(1,1))


#### Exercise 2

MyThinning_Hawkes=function(nu,Tmax)
{
  mypoints=c()
  # intialisation
  M=nu
  myT=rexp(1,rate=M)
  mypoints=c(mypoints,myT) # I know it is accepted always, because nu is the spontaneous rate when no other points have appeared
  
  while(myT<Tmax)
  {
    M=nu+length(which((mypoints<=myT)&(mypoints>=myT-1))) #I count the points in a window of length 1 before myT and compute the new upper bound
    myT=myT+rexp(1,rate=M)
    myU= runif(1,max=M,min=0) 
    influencing_points=mypoints[which(mypoints>=myT-1)] # NB in mypoints myT is not there yet so I have only points strictly before myT
    if(myU<(nu+sum(1-(myT-influencing_points)^2)))
    {
      mypoints=c(mypoints,myT)   
    }   
  }
  # we finish further than Tmax so we truncate
  mypoints=mypoints[which(mypoints<Tmax)]
  return(mypoints)
}

res=MyThinning_Hawkes(2,10)

plot(res,rep(1,length(res)),yaxt='n',xlab='time',ylab='',main='Hawkes simulation')

#### question c/

Lambda=function(t,nu,mypoints)
{
  Nt_1=length(which(mypoints<=t-1))
  currentpoints=mypoints[which((mypoints>t-1)&(mypoints<t))]
  weirdsum=sum((t-currentpoints)-(t-currentpoints)^3/3)
  return(nu*t+2/3*Nt_1+weirdsum)
}
Tmax=10
nu=2
res=MyThinning_Hawkes(nu,Tmax)

x=seq(0,Tmax,0.001)
lx=length(x)
L=rep(0,lx)
for(i in 1:lx)
{
  L[i]=Lambda(x[i],nu,res)
}


plot(x,L,type='l',ylab='count',xlab='time',main='Hawkes cumulative intensity Lambda')
lines(res,1:length(res),type='s',col='red')
rug(res)
legend('topleft',c('counting process','Lambda'),col=c('red','black'),lty=c(1,1))

### question d/
Tmax=100
nu=2
res=MyThinning_Hawkes(nu,Tmax)

nb=length(res)
Nnew=rep(0,nb)
for(i in 1:nb)
{
  Nnew[i]=Lambda(res[i],nu,res)
}

#### question e/

### test of uniformity

ks.test(Nnew,'punif',0,Lambda(Tmax,nu,res))
## large p-value ok

### test of exponentiality on the delays (given by command diff)
ks.test(diff(Nnew),'pexp',1)
## large pvalue ok

### no correlation between delays

acf(diff(Nnew))

# you should see that correlation of 1 of the delays with itself, of course, very small correlation  between one delays and the delays n steps ago
# n is called the lag
# in blue on the plot the band are the critical values at 5% for the test "correlation is null" : if on is inside we are not correlated
# so here no significant correlations if we are inside the band

# Nb we should correct for the numbers of tests... so the right command with Bonferroni correction  would be
acf(diff(Nnew),ci=1-0.05/29)
## 29 tests because 2 ks and 27 lags per default


### just to see that it does not work on the non transform data

ks.test(res,'punif',0,Tmax)
ks.test(diff(res),'pexp',1) 
acf(diff(res),ci=1-0.05/29)

# at least on my simulation, all three test fails on the non transformed   data!

