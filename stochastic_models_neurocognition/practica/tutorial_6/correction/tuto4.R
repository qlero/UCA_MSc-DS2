### Exercise 1, qustion f)

pval_bin=function(NaT,NT)
{
  return(1-pbinom(NaT-1,prob=0.5,size=NT))
}


library(STAR)

data(e070528citronellal)

Spikes_N1=c()

for(i in 1:15)
{
  spikes=as.vector(e070528citronellal[["neuron 1"]][[i]])
  Spikes_N1=c(Spikes_N1,spikes[which(spikes>6.64)])
}

Spikes_N2=c()

for(i in 1:15)
{
  spikes=as.vector(e070528citronellal[["neuron 2"]][[i]])
  Spikes_N2=c(Spikes_N2,spikes[which(spikes>6.64)])
}

NbT=length(Spikes_N1)
NaT=length(Spikes_N2)

pval_bin(NaT,NaT+NbT)


### Exercise 2
# a/

MyPoisson=function() # nb nothing in entries !
{
  t=0
  Ti=c()
  while(t<1)
  {
    tau=rexp(1,rate=1)
    Ti=c(Ti,t+tau)
    t=t+tau
  }
  Ti=Ti[which(Ti<1)] # I remove the last point
  lt=length(Ti)
  if(lt>0)
    {
      mark=rep(0,lt)
      for(i in 1:lt)
      {
        Ui=runif(1)
        if(Ui<(1-Ti[i]^2))
        {
          mark[i]=1
        }
      }  
      return(Ti[which(mark==1)])
  }else{
      return(c())
    }
}

Ti=MyPoisson()

# b/

Nsimu=1000
res=rep(0,Nsimu)
for(i in 1: Nsimu)
{
  res[i]=length(MyPoisson())
}

mean(res)
barplot(table(res)) # to see the distribution in addition to the mean

# c/



Successive=function(M,Tmax)
{
  ### generating parents
  Ntot=rpois(1,lambda=(Tmax*M))
  Tp=runif(Ntot,max=Tmax,min=0) ### NB this is another way to simulate POisson process. You can also cumulate exponential with parameter M until you're above Tmax
  
  ### generating the children
  Tchil=c()
  np=length(Tp)
  for(i in 1:np)
  {
    Tchil=c(Tchil,(MyPoisson()+Tp[i]))
  }
  Tchil=Tchil[which(Tchil<Tmax)]
  return(list(parents=Tp,children=Tchil))
}

# d/



MyThinning=function(M,Tmax)
{
  Tp=c()
  Tchil=c()
  # initialization : I need upper bound on the sum of the intensity of both processes to generate candidates. Here is the case where this upper bound will change throughout the algorithm
  Max=M
  
  
  myT=rexp(1,rate=Max) # the first potential point
  Tp=c(Tp,myT) # I know it is a parent, and that it cannot be a child at the beginning. So no need to check on the uniform mark I append directly, to the parent.
  
  while(myT<Tmax)
  {
    Max=M+length(which((Tp<=myT)&(Tp>=myT-1))) #I count the parents in a window of length 1 before myT and compute the new upper bound on the sum of both intensities
    myT=myT+rexp(1,rate=Max)
    myU= runif(1,max=Max,min=0)
    if(myU<M) # then this is a parent
    {
      Tp=c(Tp,myT)
    }
    
    # for the children it is more difficult, because we have to sum on all parents that are at distance at most 1 of myT
    
    currentTp=Tp[which((Tp<myT)&(Tp>=myT-1))] # be careful here we want the ones just before myT but not myT
    
    if((myU>=M)&(myU<M+sum(1-(myT-currentTp)^2)))
    {
      Tchil=c(Tchil,myT)   
    }   
  }
  # we finish further than Tmax so we truncate
  Tp=Tp[which(Tp<Tmax)]
  Tchil=Tchil[which(Tchil<Tmax)]
  return(list(parents=Tp,children=Tchil))
  
}

 
res1=Successive(2,10)
res2=MyThinning(2,10)

plot(res1[[1]],rep(1,length(res1[[1]])),col='red',yaxt='n',xlab='time',ylab='n',main='Simulations',ylim=c(0,3))
points(res1[[2]],rep(1,length(res1[[2]])),col='orange')

points(res2[[1]],rep(2,length(res2[[1]])),col='blue')
points(res2[[2]],rep(2,length(res2[[2]])),col='cyan')
legend('topleft',c('Successive-parents','Successive-children','Thinning-parents','Thinning-children'),col=c('red','orange','blue','cyan'),pch=c(1,1,1,1))

