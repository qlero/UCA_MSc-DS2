#Q1b

pAD=0.3 # hence 0.7 prob to stay awayke
pDS=0.3
pDA=0.6 # hence 0.1 proba to stay drowsy

pSD=0.3 # hence 0.7 prob to stay sleeping

p=c(pAD,pDS,pDA,pSD)

Sleep_model=function(x0,p,n)
{
  pAD=p[1]
  pDS=p[2]
  pDA=p[3]
  pSD=p[4]
  
  X=rep(0,n)
  before=x0
  for(i in 1:n)
  {
    U=runif(1) # take a uniform (will be used to know to which state I go)
    if(before=='A')
    {
      if(U<pAD)
      {
        X[i]='D'
      }else{
        X[i]='A'
      }
    }  
    if(before=='D')
    {
      if(U<pDS)
      {
        X[i]='S'
      }else{
        if((U>pDS)&(U<(pDS+pDA)))
        {
          X[i]='A'
        }else{
          X[i]='D'
        }  
      }
    }
    if(before=='S')
    {
      if(U<pSD)
      {
        X[i]='D'
      }else{
        X[i]='S'
      }
    } 
    before=X[i]
  }
  return(X)
}

X=Sleep_model('A',p,100)

#1d/

# let us make a function that counts the transitions DS and the states D and therefore computes the MLE


MLE=function(X)
{
  before=X[1]
  n=length(X)
  countDS=0
  countD=0
  for(i in 2:n)
  {
    if(before=='D')
    {
      countD=countD+1
      
      if(X[i]=='S')
      {
        countDS=countDS+1
      }
    }
    before=X[i]
  }
  MLE=countDS/countD
  return(MLE)
}

Nsim=5000
X=Sleep_model('A',p,Nsim)


pDS_hat=rep(0,Nsim-1)
for(j in 2:Nsim)
{
  Xtrunc=X[1:j]
  pDS_hat[j-1]=MLE(Xtrunc)
}

plot(1:length(pDS_hat),pDS_hat,type='l',ylab='MLE',xlab='n',main='Convergence of the estimator')
abline(h=pDS,col='red')
