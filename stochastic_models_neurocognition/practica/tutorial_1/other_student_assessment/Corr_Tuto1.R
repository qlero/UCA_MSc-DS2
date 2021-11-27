rm(list=ls())

### question 1b)

# choice of the true parameters of the model
theta_true=30
eta_true=0.01
n=10

# generation of the n iid ISI 

Y=rexp(n,theta_true)
X=Y+eta_true

# for fixed theta (not necessaruly the true one), the likelihood

theta=20
eta=seq(0,0.02,0.00001)
L=theta^n*exp(-theta*n*(mean(X)-eta))*(min(X)>=eta)

plot(eta,L,type='l',main='Likelihood as a function of eta')
rug(X)

# we see that the maximum is achieved for min(X)

### question 1d)

theta_true=30 # make the values vary
eta_true=0.01
nmax=1000

# generation of the n iid ISI 

Y=rexp(nmax,theta_true)
X=Y+eta_true

theta_hat=rep(0,nmax)
eta_hat=rep(0,nmax)

for(n in 1:nmax)
{
  eta_hat[n]=min(X[1:n])
  theta_hat[n]=1/(mean(X[1:n])-min(X[1:n]))
}

par(mfrow=c(1,2))

plot(1:nmax,eta_hat,type='l',main='Estimation of eta',xlab='n',ylab='eta',ylim=c(0,2*eta_true))
lines(c(1,nmax),c(eta_true,eta_true),col='red')
legend('topright',c('estimate','truth'),lwd=c(1,1),col=c('black','red'))


plot(1:nmax,theta_hat,type='l',main='Estimation of theta',xlab='n',ylab='theta',ylim=c(0,2*theta_true))
lines(c(1,nmax),c(theta_true,theta_true),col='red')
legend('topright',c('estimate','truth'),lwd=c(1,1),col=c('black','red'))

### question 1 e) see https://www.rdocumentation.org/packages/STAR/versions/0.3-7/topics/cockroachAlData
install.packages("STAR") # via tools on top of R studio  and CRAN repository
library(STAR)

### on citronellal  each trial is 13 s long. the odor puff is between 6.14 and  6.64
### there are 15 trials

### let us start with the pre-puff phase

data(e070528citronellal)
plot(e070528citronellal[["neuron 1"]])

delays_pre=c()

for(i in 1:15)
{
  spike=as.vector(e070528citronellal[["neuron 1"]][[i]])
  index=which(spike<6.14)
  delays_pre=c(delays_pre,diff(spike[index]))
}

eta_hat_pre=min(delays_pre)
theta_hat_pre=1/(mean(delays_pre)-min(delays_pre))
print(c(eta_hat_pre,theta_hat_pre))

delays_puff=c()

for(i in 1:15)
{
  spike=as.vector(e070528citronellal[["neuron 1"]][[i]])
  index=which((spike>6.14)&(spike<6.64))
  delays_puff=c(delays_puff,diff(spike[index]))
}

eta_hat_puff=min(delays_puff)
theta_hat_puff=1/(mean(delays_puff)-min(delays_puff))
print(c(eta_hat_puff,theta_hat_puff))

 
delays_post=c()

for(i in 1:15)
{
  spike=as.vector(e070528citronellal[["neuron 1"]][[i]])
  index=which((spike>6.64))
  delays_post=c(delays_post,diff(spike[index]))
}

eta_hat_post=min(delays_post)
theta_hat_post=1/(mean(delays_post)-min(delays_post))
print(c(eta_hat_post,theta_hat_post))

### question 1 f)

par(mfrow=c(1,3))


plot(density(delays_pre),xlim=c(0,1.5),main='Pre-odor puff for neuron 1')
x=seq(0,1.5,0.0001)
lines(x,theta_hat_pre*exp(-theta_hat_pre*(x-eta_hat_pre))*(x>=eta_hat_pre),col='red')
legend('topright',c('Kernel density est.','Density corresponding to MLE'),lty=c(1,1),col=c('black','red'))

plot(density(delays_puff),xlim=c(0,0.2),main='During odor puff for neuron 1')
x=seq(0,0.2,0.0001)
lines(x,theta_hat_puff*exp(-theta_hat_puff*(x-eta_hat_puff))*(x>=eta_hat_puff),col='red')
legend('topright',c('Kernel density est.','Density corresponding to MLE'),lty=c(1,1),col=c('black','red'))

plot(density(delays_post),xlim=c(0,1.5),main='Post-odor puff for neuron 1')
x=seq(0,1.5,0.0001)
lines(x,theta_hat_post*exp(-theta_hat_post*(x-eta_hat_post))*(x>=eta_hat_post),col='red')
legend('topright',c('Kernel density est.','Density corresponding to MLE'),lty=c(1,1),col=c('black','red'))


X=rexp(length(delays_puff),74)
min(X)
X=rexp(length(delays_post),7)
min(X)


### Question 2e)

### parameters of the model
n1=20
n2=20


sigma= 5

# I take at random the variability of the cells, I manage that the lowest firing rate is a-b>0
a1=runif(n1,min=16,max=20) # first pop
b1=runif(n1,min=10, max=14)
a2=runif(n2,min=16,max=20) # second pop
b2=runif(n2,min=16,max=20)

### direction of movement (must be of norm 1)

theta=7*pi/4

m1=cos(theta)
m2=sin(theta)

# firing rates

Y1= a1+b1*m1+sigma*rnorm(n1)
Y2=a2+b2*m2+sigma*rnorm(n2)

# estimation m1/m2

hat_m1= sum((Y1-a1)*b1)/sum(b1^2)
hat_m2=sum((Y2-a2)*b2)/sum(b2^2)

# in theta
grille=seq(0,2*pi,0.001)
lg=length(grille)
LS=rep(0,lg)
for(i in 1:lg)
{
  LS[i]=sum((Y1-a1-b1*cos(grille[i]))^2)+sum((Y2-a2-b2*sin(grille[i]))^2)
}
theta_hat=grille[which.min(LS)]


x=seq(0,2*pi,0.001)
plot(sin(x),cos(x),main='visualization',xlab='',ylab='',type='l')
arrows(0,0,m1,m2)
arrows(0,0,hat_m1,hat_m2,col='red')
arrows(0,0,cos(theta_hat),sin(theta_hat),col='green')
legend('topright',c('true direction','estimated direction m1/m2','estimated direction theta'),lty=c(1,1,1),col=c('black','red','green'))




