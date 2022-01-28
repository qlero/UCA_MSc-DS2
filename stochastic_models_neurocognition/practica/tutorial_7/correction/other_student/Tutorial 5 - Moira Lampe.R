#Point Processes

#######################################################################################################################################################

#1: Maximum Likelihood for Markov Chain

#three states: "sleeping", "awake", "drowsy"

#let's say that:

sl = 0 #sleeping
dr = 1 #drowsy 
aw = 2 #awake

#a) Plot the diagrams and parametrize the two following (informal) models with the least possible amount of 
#parameters

#First case

Y = rbinom(50,1, 0.3)
print(Y)
Z = rbinom(50,1, 0.7)
print(Z)
W = rbinom(50,1, 0.4)
print(W)
U = rbinom(50,1, 0.5)
print(U)
X <- c()
for (i in 2:50){
  X[1] = 0 #initializing state
  if (X[i-1] == 0){
    X[i] <- X[i-1]*(1 - Z[i])+(1 - X[i-1])*Y[i]
  }
  else if (X[i-1] == 1){ #we are in state drowsy 
    #if we are in state drowsy, we can go back to sleep or awake, so we can go to 0 or to 1, and we can stay in 1
    
    X[i] <-X[i-1]*(1 + W[i]) - X[i - 1]*Y[i] 
  }
  else if (X[i-1] == 2){
    X[i] <- X[i-1]*(1 - U[i]) #if U is 0, we stay in 2, if U is 1, we go back to 1
  }
}
plot(1:50, X, type = 'l', main = 'First Markov Chain',col = 'violet') 


#Second case

Y = rbinom(50,1, 0.3)
print(Y)
Z = rbinom(50,1, 0.6)
print(Z)
W = rbinom(50,1, 0.2)
print(W)
U = rbinom(50,1, 0.8)
print(U)
P = rbinom(50,1, 0.2)
print(P)
G = rbinom(50,1, 0.5)
print(G)
X <- c()
for (i in 2:50){
  X[1] = 0 #initializing state
  if (X[i-1] == 0){
    X[i] <- Y[i] + P[i]
  }
  else if (X[i-1] == 1){ #we are in state drowsy 
    #if we are in state drowsy, we can go back to sleep or awake, so we can go to 0 or to 1, and we can stay in 1
    X[i] <- W[i] + Z[i]
  }
  else if (X[i-1] == 2){
    X[i] <- U[i] + G[i] #if U is 0, we stay in 2, if U is 1, we go back to 1
  }
}

plot(1:50, X, type = 'l', main = 'Second Markov Chain',col = 'blue') 
plot(1:50, X, main = 'First Markov Chain',col = 'red') 


#######################################################################################################################################################

#b) Make a function in R which simulates the first model if you give in the entries the state x_0, 
#the parameters of the model and the length of the chain to simulate, n. 

#First case

MarkovChain_1 = function(p_01, p_10, p_12, p_21, n, X_0){
  Y = rbinom(n,1, p_01)
  print(Y)
  Z = rbinom(n,1, p_10)
  print(Z)
  W = rbinom(n,1, p_12)
  print(W)
  U = rbinom(n,1, p_21)
  print(U)
  X <- c()
  for (i in 2:n){
  X[1] = X_0 #initializing state
      if (X[i-1] == 0){
        X[i] <- Y[i]
      }
      else if (X[i-1] == 1){ #we are in state drowsy 
        #if we are in state drowsy, we can go back to sleep or awake, so we can go to 0 or to 1, and we can stay in 1
        X[i] <- Z[i] + W[i]
      }
      else if (X[i-1] == 2){
        X[i] <- U[i] + 1 #if U is 0, we stay in 2, if U is 1, we go back to 1
      }
    }
  return(X)
  }

MarkovChain_1(0.5, 0.5, 0.5, 0.5, 10, 0)
print(X)

#Second Case

MarkovChain_2 = function(p_01, p_10, p_12, p_21, p_02, p_20, n, X_0){
  Y = rbinom(n,1, p_01)
  print(Y)
  Z = rbinom(n,1, p_10)
  print(Z)
  W = rbinom(n,1, p_12)
  print(W)
  U = rbinom(n,1, p_21)
  print(U)
  P = rbinom(n,1, p_02)
  print(P)
  G = rbinom(n,1, p_20)
  print(G)
  X <- c()
  for (i in 2:n){
    X[1] = X_0 #initializing state
    if (X[i-1] == 0){
      X[i] <- Y[i] + P[i]
    }
    else if (X[i-1] == 1){ #we are in state drowsy 
      #if we are in state drowsy, we can go back to sleep or awake, so we can go to 0 or to 1, and we can stay in 1
      X[i] <- W[i] + Z[i]
    }
    else if (X[i-1] == 2){
      X[i] <- U[i] + G[i] #if U is 0, we stay in 2, if U is 1, we go back to 1
    }
  }
  return(X)
}

MarkovChain_2(0.2, 0.9, 0.2, 0.1, 0.1, 0.8, 30, 0)

#to be honest I am not 100% if it is mathematically correct from a probabilities point of view but it works.

#######################################################################################################################################################

#d) show on simulation that this estimator converges towards the true probability when n tends to infinity

S = c(0, 0, 0, 1, 1, 0, 1, 2, 1, 2) #just to test
print(S)

n = 10

one_to_zero <- c()
ones <- c()
for (k in 1:n){
  if (X[k] == 1){
    ones = c(ones, +1)
    print(ones)
  }
}

for (j in 1:n){
  if (X[j-1] == 0 && X[j] == 1){
    one_to_zero = c(one_to_zero, +1)
    print(one_to_zero)
  }
}
  


N_10 = length(one_to_zero)
print(N_10)
N_ones = length(ones)
print(N_ones)

p_10_hat = N_10/N_ones

print(p_10_hat)

#now let's make a function out of this and try to simulate it 

amount_drowsy = function(X, n){
  ones <- c()
  for (k in 1:n){
    if (X[k] == 1){
      ones = c(ones, +1)
      print(ones)
    }
  }
return(n_ones = length(ones))
  }


drowsy_to_sleep = function(X, n){
  one_to_zero <- c()
  for (j in 1:n){
    if (X[j-1] == 0 && X[j] == 1){
      one_to_zero = c(one_to_zero, +1)
      print(one_to_zero)
    }
  }
  return(n_10 = length(one_to_zero))
}

p_10_hat = function(n_10, n_ones){
  return(n_10/n_ones)
}

#let's start the simulation with large n:

#First Case:

X = MarkovChain_1(0.5, 0.5, 0.5, 0.5, 1000, 0)
print(X)
test_len = length(X)
print(test_len)
n_ones = amount_drowsy(X, 1000)
n_10 = drowsy_to_sleep(X, 1000)
print(n_10)
print(n_ones)
p_10_hat = n_10/ n_ones
print(p_10_hat)

#Second Case:


#######################################################################################################################################################

#2) Maximum Likelihood for Poisson Process

#a) When d = 1 write the likelihood of the process and provide the maximum likelihood estimator of a11
#Implement it and show on simulation that when Tmax grows, the estimation is converging when the process 
#is homogeneous.

#function for homogeneous poisson process

Homo_Poisson = function(Tmax, lambda)
{
  t = 0 #t starts out with zero
  N = numeric()
  res = c()
  while(t < Tmax){ #we want the process to go on until the cumulative sum of t reaches T, which is 1 here
    u = runif(1) #as long as the above is true, we create u, which is a uniform random variable
    t = t - log(u)/lambda #then our new t will be this #it is the same thing as doing it separately and then taking the cumulative sum of it. 
    N = c(N, t) 
  }
  res = c(N,"The a_hat is:",sum(N/Tmax), "the sum of N is:", sum(N))
  return(res)
}

Homo_Poisson(10, lambda = 3)

#this is not working, it is definitely not converging when Tmax grows...

#Let's try a different way of simulating a poisson process:
#Let's try with exponentially distributed interarrival times.
#We simulate the arrival times until the maximum time horizon is achieved.

My_Poisson = function(Tmax, rate){
  Y = c()
  arrivals <- rexp(50, rate)
  while (arrivals[length(arrivals)] < Tmax){
  Y = c(Y, arrivals)
  }
  return(Y)
}

My_Poisson(1, 0.4)
print(Y)

#I thought I could simulate it differently but somehow this one also doesn't work

#######################################################################################################################################################

#c) Simulate by thinning a Poisson Process of intensity () on [0, Tmax] for various n. 
#Superpose different MLE of the intensities for various choices of d&n. 

#first intensity function

intensity_1 = function(x, n){
  return(n*exp(-x))
}

#second intensity function

intensity_2 = function(x, n){
  return(n*(1 + sin(x)))
}

intesity_1_plus = 0
  
intensity_2_plus = 20

Non_Homo_Poisson = function()
{
  t = 0 #t starts out with zero
  T = 1 #t ends with 1
  X = numeric()
  while(t < T){ #we want the process to go on until the cumulative sum of t reaches T, which is 1 here
    u = runif(1) #as long as the above is true, we create u, which is a uniform random variable
    t = t - log(u)/intensity_1_plus #then our new t will be this #it is the same thing as doing it separately and then taking the cumulative sum of it. 
    d = runif(1)
    if (d < h(t)/intensity_1_plus){ # compare our uniform variable to the probability
      X = c(X, t) #if the above is the case, we add this to our X 
    }
  }
  return(X)
}


#######################################################################################################################################################

