#Model Selection

#Gaussian Model Selection in the simplified Georgopoulos setting


######################################################################################################################

#Question 1
#create matrix
#make a function

n = 100
p = 50
  
X = matrix(0, nrow = n, ncol = 2*p + 1) #this is an empty matrix
#number of rows is n because every row represents a time point for the same cell
print(X)
  
  
ui = 2*pi*(0:(n-1))/n
print(ui)

for (k in 0:p){
    } for (row in 1:nrow(X)){
        for (column in 1:ncol(X)){
          if (column = 1) #column here is the same as j 
            X[row, column] = 1
         } else if (column = 2*k) #k is the same as p
            X[row, column] = cos(k*ui)
         } else if (column = 2*k + 1)
            X[row, column] = sin(k*ui) 
      

print(X)

#something is wrong with the syntaxing so it is not working; can you help me figure out the mistake?

#I tried a different method which worked and continued with it, but I would still like to know what 
#is wrong with the above code. 

X = matrix(0, nrow = n, ncol = 2*p + 1)
X[,1] = rep(1,n) #fill the first columns with ones
for(k in 1:p){
  X[,2*k] = cos(k*ui)
  X[,2*k + 1] = sin(k*ui)
}

print(X)
  

######################################################################################################################

#Question 2

#By computing the different scalar products, show that the columns of X are orthogonal
# but not norm 1 and renormalize them: this gives you the matrix X'

#scalar product 

I = t(X) %*% X 

#we can see that the matrix is orthogonal if it is equal to the identity matrix

print(I)

#we can see that we get an identity matrix, but we can see that it is not by norm 1
#therefore, we still have to normalize it 

X_prime = matrix(0, nrow = n , ncol = 2*p + 1) #again we create an empty matrix 
for (j in 1:ncol(X)){ #we go per column, because we take each column as a vector 
    X_prime[,j] =  X[,j]/(sqrt(sum(X[,j]^2))) #we take each column as a vector
}


######################################################################################################################

#Question 3

d=seq(1,p-1,1) #to make sure d is smaller than p

proj_estim = function(d, X_prime, Y){ #this is our fourier estimator
  nrow = length(Y)
  four_coeff = 2*d + 1
  estimator = rep(0, nrow) #this is an empty vector with zeros
  for (i in 1:four_coeff){
    estimator = estimator + sum(X_prime[,i]*Y)*X_prime[,i]
  }
    
  return(estimator)
  
} 

######################################################################################################################

#Question 4

ei = rnorm(n)

Y1 = 16 + 14*cos(ui) + 5*ei 

#plot the data
plot(ui, Y1, type='l',main='Fourier decomposition')
lines(ui,16+14*cos(ui),col='pink')
lines(ui, proj_estim(1, X_prime, Y1),col = 'blue')
lines(ui, proj_estim(5, X_prime, Y1),col = 'red')
lines(ui, proj_estim(15,X_prime, Y1),col = 'green')
lines(ui, proj_estim(25,X_prime, Y1),col = 'violet')
legend('bottomleft',c('correct','d = 1','d = 5','d = 15','d = 25'),lty=c(1, 1, 1, 1, 1),col=c('pink','blue','red','green','violet'))

Y2 = 10*exp(-(ui-pi)^2/(0.2)) + 1*ei

#plot the data
plot(ui, Y2, type='l',main='Fourier decomposition')
lines(ui,10*exp(-(ui-pi)^2/(0.2)),col='pink')
lines(ui, proj_estim(1, X_prime, Y2),col = 'blue')
lines(ui, proj_estim(5, X_prime, Y2),col = 'red')
lines(ui, proj_estim(15,X_prime, Y2),col = 'green')
lines(ui, proj_estim(25,X_prime, Y2),col = 'violet')
legend('bottomleft',c('correct','d = 1','d = 5','d = 15','d = 25'),lty=c(1, 1, 1, 1, 1),col=c('pink','blue','red','green','violet'))


######################################################################################################################

#Question 5

LS = function(Y,X,d){ #LS = Least Squares
  return(sum((Y-projection(Y,X,d))^2)) #Sum of least squares
}

Mallows_Cp = function(Y, X_prime, sigma){
  CPs = rep(0,length(d))
  for (i in 1:length(d))
  CPs[i] = LS(Y,X,d[i]) + 2*d[i]*sigma^2

  return(CPs)
}


######################################################################################################################

#Question 6

#I really don't understand what I am supposed to do in this question... 




