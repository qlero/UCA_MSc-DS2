em1d <- function(X,K,max_it=50){
  n = length(X)
  
  # Initialization of \theta^0
  mu = rnorm(K,mean(x),sd=1)
  prop = rep(1/K,K)
  sigma = rep(1,K)
  gamma = matrix(NA,n,K)
  
  for (i in 1:max_it){
    # E step
    for (k in 1:K){
      gamma[,k] = prop[k] * dnorm(X,mu[k],sigma[k])
    }
    ## normalize each line of gamma
    
    # M step
    for (k in 1:K){
      nk = sum(gamma[,k])
      prop[k] = nk / n
      mu[k] = sum(gamma[,k]*X) / nk
      sigma[k] = sum(gamma[,k]*(X-mu[k])^2) / nk
    }
    
    # plot of the data and the means
    
    # Evaluate the log-likelihood
    
  }
  
  # Compute BIC
  
  # Return the results
  list(gamma,prop,mu,sigma)
}
