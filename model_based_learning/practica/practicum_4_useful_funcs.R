log_gaussian_density <- function (x, mean, sigma) {
  distval <- mahalanobis(x, center = mean, cov = sigma)
  logdet <- determinant(sigma,logarithm = TRUE)$modulus
  logretval <- -(ncol(x) * log(2 * pi) + logdet + distval)/2
  return(logretval)
}

logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

normalise <- function (x) {
  logratio = log(x) - logsumexp(log(x))
  exp(logratio)
}

