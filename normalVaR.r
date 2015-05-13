# normalVaR.r
##
## Normal VaR functions for portfolio VaR report
##

normalVaR <- function(mu, sigma, tail.prob = 0.01, invert=FALSE) {
## compute normal VaR for collection of assets given mean and sd vector
## inputs:
## mu         n x 1 vector of expected returns
## sigma      n x 1 vector of standard deviations
## tail.prob  scalar tail probability
## invert     logical. If TRUE report VaR as positive number
## output:
## VaR        n x 1 vector of left tail return quantiles
## References:
## Jorian (2007) pg 111.
  mu = as.matrix(mu)
  sigma = as.matrix(sigma)
  if ( nrow(mu) != nrow(sigma) )
    stop("mu and sigma must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  VaR = mu + sigma*qnorm(tail.prob)
  if (invert) {
    VaR = -VaR
  }
  return(VaR)
}
