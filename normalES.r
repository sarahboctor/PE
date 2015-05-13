# normalES.r
normalES <- function(mu, sigma, tail.prob = 0.01, invert=FALSE) {
## compute normal ES for collection of assets given mean and sd vector
## inputs:
## mu       n x 1 vector of expected returns
## sigma    n x 1 vector of standard deviations
## tail.prob  scalar tail probability
## invert   logical. If TRUE, return ES as positive number
## output:
## ES      n x 1 vector of left tail average returns reported as a positive number
  mu = as.matrix(mu)
  sigma = as.matrix(sigma)
  if ( nrow(mu) != nrow(sigma) )
    stop("mu and sigma must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  ES = mu - sigma*dnorm(qnorm(tail.prob))/tail.prob
  if(invert) {
   ES = -ES
  }
  return(ES)
}
