NormalDistribution <- function(mean, var) {
  mean = mean
  var = var
  
  get <- function(param = c("mean", "var")) {
    param <- match.arg(param)
    switch(param,
           mean = mean, var = var)
  }
  
  sample <- function(n = 1) rnorm(n, mean = mean, sd = sqrt(var))
  log_p <- function(x) dnorm(x, mean = mean, sd = sqrt(var), log = TRUE)
  
  return(list(sample = sample, log_p = log_p, get = get))
}

InverseGammaDistribution <- function(shape, rate) {
  shape = shape
  rate = rate
  
  get <- function(param = c("shape", "rate")) {
    param <- match.arg(param)
    switch(param,
           shape = shape, rate = rate)
  }
  
  sample <- function() 1 / rgamma(1, shape, rate)
  log_p <- function(x) {
    log(rate ** shape) - log(gamma(shape)) + 
      (-shape-1) * log(x) - (rate / x)
  }
  
  return(list(sample = sample, log_p = log_p, get = get))
}