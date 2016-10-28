f_gibbs <- function(y, mu_0, tau2_0, sigma2_0, nu_0, S = 1000) {
  # Sample statistics
  n <- length(y) ; ybar <- mean(y)
  
  # Initialize storage
  res_theta <- rep(NA, S)
  res_sigma2 <- rep(NA, S)
  
  # Starting value as the sample variance and mean
  res_sigma2[1] <- var(y)
  res_theta[1] <- ybar
  
  # Gibbs sampler -- big loop
  for (s in 2:S) {
    # Full conditional for theta
    sigma2 <- res_sigma2[s - 1] # Get the current value of sigma2
    
    tau2_n <- 1 / (1 / tau2_0 + n / sigma2)
    mu_n <- (mu_0 / tau2_0 + n * ybar / sigma2) / (1 / tau2_0 + n / sigma2)
    
    # Generate and save theta. Note that rnorm takes SD, not variance
    res_theta[s] <- rnorm(1, mu_n, sqrt(tau2_n))
    
    # Full conditional for sigma
    theta <- res_theta[s] # Get the current value of theta
    
    nu_n <- nu_0 + n
    nu_sigma2_n <- nu_0 * sigma2_0 + sum((y - theta) ** 2)
    res_sigma2[s] <- 1 / rgamma(1, nu_n / 2, nu_sigma2_n / 2)
  }
  
  return(list(theta = res_theta, sigma2 = res_sigma2))
}

# Data and priors are specified as in Hoff p. 95
y <- c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
set.seed(1)
gibbs <- f_gibbs(y = y, S = 1000,
                 mu_0 = 1.9, tau2_0 = 0.95 ** 2,
                 sigma2_0 = 0.01, nu_0 = 1)
quantile(gibbs$theta, c(0.025, 0.5, 0.975)) # Exactly as in Hoff p. 95