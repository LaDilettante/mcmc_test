model_bugged <- function(theta_prior, sigma2_prior) {
  theta_prior = theta_prior
  sigma2_prior = sigma2_prior
  
  get <- function(param = c("theta_prior", "sigma2_prior")) {
    param <- match.arg(param)
    switch(param,
           theta_prior = theta_prior,
           sigma2_prior = sigma2_prior)
  }
  
  cond_theta <- function(state, y) {
    n <- length(y) ; ybar <- mean(y)
    tau2_n <- 1.1 / (1.1 / theta_prior$get("var") + n / state$get("sigma2"))
    mu_n <- (theta_prior$get("mean") / theta_prior$get("var") + n * ybar / state$get("sigma2")) / 
      (1.1 / theta_prior$get("var") + n / state$get("sigma2"))
    
    return(NormalDistribution(mu_n, tau2_n))
  }
  
  cond_sigma2 <- function(state, y) {
    n <- length(y) ; ybar <- mean(y)
    shape <- sigma2_prior$get("shape") + n / 2
    rate <- sigma2_prior$get("rate") + sum((y - state$get("theta")) ** 2) / 2
    
    return(InverseGammaDistribution(shape, rate))
  }
  
  gibbs_step <- function(state, y) {
    new_theta <- cond_theta(state, y)$sample()
    state$set_theta(new_theta)
    
    new_sigma2 <- cond_sigma2(state, y)$sample()
    state$set_sigma2(new_sigma2)
  }
  
  joint_log_p <- function(state, y) { # p(y|theta, sigma2) * p(theta) * p(sigma2)
    sum(NormalDistribution(state$get("theta"), state$get("sigma2"))$log_p(y)) +
      sigma2_prior$log_p(state$get("sigma")) +
      theta_prior$log_p(state$get("theta"))
  }
  
  forward_sample <- function(ndata) {
    theta <- theta_prior$sample()
    sigma2 <- sigma2_prior$sample()
    y <- NormalDistribution(theta, sigma2)$sample(ndata)
    
    list(state = state(theta, sigma2),
         y = y)
  }
  
  return(list(get = get, 
              cond_theta = cond_theta,
              cond_sigma2 = cond_sigma2,
              joint_log_p = joint_log_p,
              forward_sample = forward_sample,
              gibbs_step = gibbs_step))
}