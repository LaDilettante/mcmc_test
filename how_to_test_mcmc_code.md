how_to_test_mcmc_code
========================================================
author: Anh Le
width: 1700
height: 768


Appealing?
========================================================

```
myModel <- model(theta_prior = NormalDistribution(1.9, 0.95**2),
                 sigma2_prior = InverseGammaDistribution(0.5, 0.01 / 2))

# Storage
res_theta <- rep(NA, 1000) ; res_sigma2 <- rep(NA, 1000)

# Run
set.seed(1)
for (iter in 1:1000) {
  if (iter == 1) {
  myState <- state(mean(y), var(y)) # Initialize 
  } else {
    myModel$gibbs_step(myState, y)
  }
  
  res_theta[iter] <- myState$get("theta")
  res_sigma2[iter] <- myState$get("sigma2")
}
```

Teaser
========================================================

```
model <- function(theta_prior, sigma2_prior) {
  theta_prior = theta_prior
  sigma2_prior = sigma2_prior
  
  ....
  
  gibbs_step <- function(state, y) {
    new_theta <- cond_theta(state, y)$sample()
    state$set_theta(new_theta)
    
    new_sigma2 <- cond_sigma2(state, y)$sample()
    state$set_sigma2(new_sigma2)
  }
}
```
