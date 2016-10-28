rm(list = ls())
source("workhorse.R")

y <- c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
myModel <- model(theta_prior = NormalDistribution(1.9, 0.95**2),
                 sigma2_prior = InverseGammaDistribution(0.5, 0.01 / 2))

# Storage
res_theta <- rep(NA, 1000)
res_sigma2 <- rep(NA, 1000)

# Run
set.seed(1)
for (iter in 1:1000) {
  if (iter == 1) myState <- state(mean(y), var(y)) # Initialize 
  else myModel$gibbs_step(myState, y)
  
  res_theta[iter] <- myState$get("theta")
  res_sigma2[iter] <- myState$get("sigma2")
}

quantile(res_theta, c(0.025, 0.5, 0.975)) # 1.707 1.804 1.901 = GOOD
