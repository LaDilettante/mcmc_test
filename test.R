rm(list = ls())
source("workhorse.R")
source("workhorse_bugged.R")
library("testthat")

test_that("cond_theta works correctly", {
  
  # myModel <- model(theta_prior = NormalDistribution(1.9, 0.95**2),
  #                  sigma2_prior = InverseGammaDistribution(0.5, 0.01 / 2))
  myModel <- model_bugged(theta_prior = NormalDistribution(1.9, 0.95**2),
                   sigma2_prior = InverseGammaDistribution(0.5, 0.01 / 2))
  
  tmp <- myModel$forward_sample(10)
  myState <- tmp$state
  y <- tmp$y
  
  newState <- state(theta = rnorm(1, 0, sd = 0.5),
                    sigma2 = myState$get("sigma2"))
  
  cond_theta <- myModel$cond_theta(myState, y)
  
  expect_equal(cond_theta$log_p(newState$get("theta")) - cond_theta$log_p(myState$get("theta")),
               myModel$joint_log_p(newState, y) - myModel$joint_log_p(myState, y))
})