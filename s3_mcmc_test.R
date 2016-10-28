model <- function(mu_0, tau2_0, sigma2_0, nu_0) {
  structure(list(mu_0 = mu_0,
                 tau2_0 = tau2_0,
                 sigma2_0 = sigma2_0,
                 nu_0 = nu_0),
            class = "model")
}

state <- function(theta, sigma2) {
  structure(list(theta = theta, sigma2 = sigma2), class = "state")
}

cond_theta <- function(x, ...) {
  UseMethod("cond_theta", x)
}

cond_theta.model <- function(model, state, y) {
  ybar <- mean(y) ; n <- length(y)
  
  tau2_n <- 1 / (1 / model$tau2_0 + n / state$sigma2)
  mu_n <- (model$mu_0 / model$tau2_0 + n * ybar / state$sigma2) / 
    (1 / model$tau2_0 + n / state$sigma2)
  rnorm(1, mu_n, sqrt(tau2_n))
}

cond_sigma <- function(x, ...) {
  UseMethod("cond_sigma", x)
}

cond_sigma.model <- function(model, state, y) {
  n <- length(y)
  nu_n <- model$nu_0 + n
  nu_sigma2_n <- model$nu_0 * model$sigma2_0 + sum((y - state$theta) ** 2)
  1 / rgamma(1, nu_n / 2, nu_sigma2_n / 2)
}

gibbs <- function(x, ...) {
  UseMethod("gibbs", x)
}

gibbs.model <- function(model, state, y) {
  # Sample statistics
  n <- length(y) ; ybar <- mean(y)
  my_state$theta <- cond_theta(model, state, y)
  my_state$sigma2 <- cond_sigma(model, state, y)
}

y <- c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
my_model <- model(mu_0 = 1.9, tau2_0 = 0.95 ** 2,
                  sigma2_0 = 0.01, nu_0 = 1)
my_state <- state(theta = mean(y), sigma2 = var(y))

set.seed(1)

S <- 5
res_theta <- vector("numeric", S)
res_sigma2 <- vector("numeric", S)
for (s in 1:S) {
  gibbs(my_model, my_state, y)
  print(my_state)
  res_theta[s] <- my_state$theta
  res_sigma2[s] <- my_state$sigma2
}

quantile(res_theta, c(0.025, 0.5, 0.975))
