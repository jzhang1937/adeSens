# A function to simulate the data
generate_data <- function(n = 1000, d = 5, gamma = log(2), dose = "gaussian",
                          outcome = "binary", ground = TRUE,
                          n_samp = 1000000, eta = 0.25,
                          delta = 1, eta_AU = 0.25) {
  # draw confounders
  X <- matrix(runif(n = n * d), nrow = n)
  U <- rbinom(n = n, size = 1, prob = pnorm(sin(X[,1] + X[,2])))
  
  # draw coefficients
  theta <- rnorm(d, mean = 0, sd = 1/2)
  beta <- rnorm(d, mean = 0, sd = 1/2)
  
  # draw dose
  if (dose == "gaussian") {
    A <- rnorm(n = n, mean = rowSums(t(t(X) * theta)) + gamma * U, sd = 1)
  } else if (dose == "gamma") {
    A <- rgamma(n = n, shape = 2, rate = 8 + rowSums(t(t(X) * theta)) - gamma * U)
  }
  
  # draw outcome
  if (outcome == "binary") {
    Y <-  rbinom(n = n, size = 1, prob = pnorm(eta * A + rowSums(t(t(X) * beta)) + delta * U +
                                eta_AU * A * U))
  } else if (outcome == "continuous") {
    Y <- rnorm(n = n, mean = eta * A + rowSums(t(t(X) * beta)) + delta * U +
                 eta_AU * A * U, sd = 1)
  }
  
  if (ground) {
    # draw confounders
    X_samp <- matrix(runif(n = n_samp * d), nrow = n_samp)
    U_samp <- rbinom(n = n_samp, size = 1, prob = pnorm(sin(X_samp[,1] + X_samp[,2])))
    # draw dose
    if (dose == "gaussian") {
      A_samp <- rnorm(n = n_samp, mean = rowSums(t(t(X_samp) * theta)) + gamma * U_samp, sd = 1)
    } else if (dose == "gamma") {
      A_samp <- rgamma(n = n_samp, shape = 2, rate = 8 + rowSums(t(t(X_samp) * theta)) - gamma * U_samp)
    }
    
    # draw outcome
    if (outcome == "binary") {
      derivative <- dnorm(eta * A_samp + rowSums(t(t(X_samp) * beta)) + delta * U_samp +
                            eta_AU * A_samp * U_samp) * (eta + eta_AU * U_samp)
    } else if (outcome == "continuous") {
      derivative <- eta + eta_AU * U_samp
    }
    ground_truth <- mean(derivative)
    
    return(data = list(A = A, X = X, U = U, Y = Y, derivative = derivative,
                       ground_truth = ground_truth, A_samp = A_samp,
                       U_samp = U_samp, X_samp = X_samp))
  }
  
  return(data = list(A = A, X = X, U = U, Y = Y, 
                     ground_truth = ground_truth))
}



