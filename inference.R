# Functions for estimators
# continuous outcome sensitivity analysis
# data must be a list object with elements X, A, Y
# conditional_mean is a function that inputs training
# data and outputs a function estimate of the conditional mean
#' Title
#'
#' @param data A list object with elements X, A, Y
#' @param gamma A vector of sensitivity values; default gamma = c(0,1)
#' @param conditional_mean A function that inputs training
# data and outputs a function estimate of the conditional mean E[Y | A, X]
#' @param score A function that inputs training
# data and outputs a function estimate of the score s(A | X)
#' @param conditional_median A function that inputs training
# data and outputs a function estimate of the conditional median of Y given A,X.
#' @param conditional_mean_derivative A function that inputs training
# data and outputs a function estimate of the derivative of the conditional mean
# E[Y | A, X].
#' @param K The number of folds for cross-fitting; default K = 5.
#' @param alpha The significance level for the CIs; default alpha = 0.05.
#' @param seed The seed used for cross-fitting and fitting nuisances.
#'
#' @return A list containing point estimates, standard deviation estimates, and
#' pointwise and uniform confidence intervals for the chosen gamma vector.
#' @export
#'
#' @examples
continuous_ade <- function(data, gamma = 1, conditional_mean, score,
                           conditional_median, conditional_mean_derivative = NULL,
                           resmooth_bw = NULL,
                           K = 5, alpha = 0.05, seed = 123) {
  # extract data
  X = data$X
  Y = data$Y
  A = data$A
  
  # length of gamma
  n_gamma <- length(gamma)
  
  # Shuffle and create fold indices
  n <- nrow(data$X)
  
  # Create K roughly equally size folds
  folds <- sample(x = 1:K, size = n, replace = TRUE) |>
    R.utils::withSeed(seed = seed)
  
  # Matrix of product of residuals across folds
  upper <- matrix(0,nrow = n, ncol = n_gamma)
  lower <- matrix(0,nrow = n, ncol = n_gamma)
  ade <- rep(0, n)
  sens_correction <- rep(0,n)
  
  for (k in 1:K) {
    # Segment data by fold using the which() function
    evalIndexes <- which(folds==k,arr.ind=TRUE)
    # evaluation data
    evalDataX <- X[evalIndexes, ]
    evalDataA <- A[evalIndexes]
    evalDataY <- Y[evalIndexes]
    # nuisance training data
    if (K > 1) {
      nuisDataX <- X[-evalIndexes, ]
      nuisDataA <- A[-evalIndexes]
      nuisDataY <- Y[-evalIndexes]
    } else if (K == 1) {
      nuisDataX <- evalDataX
      nuisDataY <- evalDataY
    }
    
    # Concatenate all of the training data
    data_nuis <- list(X = nuisDataX, A = nuisDataA, Y = nuisDataY)
    
    # number of evaluation samples
    nk <- length(evalDataY)
    
    # median fit
    y_given_X_median_hat <- conditional_median(data_nuis) |>
      R.utils::withSeed(seed = seed)
    
    # mean fit
    y_given_X_mean_hat <- conditional_mean(data_nuis) |>
      R.utils::withSeed(seed = seed)
    
    # mean derivative fit
    if (!is.null(conditional_mean_derivative)) {
      y_given_X_mean_derivative_hat <- conditional_mean_derivative(data_nuis) |>
        R.utils::withSeed(seed = seed)
    }
    
    # score fit
    score_hat <- score(data_nuis) |>
      R.utils::withSeed(seed = seed)
    
    # print update
    print(paste0("Fitting Nuisance Complete, Fold: ", k))
    
    # evaluate nuisance functions
    AX <- cbind(evalDataA, evalDataX)
    # colnames(AX) <- c("A", paste0("X.", seq_len(ncol(X))))
    colnames(AX) <- c("A", paste0("X", seq_len(ncol(X))))
    
    s_hat <- score_hat(AX)
    mu_hat <- y_given_X_mean_hat(AX)
    if (is.null(conditional_mean_derivative)) {
      # resmooth
      if (is.null(resmooth_bw)) {
        smoothed <- drape::resmooth(fit = y_given_X_mean_hat, 
                                    X = AX)
      } else {
        smoothed <- drape::resmooth(fit = y_given_X_mean_hat, 
                                    X = AX, bw = resmooth_bw)
      }
      mu_prime_hat <- smoothed$deriv[[1]]
    } else {
      mu_prime_hat <- y_given_X_mean_derivative_hat(AX)
    }
    m_hat <- y_given_X_median_hat(AX)
    
    # IF evaluation
    ade[evalIndexes] <- -s_hat * (evalDataY - mu_hat) + mu_prime_hat
    sens_correction[evalIndexes] <- (evalDataY - m_hat) * 
      ((evalDataY > m_hat) - (evalDataY < m_hat))
    
    for (i in seq_along(gamma)) {
      lower[evalIndexes, i] <- ade[evalIndexes] - 
        gamma[i] * sens_correction[evalIndexes]
      upper[evalIndexes, i] <- ade[evalIndexes] + 
        gamma[i] * sens_correction[evalIndexes]
    }
    
  }
  
  # standard errors
  lower_sd_hat <- apply(X = lower, MARGIN = 2, FUN = stats::sd)
  upper_sd_hat <- apply(X = upper, MARGIN = 2, FUN = stats::sd)
  ade_sd_hat <- stats::sd(ade)
  sens_correction_sd_hat <- stats::sd(sens_correction)
  
  # point estimates
  lower_est <- colMeans(lower)
  upper_est <- colMeans(upper)
  ade_est <- mean(ade)
  sens_correction_est <- mean(sens_correction)
  
  # pointwise CIs
  lower_point_ci <- lower_est - qnorm(1-alpha) * lower_sd_hat / sqrt(n)
  upper_point_ci <- upper_est + qnorm(1-alpha) * upper_sd_hat / sqrt(n)
  
  # uniform CIs
  lower_uniform_ci <- ade_est  - qnorm(1-alpha / 2) * ade_sd_hat / sqrt(n) - gamma *
    (sens_correction_est + qnorm(1-alpha / 2) * sens_correction_sd_hat/ sqrt(n))
  upper_uniform_ci <- ade_est  + qnorm(1-alpha / 2) * ade_sd_hat / sqrt(n) + gamma *
    (sens_correction_est + qnorm(1-alpha / 2) * sens_correction_sd_hat/ sqrt(n))
  
  return(list(lower_est = lower_est, upper_est = upper_est, ade_est = ade_est,
              sens_correction_est = sens_correction_est, lower_sd_hat = 
                lower_sd_hat, upper_sd_hat = upper_sd_hat, ade_sd_hat = 
                ade_sd_hat, sens_correction_sd_hat = sens_correction_sd_hat, 
              lower_point_ci = lower_point_ci, upper_point_ci = upper_point_ci,
              lower_uniform_ci = lower_uniform_ci, upper_uniform_ci = 
                upper_uniform_ci))
  
}

#' Title
#'
#' @param data A list object with elements X, A, Y
#' @param gamma A vector of sensitivity values; default gamma = c(0,1)
#' @param conditional_mean A function that inputs training
# data and outputs a function estimate of the conditional mean E[Y | A, X]
#' @param score A function that inputs training
# data and outputs a function estimate of the score s(A | X)
#' @param conditional_mean_derivative A function that inputs training
# data and outputs a function estimate of the derivative of the conditional mean
# E[Y | A, X].
#' @param K The number of folds for cross-fitting; default K = 5.
#' @param alpha The significance level for the CIs; default alpha = 0.05.
#' @param h The parameter governing the smooth approximation; default h = 0.01.
#' @param seed The seed used for cross-fitting and fitting nuisances.
#'
#' @return A list containing point estimates, standard deviation estimates, and
#' pointwise and uniform confidence intervals for the chosen gamma vector.
#' @export
#'
#' @examples
binary_ade <- function(data, gamma = 1, conditional_mean, score,
                            conditional_mean_derivative = NULL, K = 5, 
                            resmooth_bw = NULL,
                           alpha = 0.05, t = 50, seed = 123) {
  # extract data
  X = data$X
  Y = data$Y
  A = data$A
  
  # length of gamma
  n_gamma <- length(gamma)
  
  # Shuffle and create fold indices
  n <- nrow(data$X)
  
  # Create K roughly equally size folds
  folds <- sample(x = 1:K, size = n, replace = TRUE) |>
    R.utils::withSeed(seed = seed)
  
  # Matrix of product of residuals across folds
  upper <- matrix(0,nrow = n, ncol = n_gamma)
  lower <- matrix(0,nrow = n, ncol = n_gamma)
  ade <- rep(0, n)
  sens_correction <- rep(0,n)
  
  for (k in 1:K) {
    # Segment data by fold using the which() function
    evalIndexes <- which(folds==k,arr.ind=TRUE)
    # evaluation data
    evalDataX <- X[evalIndexes, ]
    evalDataA <- A[evalIndexes]
    evalDataY <- Y[evalIndexes]
    # nuisance training data
    if (K > 1) {
      nuisDataX <- X[-evalIndexes, ]
      nuisDataA <- A[-evalIndexes]
      nuisDataY <- Y[-evalIndexes]
    } else if (K == 1) {
      nuisDataX <- evalDataX
      nuisDataY <- evalDataY
    }
    
    # Concatenate all of the training data
    data_nuis <- list(X = nuisDataX, A = nuisDataA, Y = nuisDataY)
    
    # number of evaluation samples
    nk <- length(evalDataY)
    
    # y given X fit
    y_given_X_mean_hat <- conditional_mean(data_nuis) |>
      R.utils::withSeed(seed = seed)
    
    # mean derivative fit
    if (!is.null(conditional_mean_derivative)) {
      y_given_X_mean_derivative_hat <- conditional_mean_derivative(data_nuis) |>
        R.utils::withSeed(seed = seed)
    }
    
    # score fit
    score_hat <- score(data_nuis) |>
      R.utils::withSeed(seed = seed)
    
    # print update
    print(paste0("Fitting Nuisance Complete, Fold: ", k))
    
    # evaluate nuisance functions
    AX <- cbind(evalDataA, evalDataX)
    colnames(AX) <- c("A", paste0("X", seq_len(ncol(X))))
    
    s_hat <- score_hat(AX)
    mu_hat <- y_given_X_mean_hat(AX)
    
    if (is.null(conditional_mean_derivative)) {
      # resmooth
      if (is.null(resmooth_bw)) {
        smoothed <- drape::resmooth(fit = y_given_X_mean_hat, 
                                    X = AX)
      } else {
        smoothed <- drape::resmooth(fit = y_given_X_mean_hat, 
                                    X = AX, bw = resmooth_bw)
      }
      mu_prime_hat <- smoothed$deriv[[1]]
    } else {
      mu_prime_hat <- y_given_X_mean_derivative_hat(AX)
    }
    
    # IF evaluation
    ade[evalIndexes] <- -s_hat * (evalDataY - mu_hat) + mu_prime_hat
    sens_correction[evalIndexes] <- - 1 / t * log(exp(-t * mu_hat) + exp(-t * (1 - mu_hat))) +
      (exp(-t * mu_hat) - exp(-t * (1 - mu_hat))) / 
      (exp(-t * mu_hat) + exp(-t * (1 - mu_hat))) * (evalDataY - mu_hat) + log(2) / t
    for (i in seq_along(gamma)) {
      lower[evalIndexes, i] <- ade[evalIndexes] - 
        gamma[i] * sens_correction[evalIndexes]
      upper[evalIndexes, i] <- ade[evalIndexes] + 
        gamma[i] * sens_correction[evalIndexes]
    }
    
  }

  # standard errors
  lower_sd_hat <- apply(X = lower, MARGIN = 2, FUN = stats::sd)
  upper_sd_hat <- apply(X = upper, MARGIN = 2, FUN = stats::sd)
  ade_sd_hat <- stats::sd(ade)
  sens_correction_sd_hat <- stats::sd(sens_correction)
  
  # point estimates
  lower_est <- colMeans(lower)
  upper_est <- colMeans(upper)
  ade_est <- mean(ade)
  sens_correction_est <- mean(sens_correction)
  
  # pointwise CIs
  lower_point_ci <- lower_est - qnorm(1-alpha) * lower_sd_hat / sqrt(n) 
  upper_point_ci <- upper_est + qnorm(1-alpha) * upper_sd_hat / sqrt(n) 
  
  # uniform CIs
  lower_uniform_ci <- ade_est  - qnorm(1-alpha / 2) * ade_sd_hat / sqrt(n) - gamma *
    (sens_correction_est + qnorm(1-alpha / 2) * sens_correction_sd_hat/ sqrt(n))
  upper_uniform_ci <- ade_est + qnorm(1-alpha / 2) * ade_sd_hat / sqrt(n) + gamma *
    (sens_correction_est + qnorm(1-alpha / 2) * sens_correction_sd_hat/ sqrt(n))
  
  return(list(lower_est = lower_est, upper_est = upper_est, ade_est = ade_est,
              sens_correction_est = sens_correction_est, lower_sd_hat = 
                lower_sd_hat, upper_sd_hat = upper_sd_hat, ade_sd_hat = 
                ade_sd_hat, sens_correction_sd_hat = sens_correction_sd_hat, 
              lower_point_ci = lower_point_ci, upper_point_ci = upper_point_ci,
              lower_uniform_ci = lower_uniform_ci, upper_uniform_ci = 
                upper_uniform_ci))
  
}