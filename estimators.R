# functions to fit nuisance estimators

# quantile regression
fit_y_given_X_median_xgboost <- function(data) {
  Y <- data$Y
  A <- data$A
  X <- data$X
  
  predictors <- cbind(A, X)
  labels <- Y
  
  colnames(predictors) <- c("A", paste0("X", seq_len(ncol(X))))
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(predictors), label = labels)
  
  # use default xgboost parameters
  params <- list(
    objective = "reg:absoluteerror"
  )
  
  # train xgboost
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50
  )
  
  # make function that inputs fresh data and outputs predictions
  # Define the prediction function
  predict_quantile <- function(new_data) {
    # Check if new_data is a data frame or matrix
    if (!is.data.frame(new_data) && !is.matrix(new_data)) {
      stop("new_data must be a data frame or matrix.")
    }
    
    # Ensure the new data has the same number of features
    # expected_features <- model$feature_names
    # if (!all(expected_features %in% colnames(new_data))) {
    #  stop("new_data must contain the same features as the training data.")
    #}
    
    # Reorder columns to match the training data
    #new_data <- new_data[, expected_features]
    
    # Convert new data to matrix if necessary
    if (is.data.frame(new_data)) {
      new_data <- as.matrix(new_data)
    }
    
    # Make predictions
    preds <- predict(model, new_data)
    
    return(preds)
  }
  
  return(predict_quantile)
}


# regression
fit_y_given_X_mean_xgboost <- function(data) {
  Y <- data$Y
  A <- data$A
  X <- data$X
  
  predictors <- cbind(A, X)
  labels <- Y
  
  colnames(predictors) <- c("A", paste0("X", seq_len(ncol(X))))
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(predictors), label = labels)
  
  # use default xgboost parameters
  params <- list(
    objective = "reg:squarederror"
  )
  
  # train xgboost
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50
  )
  
  # make function that inputs fresh data and outputs predictions
  # Define the prediction function
  predict_mean <- function(new_data) {
    # Check if new_data is a data frame or matrix
    if (!is.data.frame(new_data) && !is.matrix(new_data)) {
      stop("new_data must be a data frame or matrix.")
    }
    
    # Ensure the new data has the same number of features
    # expected_features <- model$feature_names
    # if (!all(expected_features %in% colnames(new_data))) {
    #  stop("new_data must contain the same features as the training data.")
    #}
    
    # Reorder columns to match the training data
    #new_data <- new_data[, expected_features]
    
    # Convert new data to matrix if necessary
    if (is.data.frame(new_data)) {
      new_data <- as.matrix(new_data)
    }
    
    # Make predictions
    preds <- predict(model, new_data)
    
    return(preds)
  }
  
  return(predict_mean)
}

# regression
fit_y_given_X_mean_xgboost_AX <- function(data) {
  A <- data$A
  X <- data$X
  
  predictors <- X
  labels <- A
  
  colnames(predictors) <- c(paste0("X", seq_len(ncol(X))))
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(predictors), label = labels)
  
  # use default xgboost parameters
  params <- list(
    objective = "reg:squarederror"
  )
  
  # train xgboost
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50
  )
  
  # make function that inputs fresh data and outputs predictions
  # Define the prediction function
  predict_mean <- function(new_data) {
    # Check if new_data is a data frame or matrix
    if (!is.data.frame(new_data) && !is.matrix(new_data)) {
      stop("new_data must be a data frame or matrix.")
    }
    
    # Ensure the new data has the same number of features
    # expected_features <- model$feature_names
    # if (!all(expected_features %in% colnames(new_data))) {
    #  stop("new_data must contain the same features as the training data.")
    #}
    
    # Reorder columns to match the training data
    #new_data <- new_data[, expected_features]
    
    # Convert new data to matrix if necessary
    if (is.data.frame(new_data)) {
      new_data <- as.matrix(new_data)
    }
    
    # Make predictions
    preds <- predict(model, new_data)
    
    return(preds)
  }
  
  return(predict_mean)
}

# regression
fit_y_given_X_mean_xgboost_binary <- function(data) {
  Y <- data$Y
  A <- data$A
  X <- data$X
  
  predictors <- cbind(A, X)
  labels <- Y
  
  colnames(predictors) <- c("A", paste0("X", seq_len(ncol(X))))
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(predictors), label = labels)
  
  # use default xgboost parameters
  params <- list(
    objective = "binary:logistic"
  )
  
  # train xgboost
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50
  )
  
  # make function that inputs fresh data and outputs predictions
  # Define the prediction function
  predict_mean <- function(new_data) {
    # Check if new_data is a data frame or matrix
    if (!is.data.frame(new_data) && !is.matrix(new_data)) {
      stop("new_data must be a data frame or matrix.")
    }
    
    # Ensure the new data has the same number of features
    # expected_features <- model$feature_names
    # if (!all(expected_features %in% colnames(new_data))) {
    #   stop("new_data must contain the same features as the training data.")
    #}
    
    # Reorder columns to match the training data
    # new_data <- new_data[, expected_features]
    
    # Convert new data to matrix if necessary
    if (is.data.frame(new_data)) {
      new_data <- as.matrix(new_data)
    }
    
    # Make predictions
    preds <- predict(model, new_data)
    
    return(preds)
  }
  
  return(predict_mean)
}

# regression
fit_y_given_X_mean_xgboost_fit <- function(data) {
  Y <- data$Y
  A <- data$A
  X <- data$X
  
  predictors <- cbind(A, X)
  labels <- Y
  
  colnames(predictors) <- c("A", paste0("X", seq_len(ncol(X))))
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(predictors), label = labels)
  
  # use default xgboost parameters
  params <- list(
    objective = "reg:squarederror"
  )
  
  # train xgboost
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50
  )
  
  # make function that inputs fresh data and outputs predictions
  # Define the prediction function
  predict_mean <- function(new_data) {
    # Check if new_data is a data frame or matrix
    if (!is.data.frame(new_data) && !is.matrix(new_data)) {
      stop("new_data must be a data frame or matrix.")
    }
    
    # Ensure the new data has the same number of features
    # expected_features <- model$feature_names
    # if (!all(expected_features %in% colnames(new_data))) {
    #  stop("new_data must contain the same features as the training data.")
    #}
    
    # Reorder columns to match the training data
    #new_data <- new_data[, expected_features]
    
    # Convert new data to matrix if necessary
    if (is.data.frame(new_data)) {
      new_data <- as.matrix(new_data)
    }
    
    # Make predictions
    preds <- predict(model, new_data)
    
    return(preds)
  }
  
  return(list(fit = predict_mean))
}

# regression
fit_y_given_X_mean_xgboost_AX_fit <- function(data) {
  A <- data$A
  X <- data$X
  
  predictors <- X
  labels <- A
  
  colnames(predictors) <- c(paste0("X", seq_len(ncol(X))))
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(predictors), label = labels)
  
  # use default xgboost parameters
  params <- list(
    objective = "reg:squarederror"
  )
  
  # train xgboost
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50
  )
  
  # make function that inputs fresh data and outputs predictions
  # Define the prediction function
  predict_mean <- function(new_data) {
    # Check if new_data is a data frame or matrix
    if (!is.data.frame(new_data) && !is.matrix(new_data)) {
      stop("new_data must be a data frame or matrix.")
    }
    
    # Ensure the new data has the same number of features
    # expected_features <- model$feature_names
    # if (!all(expected_features %in% colnames(new_data))) {
    #  stop("new_data must contain the same features as the training data.")
    #}
    
    # Reorder columns to match the training data
    #new_data <- new_data[, expected_features]
    
    # Convert new data to matrix if necessary
    if (is.data.frame(new_data)) {
      new_data <- as.matrix(new_data)
    }
    
    # Make predictions
    preds <- predict(model, new_data)
    
    return(preds)
  }
  
  return(list(fit = predict_mean))
}

# regression
fit_y_given_X_mean_xgboost_binary_fit <- function(data) {
  Y <- data$Y
  A <- data$A
  X <- data$X
  
  predictors <- cbind(A, X)
  labels <- Y
  
  colnames(predictors) <- c("A", paste0("X", seq_len(ncol(X))))
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(predictors), label = labels)
  
  # use default xgboost parameters
  params <- list(
    objective = "binary:logistic"
  )
  
  # train xgboost
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50
  )
  
  # make function that inputs fresh data and outputs predictions
  # Define the prediction function
  predict_mean <- function(new_data) {
    # Check if new_data is a data frame or matrix
    if (!is.data.frame(new_data) && !is.matrix(new_data)) {
      stop("new_data must be a data frame or matrix.")
    }
    
    # Ensure the new data has the same number of features
    # expected_features <- model$feature_names
    # if (!all(expected_features %in% colnames(new_data))) {
    #   stop("new_data must contain the same features as the training data.")
    #}
    
    # Reorder columns to match the training data
    # new_data <- new_data[, expected_features]
    
    # Convert new data to matrix if necessary
    if (is.data.frame(new_data)) {
      new_data <- as.matrix(new_data)
    }
    
    # Make predictions
    preds <- predict(model, new_data)
    
    return(preds)
  }
  
  return(list(fit = predict_mean))
}
# score
# adapted from drape::drape function
fit_score_tree <- function(data, df = 5) {
  A <- data$A
  X <- data$X
  colnames(X) <- c(paste0("X", seq_len(ncol(X))))
  
  # # # Score evaluation
  m_fit <- fit_y_given_X_mean_xgboost_AX(data)
  
  # Variance estimation
  resid_train <- A - m_fit(X)
  data_train <- data.frame("resid_train"=resid_train,
                           "X"=X)
  colnames(data_train)[-1] <- c(paste0("X", seq_len(ncol(X))))
  tree <- partykit::ctree(resid_train^2 ~ .,
                          data=data_train)
  sigma_train <- sqrt(unname(stats::predict(tree,
                                            newdata = data.frame(X))))
  predict_score <- function(new_data) {
    X = new_data[,-1]
    colnames(X) <- c(paste0("X", seq_len(ncol(X))))
    A = new_data[,1]
    
    resid_test <- A - m_fit(X)
    sigma_test <- sqrt(unname(stats::predict(tree,
                                             newdata = data.frame(X))))
    
    # If any variance estimates are close to zero, assume homogeneous (more stable)
    if (any(sigma_test < 0.01)){
      warning(paste0("Minimal heterogeneous variance equals ",
                     round(min(sigma_test),4), ", reducing to homogeneous
                   case for stability."))
      sigma_train <- rep(sqrt(mean(resid_train^2)), nrow(resid_train))
      sigma_test <- rep(sqrt(mean(resid_test^2)), length(A))
    }
    
    # Score estimation
    eps_train <- resid_train / sigma_train
    rho <- drape::spline_score(eps_train, df = df)$rho
    
    eps_test <- resid_test / sigma_test
    rho_test <- rho(eps_test)
    
    # If any score estimates are large, assume Gaussian (more stable)
    if (any(abs(rho_test) > 10)){
      warning(paste0("Maximal absolute spline score prediction equals ",
                     round(max(abs(rho_test)),2), ", reducing to Gaussian
                   case for stability."))
      rho_test <- - eps_test
    }
    
    return(rho_test/sigma_test) # rho_P(x,z)=1/sigma(z) * rho_\varepsilon( (x-m(z))/sigma(z) )
  }
  return(predict_score)
  
}

fit_score_tree_factory <- function(df) {
  placeholder <- function(data) {
    return(fit_score_tree(data = data, df = df))
  }
  return(placeholder)
}

fit_score_basis_poly <- function(data, degree = 6) {
  A <- data$A
  X <- data$X
  
  predictors <- cbind(A, X)
  
  model <- basis_poly_adapted(X = predictors, d = 1, degree = degree)
  
  return(model$rho)
  
}


# adapted function from drape package
basis_poly_adapted <- function (X, d, degree = 2, lambda = NULL) 
{
  if (degree == 1) {
    basis <- X
    d_col <- d
  }
  else {
    basis <- as.matrix(stats::poly(X, degree = degree, raw = TRUE))
    d_col <- which(attributes(basis)$degree == 1)[d]
  }
  basis <- matrix(basis[, -d_col], nrow = nrow(X))
  temp <- drape:::fit_lasso_poly(basis, X[, d], degree = 1, lambda = lambda)
  basis_fit <- temp$fit
  lambda <- temp$lambda
  resid <- as.vector(X[, d] - basis_fit(basis))
  var <- mean(X[, d] * resid)
  rho <- function(X) {
    if (degree == 1) {
      basis <- X
      d_col <- d
    }
    else {
      basis <- as.matrix(stats::poly(X, degree = degree, 
                                     raw = TRUE))
      d_col <- which(attributes(basis)$degree == 1)[d]
    }
    basis <- matrix(basis[, -d_col], nrow = nrow(X))
    resid <- as.vector(X[, d] - basis_fit(basis))
    # var <- mean(X[, d] * resid)
    return(-resid)
  }
  return(list(rho = rho, lambda = lambda))
}