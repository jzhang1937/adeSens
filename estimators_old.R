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