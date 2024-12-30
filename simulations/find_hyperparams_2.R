# code for pre-tuning hyperparameters
library(xgboost)
library(drape)
source("sim_helpers.R")
source("estimators.R")
source("inference.R")
n = 2500
d = 5
delta = c(2,3,4)
eta = 1
eta_AU = 0
dose = c("gaussian", "gamma")
outcome = c("binary", "continuous")


grid <- expand.grid(n = n, d = d, delta = delta, eta = eta, 
                    eta_AU = eta_AU, dose = dose, outcome = outcome)
resmooth_bws <- vector(mode = "list", length = nrow(grid))
spline_dfs <- vector(mode = "list", length = nrow(grid))
iter <- 20
nfolds <- 5

set.seed(123)

for (i in 1:nrow(grid)) {
  print(i)
  resmooth_bws[[i]] <- rep(NA, iter)
  spline_dfs[[i]] <- rep(NA, iter)
  if (grid$outcome[i] == "binary") {
    response_regression = fit_y_given_X_mean_xgboost_binary_fit
  } else if (grid$outcome[i] == "continuous") {
    response_regression = fit_y_given_X_mean_xgboost_fit
  }
  predictor_regression = fit_y_given_X_mean_xgboost_AX
  for (j in 1:iter) {
    print(j)
    foldid <- sample(rep(seq(nfolds), length.out=grid$n[i]))
    sample_data <- generate_data(n = grid$n[i], delta = grid$delta[i],
                                 eta = grid$eta[i], eta_AU = grid$eta_AU[i],
                                 dose = grid$dose[i], outcome = grid$outcome[i])
    X = sample_data$X
    A = sample_data$A
    Y = sample_data$Y
    
    # Train regression procedures
    
    regression_list <- list()
    predictor_list <- list()
    
    for (fold in seq(nfolds)){
      
      te <- (foldid == fold)
      if (nfolds > 1){tr <- !te} else{tr <- te}
      data_train <- list(X = X[tr,], A = A[tr], Y = Y[tr])
      
      regression_list[[fold]] <- response_regression(data_train)
      predictor_list[[fold]] <- predictor_regression(data_train)
      
    }
    
    # bw
    AX <- cbind(A, X)
    colnames(AX) <- c("A", paste0("X", seq_len(ncol(X))))
    cv_resmooth_output <- cv_resmooth(X=AX,
                                      y=Y,
                                      d=1,
                                      regression=regression_list,
                                      tol=2,
                                      prefit=TRUE,
                                      foldid=foldid)
    resmooth_bw <- cv_resmooth_output$bw_opt[1]
    resmooth_bws[[i]][j] <- resmooth_bw
    
    # spline score
    m_fit <- predictor_regression(sample_data)
    
    # Variance estimation
    resid <- A - m_fit(X)
    tree <- partykit::ctree(resid^2 ~ .,
                            data=data.frame("resid"=resid, "X"=X))
    sigma <- sqrt(unname(stats::predict(tree,
                                        newdata = data.frame("X"=X))))
    eps <- resid / sigma
    cv_spline_score_output <- cv_spline_score(x=eps)
    
    spline_df <- cv_spline_score_output$df_1se # Or df_min
    spline_dfs[[i]][j] <- spline_df
    
  }
}


get_mode <- function(x) {
  freq <- table(x)
  max_count <- max(freq)
  mode_candidates <- names(freq)[freq == max_count]
  as.numeric(mode_candidates[1])
}

spline_dfs_best <- pmax(3,unlist(lapply(spline_dfs, get_mode)))

get_mode_rounded <- function(x) {
  x_rounded <- round(x / 0.05) * 0.05
  get_mode(x_rounded)
}

resmooth_bws_best <- unlist(lapply(resmooth_bws, median))

saveRDS(resmooth_bws_best, file = "simulations/resmooth_bws_2.rds")
saveRDS(spline_dfs_best, file = "simulations/spline_dfs_2.rds")
saveRDS(grid, file = "simulations/grid_2.rds")
