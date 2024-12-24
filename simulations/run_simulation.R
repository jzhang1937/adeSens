# Run a simulation study
args <- commandArgs(trailingOnly = TRUE)
# first argument - dose distribution (normal or gamma)
# second argument - outcome type (binary or continuous)
# third argument - delta (coefficient governing strength of association between
# U and outcome)
# fourth argument - n (sample size)
# fifth argument - d (dimension of observed covariates)
# sixth argument - iter (number of iterations, default 1000)

# Parse arguments
dose <- as.character(args[1])
outcome <- as.character(args[2])
delta <- as.numeric(args[3])
n <- as.numeric(args[4])
d <- as.numeric(args[5])
iter <- as.numeric(args[6])
eta <- as.numeric(args[7])
eta_AU <- as.numeric(args[8])

# initialize results list
results <- vector("list", length = iter)

source("sim_helpers.R")
source("estimators.R")
source("inference.R")

library(xgboost)
library(drape)
library(partykit)

# hyperparameters
resmooth_bws <- readRDS("simulations/resmooth_bws.rds")
spline_dfs <- readRDS("simulations/spline_dfs.rds")
param_grid <- readRDS("simulations/grid.rds")

vals <- c(n = n, d = d, delta = delta, eta = eta, eta_AU = eta_AU,
          dose = dose, outcome = outcome)
param_grid <- data.frame(lapply(param_grid, as.character), stringsAsFactors = FALSE)

match <- apply(param_grid, 1, function(row) all(row == vals))

if (sum(match) > 0) {
  print("matches")
  row_number <- which(match)
  resmooth_bw <- resmooth_bws[row_number]
  spline_df <- spline_dfs[row_number]
} else {
  print("no matches")
  resmooth_bw <- NULL
  spline_df <- 3
}

# set seed for reproducibility
set.seed(1234)

# run the simulation
for (i in 1:iter) {
  print(i)
  sim_data <- generate_data(n = n, d = d, dose = dose, outcome = outcome,
                            delta = delta, gamma = log(2))
  
  if (outcome == "continuous") {
    results[[i]] <- continuous_ade(data = sim_data, gamma = seq(0,log(2),log(2)/4), 
                                   conditional_mean = fit_y_given_X_mean_xgboost, 
                                   score = fit_score_tree_factory(spline_df),
                                   resmooth_bw = resmooth_bw,
                                   conditional_median = fit_y_given_X_median_xgboost)
  } else if (outcome == "binary") {
    results[[i]] <- binary_ade(data = sim_data, gamma = seq(0,log(2),log(2)/4), 
                                   conditional_mean = fit_y_given_X_mean_xgboost_binary, 
                                   score = fit_score_tree_factory(spline_df),
                                  resmooth_bw = resmooth_bw)
  }
  results[[i]]$ground_truth <- sim_data$ground_truth
  results[[i]]$ground_truth_rr <- sim_data$ground_truth_rr
}



# save results 
folder_path <- "simulations/results/"   # Replace with your folder path
file_name <- paste0(paste(dose, outcome, delta, n, d, sep = "_"), "_results.rds") # File name for the RDS file

# Create the full file path
file_path <- file.path(folder_path, file_name)
# Check if the folder exists
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# Save the object to the specified folder
saveRDS(results, file = file_path)



