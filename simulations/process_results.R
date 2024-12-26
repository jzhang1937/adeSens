library(dplyr)
# code to process and put the results in tables
param_grid <- readRDS("simulations/grid.rds")
param_grid$eta <- NULL
param_grid$eta_AU <- NULL

# Rearrange columns to the desired order
param_grid <- param_grid %>% select(dose, outcome, delta, n, d)
param_grid <- param_grid %>%
  mutate(delta = ifelse(delta == "2.00", "2", delta))


results <- apply(param_grid, 1, read_results)

# Compute averages for each simulation
summary_results <- lapply(results, function(simulation) {
  # Compute measures for all iterations
  measures_list <- lapply(simulation, compute_coverage)
  
  # Combine into a matrix for averaging (each row is a measure vector)
  measures_matrix <- do.call(rbind, measures_list)
  measures_matrix[is.na(measures_matrix)] <- 0
  
  # Take the average of each column (final vector average)
  colMeans(measures_matrix)
})

# Extract vectors from inner lists
results_list <- lapply(summary_results, function(x) unlist(x))

# Combine vectors row-wise into a matrix
summary_results_df <- as.data.frame(do.call(rbind, results_list))

# code to process and put the results in tables
param_grid_1 <- readRDS("simulations/grid_1.rds")
param_grid_1$eta <- NULL
param_grid_1$eta_AU <- NULL

# Rearrange columns to the desired order
param_grid_1 <- param_grid_1 %>% select(dose, outcome, delta, n, d)

results_1 <- apply(param_grid_1, 1, read_results)


# Compute averages for each simulation
summary_results_1 <- lapply(results_1, function(simulation) {
  # Compute measures for all iterations
  measures_list <- lapply(simulation, compute_coverage)
  
  # Combine into a matrix for averaging (each row is a measure vector)
  measures_matrix <- do.call(rbind, measures_list)
  measures_matrix[is.na(measures_matrix)] <- 0
  
  # Take the average of each column (final vector average)
  colMeans(measures_matrix)
})

# Extract vectors from inner lists
results_list_1 <- lapply(summary_results_1, function(x) unlist(x))

# Combine vectors row-wise into a matrix
summary_results_df_1 <- as.data.frame(do.call(rbind, results_list_1))

summary_results_df <- rbind(summary_results_df, summary_results_df_1)


# Function to construct file name and read the RDS file
read_results <- function(row) {
  # Construct file name using row values
  file_name <- paste0(row, collapse = "_")
  file_path <- paste0("simulations/results/", file_name, "_results.rds")
  # Remove all instances of ".00"
  file_path <- gsub("\\.00", "", file_path)
  
  # Read the .rds file
  if (file.exists(file_path)) {
    readRDS(file_path)
  } else {
    warning(paste("File not found:", file_path))
    NULL
  }
}

# Function to compute measures of interest
compute_coverage <- function(iteration) {
  truth_values <- c(iteration$ground_truth, iteration$ground_truth_rr)
  truth <- mean(truth_values, na.rm = TRUE)
  # Element-wise logical comparisons for coverage
  coverage <- as.numeric((truth >= iteration$lower_point_ci) &
                           (truth <= iteration$upper_point_ci))
  
  uniform_coverage <- as.numeric((truth >= iteration$lower_uniform_ci) &
                                   (truth <= iteration$upper_uniform_ci))
  
  # Combine coverage and uniform_coverage into a single vector
  c(coverage, uniform_coverage)
}

