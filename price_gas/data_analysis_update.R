# Set seed for reproducibility
set.seed(90095)

# Load packages
library(tidyverse)
library(xgboost)
library(drape)
library(foreign)
library(ranger)
library(ggrepel)

source("estimators.R")
source("inference.R")

df  <- read.dta("price_gas/gasoline_final_tf1.dta")
names(df)

N=nrow(df)

# take logs of continuous vars
cols.to.log<-c("gas","price","income","age","distance")
df[,cols.to.log]<-lapply(df[,cols.to.log],log)

# factors: driver, hhsize, month, prov, year
clean_df <- df %>%
  pivot_longer(
    cols = c(driver, hhsize, month, prov, year),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Value = as.character(Value)) %>%
  unite("Variable_Value", Variable, Value, sep = "_") %>%
  mutate(Value = 1) %>%
  pivot_wider(
    names_from = Variable_Value,
    values_from = Value,
    values_fill = 0
  ) %>% select(-c(obs))

# output
confounders_df <- as.matrix(clean_df %>% select(-c(price, gas)))
# Run method for price and gas
price_gas_data <- list(A = clean_df$price, Y = clean_df$gas,
                       X = confounders_df)

result_price_gas_rf <- continuous_ade(data = price_gas_data, gamma = seq(0,log(4), length.out = 40), 
                                      conditional_mean = fit_y_given_X_mean_ranger, 
                                      score = fit_score_tree_factory(df = 2),
                                      conditional_median = fit_y_given_X_median_xgboost,
                                      conditional_mean_derivative = NULL, K = 5, 
                                      resmooth_bw = NULL, alpha = 0.05)
# get correction estimates and sd
sens_correction_est <- result_price_gas_rf$sens_correction_est
sens_correction_sd_hat <- result_price_gas_rf$sens_correction_sd_hat
n <- nrow(price_gas_data$X)
gamma_values = seq(0, 1, length.out = 40)
alpha = 0.05

# estimates and sd from Chernozhukov et. al.
gds <- -0.28	
gds_sd <- 0.06
lasso <- -0.16
lasso_sd <- 0.05
rf <- -0.01	
rf_sd <- 0.06

# make the uniform cis for each
uniform_ci_plot <- function(point_est, sd, sens_correction_est, method = "OLS",
                            sens_correction_sd_hat, n, gamma_values) {
  lower_point <- point_est - gamma_values * sens_correction_est
  upper_point <- point_est + gamma_values * sens_correction_est
  lower_uniform_ci <- point_est  - qnorm(1-alpha / 2) * sd - gamma_values *
    (sens_correction_est + qnorm(1-alpha / 2) * sens_correction_sd_hat/ sqrt(n))
  upper_uniform_ci <- point_est + qnorm(1-alpha / 2) * sd + gamma_values *
    (sens_correction_est + qnorm(1-alpha / 2) * sens_correction_sd_hat/ sqrt(n))
  
  # Combine data into a long-format dataframe
  data <- data.frame(
    gamma_values = gamma_values,
    lower_point = lower_point,
    upper_point = upper_point,
    lower_uniform_ci = lower_uniform_ci,
    upper_uniform_ci = upper_uniform_ci
  )
  # find gamma values where crossing zero for point est and uniform ci
  n_gamma_values <- length(gamma_values)
  
  if (point_est < 0) {
    point_cross <- find_zero_crossing(gamma_values[1], upper_point[1], 
                                      gamma_values[n_gamma_values], upper_point[n_gamma_values])
    ci_cross <- find_zero_crossing(gamma_values[1], upper_uniform_ci[1], 
                                   gamma_values[n_gamma_values], upper_uniform_ci[n_gamma_values])
    nudge_sign <- -1
    
  } else if (point_est > 0) {
    point_cross <- find_zero_crossing(gamma_values[1], lower_point[1], 
                                      gamma_values[n_gamma_values], lower_point[n_gamma_values])
    ci_cross <- find_zero_crossing(gamma_values[1], lower_uniform_ci[1], 
                                   gamma_values[n_gamma_values], lower_uniform_ci[n_gamma_values])
    nudge_sign <- 1

  }
  
  crossing_x <- c(point_cross, max(ci_cross, 0))

  
  # Create a data frame for the red points with labels
  crossing_points <- data.frame(
    x = crossing_x,
    y = rep(0, length(crossing_x)),       # y = 0 for all points
    label = round(crossing_x, 3)          # Labels as rounded x values
  )
  
  # Plot with shading
  ggplot(data, aes(x = gamma_values)) +
    # Shading for (uniform_ci)
    geom_ribbon(aes(ymin = lower_uniform_ci, ymax = upper_uniform_ci), 
                color = "gray70", fill = "gray",
                alpha = 0.3, show.legend = FALSE) +
    # Point estimates as dots
    geom_point(aes(y = lower_point), size = 0.5, color = "black") +
    geom_point(aes(y = upper_point), size = 0.5, color = "black") +
    # Add the red points for crossings at y = 0
    geom_point(data = crossing_points, aes(x = x, y = y), color = "red", size = 0.5) +
    # Add non-overlapping labels for the red points
    geom_text_repel(data = crossing_points, aes(x = x, y = y, label = label), 
                    color = "red", size = 2.5, nudge_y = -0.12 * nudge_sign, box.padding = 0.35) +
    labs(
      title = paste0(method),
      x = "gamma",
      y = "average derivative effect"
    ) +
    theme_minimal()
  
}

find_zero_crossing <- function(x1, y1, x2, y2) {
  if (y1 == y2) {
    stop("The line is parallel to y = 0 and does not cross it.")
  }
  x_cross <- x1 - y1 * (x2 - x1) / (y2 - y1)
  return(x_cross)
}

# Plots
# gds
uniform_ci_plot(point_est = gds, sd = gds_sd, 
                            sens_correction_est = sens_correction_est, 
                            method = "GDS", sens_correction_sd_hat = sens_correction_sd_hat,
                            n = n, gamma_values = gamma_values)
# lasso
uniform_ci_plot(point_est = lasso, sd = lasso_sd, 
                sens_correction_est = sens_correction_est, 
                method = "Lasso", sens_correction_sd_hat = sens_correction_sd_hat,
                n = n, gamma_values = gamma_values)
# rf
uniform_ci_plot(point_est = rf, sd = rf_sd, 
                sens_correction_est = sens_correction_est, 
                method = "Random Forest", sens_correction_sd_hat = sens_correction_sd_hat,
                n = n, gamma_values = gamma_values)




