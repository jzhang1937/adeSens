# Set seed for reproducibility
set.seed(90095)

# Load packages
library(tidyverse)
library(xgboost)
library(drape)

# Check that all required code is in the directory
source("code/check_environment.R")
source("../estimators.R")
source("../inference.R")

# Prepare data
print("Prepare data")
source("code/prepare_data.R")
clear_environment()

# data to use
data <- merged |> select(PUBID, income, female, race, educJoint, wealth, enrolled_any,
                         enrolled_4yr) |> 
  filter(income > 10000 & income < 300000) |> drop_na()

# preprocess data (one-hot encoding and log)
# One-Hot Encoding Process
clean_data <- data %>%
  pivot_longer(
    cols = c(race, educJoint),
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
  ) %>% mutate(log_income = log(income), log_wealth = log(wealth)) %>%
  select(-c(PUBID, income, wealth))
confounders <- as.matrix(clean_data %>% select(-c(log_income, enrolled_any, enrolled_4yr)))
# Run method for enrolled any
enrolled_any_data <- list(A = clean_data$log_income, Y = clean_data$enrolled_any,
                          X = confounders)
result_enrolled_any <- binary_ade(data = enrolled_any_data, gamma = seq(0,log(4), length.out = 40), 
                       conditional_mean = fit_y_given_X_mean_xgboost_binary, 
                       score = fit_score_tree,
                       conditional_mean_derivative = NULL, K = 10, 
                       resmooth_bw = NULL)
ols_enrolled_any <- lm(clean_data$enrolled_any ~ clean_data$log_income + confounders)

# Run method for enrolled 4yr
enrolled_4yr_data <- list(A = clean_data$log_income, Y = clean_data$enrolled_4yr,
                          X = confounders)
result_enrolled_4yr <- binary_ade(data = enrolled_4yr_data, gamma = seq(0,log(4), length.out = 40), 
                                  conditional_mean = fit_y_given_X_mean_xgboost_binary, 
                                  score = fit_score_tree, 
                                  conditional_mean_derivative = NULL, K = 10, 
                                  resmooth_bw = NULL)
ols_enrolled_4yr <- lm(clean_data$enrolled_4yr ~ clean_data$log_income + confounders)

# Sensitivity plot
gamma_values <- seq(0,log(4), length.out = 40)
lower_point <- result_enrolled_any$lower_est
upper_point <- result_enrolled_any$upper_est
lower_point_ci <- result_enrolled_any$lower_point_ci
upper_point_ci <- result_enrolled_any$upper_point_ci
lower_uniform_ci <- result_enrolled_any$lower_uniform_ci
upper_uniform_ci <- result_enrolled_any$upper_uniform_ci

# Combine data into a long-format dataframe
plot_data <- tibble(
  gamma = rep(gamma_values, 6),
  y = c(
    lower_point, upper_point,   # point group
    lower_point_ci, upper_point_ci, # point ci groupd
    lower_uniform_ci, upper_uniform_ci # uniform group
  ),
  group = rep(
    c("point", "point_ci", "uniform_ci"),
    each = 2 * length(gamma_values)
  ),
  subgroup = rep(
    rep(c("lower", "upper"), 3),
    each = length(gamma_values)
  )
)

plot <- ggplot(plot_data, aes(x = gamma, y = y, color = group,
                              group = interaction(group, subgroup))) +
  geom_line(size = 1) +                            # Add lines
  labs(
    title = "Sensitivity Plot",
    x = "Gamma values",
    y = "ADE",
    color = ""
  ) +
  theme_bw()
plot
