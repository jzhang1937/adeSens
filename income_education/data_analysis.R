# Set seed for reproducibility
set.seed(90095)

# Load packages
library(tidyverse)
library(xgboost)
library(drape)
library(ggrepel)

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
                       conditional_mean_derivative = NULL, K = 5, 
                       resmooth_bw = NULL)
ols_enrolled_any <- lm(clean_data$enrolled_any ~ clean_data$log_income + confounders)

result_enrolled_any_10 <- binary_ade(data = enrolled_any_data, gamma = seq(0,log(4), length.out = 40), 
                                  conditional_mean = fit_y_given_X_mean_xgboost_binary, 
                                  score = fit_score_tree,
                                  conditional_mean_derivative = NULL, K = 10, 
                                  resmooth_bw = NULL)
# Run method for enrolled 4yr
enrolled_4yr_data <- list(A = clean_data$log_income, Y = clean_data$enrolled_4yr,
                          X = confounders)
result_enrolled_4yr <- binary_ade(data = enrolled_4yr_data, gamma = seq(0,log(4), length.out = 40), 
                                  conditional_mean = fit_y_given_X_mean_xgboost_binary, 
                                  score = fit_score_tree, 
                                  conditional_mean_derivative = NULL, K = 10, 
                                  resmooth_bw = NULL)
ols_enrolled_4yr <- lm(clean_data$enrolled_4yr ~ clean_data$log_income + confounders)

# Sensitivity plot, enrolled any
gamma_values <- seq(0,log(4), length.out = 40)
lower_point <- result_enrolled_any$lower_est
upper_point <- result_enrolled_any$upper_est
lower_point_ci <- result_enrolled_any$lower_point_ci
upper_point_ci <- result_enrolled_any$upper_point_ci
lower_uniform_ci <- result_enrolled_any$lower_uniform_ci
upper_uniform_ci <- result_enrolled_any$upper_uniform_ci

library(ggplot2)

library(ggplot2)

# Combine the data into a tidy format for shading
data <- data.frame(
  gamma_values = gamma_values,
  lower_point = lower_point,
  upper_point = upper_point,
  lower_ci_90 = lower_point_ci,
  upper_ci_90 = upper_point_ci,
  lower_ci_95 = lower_uniform_ci,
  upper_ci_95 = upper_uniform_ci
)

find_zero_crossing <- function(x1, y1, x2, y2) {
  if (y1 == y2) {
    stop("The line is parallel to y = 0 and does not cross it.")
  }
  x_cross <- x1 - y1 * (x2 - x1) / (y2 - y1)
  return(x_cross)
}
find_zero_crossing(gamma_values[10], lower_point[10], gamma_values[11], lower_point[11])
find_zero_crossing(gamma_values[7], lower_point_ci[7], gamma_values[8], lower_point_ci[8])
find_zero_crossing(gamma_values[6], lower_uniform_ci[6], gamma_values[7], lower_uniform_ci[7])

crossing_x <- c(0.3227441, 0.2223904, 0.197319)

# Create a data frame for the red points with labels
crossing_points <- data.frame(
  x = crossing_x,
  y = rep(0, length(crossing_x)),       # y = 0 for all points
  label = round(crossing_x, 3)          # Labels as rounded x values
)

# Plot with shading
ggplot(data[1:20,], aes(x = gamma_values)) +
  # Shading for (uniform_ci)
  geom_ribbon(aes(ymin = lower_ci_95, ymax = upper_ci_95, fill = "Uniform"), alpha = 0.3) +
  # Shading for (point_ci)
  geom_ribbon(aes(ymin = lower_ci_90, ymax = upper_ci_90, fill = "Point"), alpha = 0.5) +
  # Point estimates as dots
  geom_point(aes(y = lower_point), size = 0.5, color = "black") +
  geom_point(aes(y = upper_point), size = 0.5, color = "black") +
  # Add the red points for crossings at y = 0
  geom_point(data = crossing_points, aes(x = x, y = y), color = "red", size = 0.5) +
  # Add non-overlapping labels for the red points
  geom_text_repel(data = crossing_points, aes(x = x, y = y, label = label), 
                  color = "red", size = 2.5, nudge_y = -0.12, box.padding = 0.35) +
  # Custom legend and labels
  scale_fill_manual(
    name = "Confidence Interval",
    values = c("Uniform" = "gray70", "Point" = "gray50")
  ) +
  labs(
    title = "Point estimates and confidence \n intervals for the ade bounds",
    x = "gamma values",
    y = "average derivative effect"
  ) +
  theme_minimal()













## OLD ##


# Combine the data into a tidy format
plot_data <- data.frame(
  gamma_values = rep(gamma_values, 2),
  group = rep(c("Lower", "Upper"), each = length(gamma_values)),
  point_est = c(lower_point, upper_point),
  point_ci = c(lower_point_ci, upper_point_ci),
  uniform_ci = c(lower_uniform_ci, upper_uniform_ci)
)

# Plot
ggplot(plot_data, aes(x = gamma_values, color = group)) +
  geom_point(aes(y = point_est), size = 1) + # Point estimates as dots
  geom_errorbar(aes(ymin = point_ci, ymax = point_est), width = 0.02, linetype = "dotted") + # 90% CI (point_ci)
  geom_errorbar(aes(ymin = uniform_ci, ymax = point_est), width = 0.04, linetype = "solid") + # 95% CI (uniform_ci)
  labs(
    title = "Confidence Intervals and Point Estimates",
    x = "gamma values",
    y = "Estimates",
    color = "Group"
  ) +
  theme_minimal()

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
