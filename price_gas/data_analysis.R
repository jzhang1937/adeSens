library(dml.sensemakr)
library(dplyr)
library(xgboost)
library(drape)
library(tidyverse)


source("estimators.R")
source("inference.R")

data("gasdemand")

# trim high and low incomes
df <- gasdemand |> filter((log_y > log(15000) & (exp(log_p) > 1.2))) 
clean_df <- df |> pivot_longer(
  cols = c(state_fips),
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
  )|> select(-region)
# output
subset_clean_df <- clean_df[,1:21]
confounders_df <- as.matrix(subset_clean_df %>% select(-c(log_q, log_p)))
# Run method for enrolled any
price_gas_data <- list(A = clean_df$log_p, Y = clean_df$log_q,
                       X = confounders_df)
for (m in 1:20) {
  result_price_gas <- continuous_ade(data = price_gas_data, gamma = seq(0,log(4), length.out = 40), 
                                     conditional_mean = fit_y_given_X_mean_xgboost, 
                                     score = fit_score_tree_postlasso_factory(df = 3),
                                     conditional_median = fit_y_given_X_median_xgboost,
                                     conditional_mean_derivative = NULL, K = 5, 
                                     resmooth_bw = m / 10, alpha = 0.1)
  print(m)
  print(result_price_gas$ade_est)
}
result_price_gas <- continuous_ade(data = price_gas_data, gamma = seq(0,log(4), length.out = 40), 
                                   conditional_mean = fit_y_given_X_mean_ranger, 
                                   score = fit_score_tree_postlasso_factory(df = 8),
                                   conditional_median = fit_y_given_X_median_xgboost,
                                   conditional_mean_derivative = NULL, K = 5,
                                   finite_diff = 0.01,
                                   resmooth_bw = NULL, alpha = 0.05)
ols_price_gas <- lm(clean_df$log_q ~ clean_df$log_p + confounders_df)


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