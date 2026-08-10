# Set seed for reproducibility
set.seed(90095)

# Load packages
library(tidyverse)
library(xgboost)
library(drape)
library(foreign)
library(ranger)

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
# Run method for enrolled any
price_gas_data <- list(A = clean_df$price, Y = clean_df$gas,
                          X = confounders_df)
result_price_gas_postlasso <- continuous_ade(data = price_gas_data, gamma = seq(0,log(4), length.out = 40), 
                                  conditional_mean = fit_y_given_X_mean_postlasso, 
                                  score = fit_score_tree_factory(df = 2),
                                  conditional_median = fit_y_given_X_median_xgboost,
                                  conditional_mean_derivative = NULL, K = 5, 
                                  resmooth_bw = NULL, alpha = 0.05)
result_price_gas_rf <- continuous_ade(data = price_gas_data, gamma = seq(0,log(4), length.out = 40), 
                                   conditional_mean = fit_y_given_X_mean_ranger, 
                                   score = fit_score_tree_factory(df = 2),
                                   conditional_median = fit_y_given_X_median_xgboost,
                                   conditional_mean_derivative = NULL, K = 5, 
                                   resmooth_bw = NULL, alpha = 0.05)
ols_price_gas <- lm(clean_df$gas ~ clean_df$price + confounders_df)

# ensure diff memory location