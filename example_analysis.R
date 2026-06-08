# Example analysis for the CORMeta repository

library(ggplot2)
library(dplyr)

source("functions.R")

# Load example data

d <- read.csv("example_data.csv")

# Effect estimates and sampling variances

y <- d$beta_alcohol
v <- d$se_beta^2

# Example correlation assumptions
R <- matrix(0.5, nrow = length(y), ncol = length(y))
diag(R) <- 1

RWorking <- R

# Fit the overall model
fit <- metafor.mod(
  y = y,
  v = v,
  R = R,
  RWorking = RWorking,
  data = d
)

cat("Overall model\n")
cat("Pooled estimate:", fit$value_1, "\n")
cat("Adjusted SE:", fit$value_2, "\n")
cat("Tau-squared:", fit$value_3, "\n")
cat("P-value:", fit$value_4, "\n\n")

# Display plots
print(fit$plot)
print(fit$plot_post)

# Fit the cohort-adjusted model
XMatrix <- model.matrix(~ cohort_new, data = d)

fit_cohort <- metafor.mod.cohort(
  y = y,
  v = v,
  R = R,
  RWorking = RWorking,
  XMatrix = XMatrix,
  data = d
)

cat("Cohort-adjusted model\n")
cat("Pooled estimate:", fit_cohort$value_1, "\n")
cat("Adjusted SE:", fit_cohort$value_2, "\n")
cat("Tau-squared:", fit_cohort$value_3, "\n")
cat("P-value:", fit_cohort$value_4, "\n")
