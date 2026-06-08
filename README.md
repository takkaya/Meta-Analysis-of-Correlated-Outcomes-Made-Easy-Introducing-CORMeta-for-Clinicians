# Bridging Statistical Rigor and Clinical Usability: The CORMeta App for Meta-Analysis of Correlated Outcomes

This repository contains supporting R code and example data for the CORMeta workflow, a clinician-friendly approach for conducting meta-analysis when multiple correlated outcomes are reported across studies.

## Repository contents

- `functions.R` — core plotting and multivariate meta-analysis helper functions.
- `example_data.csv` — small example dataset with correlated outcomes.
- `example_analysis.R` — reproducible example showing how to run the functions.
- `CITATION.cff` — citation metadata for the repository.
- `LICENSE` — open-source license.

## Required R packages

```r
install.packages(c("ggplot2", "dplyr"))
```

Load the packages and source the functions:

```r
library(ggplot2)
library(dplyr)

source("functions.R")
```

## Example usage

```r
d <- read.csv("example_data.csv")

y <- d$beta_alcohol
v <- d$se_beta^2

# Assumed true and working correlation matrices among outcomes.
# In applied analyses, these should be informed by prior knowledge,
# external data, or sensitivity analyses.
R <- matrix(0.5, nrow = length(y), ncol = length(y))
diag(R) <- 1

RWorking <- R

fit <- metafor.mod(
  y = y,
  v = v,
  R = R,
  RWorking = RWorking,
  data = d
)

fit$value_1  # pooled effect estimate
fit$value_2  # adjusted standard error
fit$value_3  # tau-squared
fit$value_4  # p-value

fit$plot
fit$plot_post
```

## Cohort-adjusted model

The `metafor.mod.cohort()` function allows users to include a design matrix for cohort or study-level adjustment.

```r
XMatrix <- model.matrix(~ cohort_new, data = d)

fit_cohort <- metafor.mod.cohort(
  y = y,
  v = v,
  R = R,
  RWorking = RWorking,
  XMatrix = XMatrix,
  data = d
)

fit_cohort$value_1
fit_cohort$value_2
fit_cohort$value_3
fit_cohort$value_4
```

## Notes for users

The current implementation is intended as a transparent research and teaching workflow. Users should carefully specify the outcome correlation matrix and examine sensitivity to alternative correlation assumptions.

## Citation

Please cite this repository and the associated manuscript when using the code:

> Akkaya Hocagil, T., Cook, R. J., & Ryan, L. M. (2025). Bridging Statistical Rigor and Clinical Usability: The CORMeta App for Meta-Analysis of Correlated Outcomes. Ankara Üniversitesi Tıp Fakültesi Mecmuası, 78(4), 338-347. https://doi.org/10.65092/autfm.1758848. GitHub repository.

A formal citation can also be generated from `CITATION.cff`.
