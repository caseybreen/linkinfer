# linkinfer

**linkinfer** provides tools for diagnosing and correcting bias when linking census‑style datasets, following the methodology of Breen & Joo (2026).

## Installation

```r
install.packages("devtools")
devtools::install_github("caseybreen/linkinfer")
```

## Quick start

```r
library(linkinfer)

# Simulate data
sim   <- linked_data(n = 10000, p = 0.2, v1 = 0.5, v2 = 0.8)
pop   <- sim$population
linked<- sim$linked

# Compare populations
compare_linked(population = pop,
               linked = linked,
               vars = c("age","sex","race","education","urban","name_commonness"))

# Inverse‑probability weighting
ipw_res <- weight_ipw(population = pop,
                      linked_indicator = "linked",
                      covariates = c("age","sex","race","education","urban","name_commonness"),
                      method = "logistic")

# Diagnostics
diag <- weight_diagnostics(weights = ipw_res$weights,
                           population = pop,
                           linked = linked,
                           covariates = ipw_res$covariates)
print(diag)

# Adjust an outcome using the validation variable
adj <- adjust_estimate(weight_result = ipw_res,
                       linked_data = linked,
                       population_data = pop,
                       outcome = "race_diff",
                       validation_var = "middle_initial_match",
                       n_boot = 200)
print(adj)
```

The vignette `vignettes/introduction.Rmd` contains a full workflow example.

