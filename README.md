README
================

## R package avpc

An R package `avpc` provides a function `apcomp` to estimate average
predictive comparisons. The average predictive is a predicted change in
a response variable at the original scale with a unit change of input
variable (= explanatory variable) of interest (see [Gelman and Pardoe,
2007](https://journals.sagepub.com/doi/10.1111/j.1467-9531.2007.00181.x)).
The average predictive comparison is identical to regression
coefficients when the model is linear (i.e., link function is identity)
and contains no interaction terms. However, when the model contains
interactions or non-linear variable transformation (e.g., logit, log),
the average predictive comparison is different from regression
coefficients and gives deeper insights into how the input variable of
interest affects the response variable at the original scale while
properly controlling for influences of other variables.
