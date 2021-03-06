R package avpc
================

## Overview

An R package `avpc` provides a function `apcomp` to estimate average
predictive comparisons. The average predictive is a predicted change in
an orginal-scale response variable with a unit change of input variable
(= explanatory variable) of interest (see [Gelman and Pardoe,
2007](https://journals.sagepub.com/doi/10.1111/j.1467-9531.2007.00181.x)).
The average predictive comparison is identical to regression
coefficients when the model is linear (i.e., link function is identity)
and contains no interaction terms. However, when the model contains
interactions or non-linear variable transformation (e.g., logit, log),
the average predictive comparison is different from regression
coefficients and gives deeper insights into how the input variable of
interest affects the response variable at the original scale while
properly controlling for influences of other variables.

# Installation

Use the following script to install the package:

``` r
remotes::install_github("aterui/avpc")
```

# Usage

Function `apcomp` uses model object `m` and input variable of interest
`u` as minimal arguments. Users can specify other input variables `v`
manually but this is NOT recommended.

``` r
# simulated data
set.seed(123)

n_sample <- 100
x1 <- rnorm(n_sample)
x2 <- rnorm(n_sample)
x3 <- sample(letters[1:3], size = n_sample, replace = TRUE)
mat <- model.matrix(model.frame(~ x1 + x2 + x3))
v_b <- runif(n = ncol(mat), min = 0, max = 1)

y <- mat %*% v_b + rnorm(n_sample)
m <- lm(y ~ x1 + x2 + x3)

# average predictive comparison
library(avpc)
apcomp(m, u = "x1")
```

    ## link function: identity - the inverse function was used to estimate an average predictive comparison

    ## $estimate
    ## [1] 0.6858253
    ## 
    ## $sim_estimate
    ## [1] 0.6827217
    ## 
    ## $sim_se
    ## [1] 0.1074857
    ## 
    ## $df_uv
    ## # A tibble: 10,000 x 13
    ##    sq_distance row_id col_id weight      u1     u2  sign   x2_v1 x3b_v1 x3c_v1
    ##          <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl> <dbl>   <dbl>  <dbl>  <dbl>
    ##  1       0          1      1  1     -0.560  -0.560     1 -0.710       0      1
    ##  2       1.03       2      1  0.493 -0.230  -0.560    -1  0.257       0      1
    ##  3       0.236      3      1  0.809  1.56   -0.560    -1 -0.247       0      1
    ##  4       6.09       4      1  0.141  0.0705 -0.560    -1 -0.348       0      0
    ##  5       6.52       5      1  0.133  0.129  -0.560    -1 -0.952       0      0
    ##  6       0.486      6      1  0.673  1.72   -0.560    -1 -0.0450      0      1
    ##  7       6.01       7      1  0.143  0.461  -0.560    -1 -0.785       1      0
    ##  8       1.01       8      1  0.498 -1.27   -0.560     1 -1.67        0      1
    ##  9       5.94       9      1  0.144 -0.687  -0.560     1 -0.380       1      0
    ## 10       7.81      10      1  0.114 -0.446  -0.560    -1  0.919       0      0
    ## # ... with 9,990 more rows, and 3 more variables: x2_v2 <dbl>, x3b_v2 <dbl>,
    ## #   x3c_v2 <dbl>
    ## 
    ## $df_u1v1
    ## # A tibble: 10,000 x 5
    ##    `(Intercept)`      x1      x2   x3b   x3c
    ##            <dbl>   <dbl>   <dbl> <dbl> <dbl>
    ##  1             1 -0.560  -0.710      0     1
    ##  2             1 -0.230   0.257      0     1
    ##  3             1  1.56   -0.247      0     1
    ##  4             1  0.0705 -0.348      0     0
    ##  5             1  0.129  -0.952      0     0
    ##  6             1  1.72   -0.0450     0     1
    ##  7             1  0.461  -0.785      1     0
    ##  8             1 -1.27   -1.67       0     1
    ##  9             1 -0.687  -0.380      1     0
    ## 10             1 -0.446   0.919      0     0
    ## # ... with 9,990 more rows
    ## 
    ## $df_u2v1
    ## # A tibble: 10,000 x 5
    ##    `(Intercept)`     x1      x2   x3b   x3c
    ##            <dbl>  <dbl>   <dbl> <dbl> <dbl>
    ##  1             1 -0.560 -0.710      0     1
    ##  2             1 -0.560  0.257      0     1
    ##  3             1 -0.560 -0.247      0     1
    ##  4             1 -0.560 -0.348      0     0
    ##  5             1 -0.560 -0.952      0     0
    ##  6             1 -0.560 -0.0450     0     1
    ##  7             1 -0.560 -0.785      1     0
    ##  8             1 -0.560 -1.67       0     1
    ##  9             1 -0.560 -0.380      1     0
    ## 10             1 -0.560  0.919      0     0
    ## # ... with 9,990 more rows
    ## 
    ## $interaction_term
    ## NULL
