
# setup -------------------------------------------------------------------

  context("test lm coefficient equivalency")

  pacman::p_load(tidyverse)
  ilogit <- function(x) 1 / (1 + exp(-x))


# test dataset ------------------------------------------------------------

  n_sample <- 100

  x1 <- rnorm(n = n_sample)
  x2 <- rnorm(n = n_sample)
  x3 <- sample(letters[1:5], size = n_sample, replace = TRUE)
  mat <- stats::model.matrix(model.frame(~ x1 + x2 + x3))
  v_b <- runif(n = ncol(mat), -1, 1)
  y <- rnorm(n = n_sample, mean = mat %*% v_b, sd = 1)


# run model ---------------------------------------------------------------

  m <- lm(y ~ x1 + x2 + x3)
  beta <- coef(m)
  names(beta) <- NULL


# test --------------------------------------------------------------------

  test_that("compare coefficients", {
    expect_equal(apcomp(m, u = "x1")$estimate,
                 beta[2])
    expect_equal(apcomp(m, u = "x2")$estimate,
                 beta[3])
    expect_equal(apcomp(m, u = "x3b")$estimate,
                 beta[4])
  })
