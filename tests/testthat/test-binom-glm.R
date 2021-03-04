
# setup -------------------------------------------------------------------

  context("test binomial glm coefficient equivalency")

  pacman::p_load(tidyverse)
  ilogit <- function(x) 1 / (1 + exp(-x))


# test dataset ------------------------------------------------------------

  n_sample <- 100

  x1 <- rnorm(n = n_sample)
  x2 <- rnorm(n = n_sample)
  x3 <- sample(letters[1:5], size = n_sample, replace = TRUE)
  mat <- stats::model.matrix(model.frame(~ x1 + x2 + x3))
  v_b <- runif(n = ncol(mat), -1, 1)
  y <- rbinom(n = n_sample, size = 1, prob = ilogit(mat %*% v_b))


# run model ---------------------------------------------------------------

  m <- glm(y ~ x1 + x2 + x3, family = binomial)
  beta <- coef(m)
  names(beta) <- NULL


# test --------------------------------------------------------------------

  test_that("compare coefficients", {
    expect_equal(apcomp(m, u = "x1", var_transform = "identity")$est,
                 beta[2])
    expect_equal(apcomp(m, u = "x2", var_transform = "identity")$est,
                 beta[3])
    expect_equal(apcomp(m, u = "x3b", var_transform = "identity")$est,
                 beta[4])
  })
