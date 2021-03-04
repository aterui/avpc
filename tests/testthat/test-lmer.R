
# setup -------------------------------------------------------------------

  context("test lmer coefficient equivalency")

  pacman::p_load(tidyverse)
  ilogit <- function(x) 1 / (1 + exp(-x))


# test dataset ------------------------------------------------------------

  n_sample <- 100
  n_group <- 10

  x1 <- rnorm(n = n_sample)
  x2 <- rnorm(n = n_sample)
  x3 <- sample(letters[1:5], size = n_sample, replace = TRUE)
  group <- gl(n = 10, k = 10)

  mat <- model.matrix(model.frame(~ x1 + x2 + x3))
  v_b <- runif(n = ncol(mat), -1, 1)
  y <- rnorm(n = n_sample,
                  mean = mat %*% v_b + rnorm(n = n_group)[group],
                  sd = 1)


# run model ---------------------------------------------------------------

  m <- lme4::lmer(y ~ x1 * x2 + x3 + (1 | group))
  beta <- m@beta
  names(beta) <- NULL


# test --------------------------------------------------------------------

  test_that("compare coefficients", {
    expect_equal(apcomp(m, u = "x1")$est,
                 beta[2])
    expect_equal(apcomp(m, u = "x2")$est,
                 beta[3])
    expect_equal(apcomp(m, u = "x3b")$est,
                 beta[4])
  })
