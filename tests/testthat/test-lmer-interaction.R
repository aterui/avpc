
# setup -------------------------------------------------------------------

  context("test lmer interaction test")

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

  m <- lme4::lmer(y ~ x1 + x2 * x3 + (1 | group))
  beta <- m@beta
  names(beta) <- NULL


# average predictive comparison -------------------------------------------

  b_x1 <- apcomp(m, u = "x1")$estimate

  re <- apcomp(m, u = "x2")
  b_x2 <- re$estimate
  df_u2 <- re$df_u2v1
  int_terms <- re$interaction_term

  u2 <- df_u2 %>% dplyr::select("x2") %>% pull()
  vs <- df_u2 %>%
    dplyr::select(starts_with("x3"))

  df_test1 <- as_tibble(u2 * vs) %>%
    rename_with(.fn = ~ paste0("x2:", .x))
  df_test2 <- df_u2 %>% dplyr::select(dplyr::all_of(int_terms))


# test --------------------------------------------------------------------

  test_that("compare coefficients", {
    expect_equal(b_x1, beta[2])
    expect_equal(df_test1, df_test2)
  })
