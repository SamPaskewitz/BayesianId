test_that("log-linear: y = 0", {
  expect_error(check_data(stan_data = list(Y = c(0, 0.1)),
                          family = "lognormal_linear",
                          n_trials = NULL))
})

test_that("log-linear: y < 0", {
  expect_error(check_data(stan_data = list(Y = c(-0.2, 0.1)),
                          family = "lognormal_linear",
                          n_trials = NULL))
})

test_that("logistic: y not binary", {
  expect_error(check_data(stan_data = list(Y = c(-0.2, 0.1)),
                          family = "bernoulli_logistic",
                          n_trials = NULL))
})

test_that("binomial: y < 0", {
  expect_error(check_data(stan_data = list(Y = c(-1, 2)),
                          family = "binomial_logistic",
                          n_trials = 10))
})

test_that("binomial: y is not an integer", {
  expect_error(check_data(stan_data = list(Y = c(0.1, 2)),
                          family = "binomial_logistic",
                          n_trials = 10))
})

test_that("binomial: n_trials not specified", {
  expect_error(check_data(stan_data = list(Y = c(0, 2)),
                          family = "binomial_logistic",
                          n_trials = NULL))
})

test_that("binomial: y > n_trials", {
  expect_error(check_data(stan_data = list(Y = c(11, 2)),
                          family = "binomial_logistic",
                          n_trials = 10))
})

test_that("Poisson: y < 0", {
  expect_error(check_data(stan_data = list(Y = c(-1, 2)),
                          family = "poisson",
                          n_trials = NULL))
})

test_that("Poisson: y is not an integer", {
  expect_error(check_data(stan_data = list(Y = c(0.1, 2)),
                          family = "poisson",
                          n_trials = NULL))
})
