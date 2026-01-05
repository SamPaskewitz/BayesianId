# set up test data
data("penguins")
library(rstan)
test_data = na.omit(penguins) |> dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex)
test_data$y_bin = as.integer(test_data$f2) - 1

test_that("normal linear model runs with one numeric predictor", {
  expect_no_warning(breg(formula = y ~ x1,
                         data = test_data,
                         center = TRUE,
                         chains = 2,
                         iter = 1000))
})

test_that("normal linear model runs one factor predictor", {
  expect_no_warning(breg(formula = y ~ f1,
                         data = test_data,
                         center = TRUE,
                         chains = 2,
                         iter = 1000))
})

test_that("normal linear model runs with complex formula", {
  expect_no_warning(breg(formula = y ~ x1*x2*f1,
                         data = test_data,
                         center = TRUE,
                         chains = 2,
                         iter = 1000))
})

test_that("normal linear intercept model runs", {
  expect_no_warning(breg(formula = y ~ 1,
                         data = test_data,
                         chains = 2,
                         iter = 1000))
})

test_that("bernoulli logistic model runs", {
  expect_no_warning(breg(formula = y_bin ~ x1*x2*f1,
                         family = "bernoulli_logistic",
                         data = test_data,
                         iter = 1000))
})

test_that("bernoulli logistic intercept model runs", {
  expect_no_warning(breg(formula = y_bin ~ 1,
                         family = "bernoulli_logistic",
                         data = test_data,
                         iter = 1000))
})
