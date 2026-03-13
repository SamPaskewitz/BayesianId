data("penguins")
test_data = na.omit(penguins) %>% dplyr::rename(y = body_mass, x1 = flipper_len, f1 = island, f2 = sex)

test_that("runs with intercept only", {
  expect_no_error(breg_laplace(y ~ 1, data = test_data))
})

test_that("runs with one numeric predictor", {
  expect_no_error(breg_laplace(y ~ x1, data = test_data))
})

test_that("runs with one factor", {
  expect_no_error(breg_laplace(y ~ f1, data = test_data))
})

test_that("runs with multiple predictors", {
  expect_no_error(breg_laplace(y ~ x1 + f1 + f2, data = test_data))
})

test_that("runs with interactions", {
  expect_no_error(breg_laplace(y ~ x1*f1*f2, data = test_data))
})
