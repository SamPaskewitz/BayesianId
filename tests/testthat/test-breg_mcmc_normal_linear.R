data("penguins")
test_data = na.omit(penguins) %>% dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex)

fit = breg_mcmc(y ~ x1 + x2, data = test_data, seed = 1212)
compare = breg_laplace(y ~ x1 + x2, data = test_data) # Laplace aprx for comparison

test_that("coef gives similar results", {
  expect_equal(coef(fit), coef(compare), tolerance = 1e-3)
})

test_that("vcov gives similar results", {
  expect_equal(vcov(fit), vcov(compare), tolerance = 1e-2)
})

test_that("similar CI's", {
  expect_equal(posterior_interval(fit, prob = 0.95), confint(compare, level = 0.95), tolerance = 1e-2)
})

test_that("log_evidence similar", {
  expect_equal(fit$log_evidence, compare$log_evidence, tolerance = 1e-2)
})

test_that("print method runs", {
  expect_no_error(print(fit))
})

test_that("summary method runs", {
  expect_no_error(print(fit))
})

test_that("terms method runs", {
  expect_no_error(terms(fit))
})

test_that("model.frame method runs", {
  expect_no_error(model.frame(fit))
})

test_that("simulate method runs", {
  expect_no_error(simulate(fit))
})

test_that("plot method runs", {
  expect_no_error(plot(fit))
})

test_that("show_stancode runs", {
  expect_no_error(show_stancode(fit))
})
