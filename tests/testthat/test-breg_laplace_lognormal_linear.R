set.seed(1212)
test_data = data.frame(x1 = rnorm(100), x2 = rnorm(100), f = rep(c("a", "b"), times = 50) |> as.factor()) |>
  dplyr::mutate(y = exp(1 + 1.5*x1 + 0.3*(f == "a") + rnorm(100)))

fit = breg_laplace(y ~ x1 + x2 + f, data = test_data, family = "lognormal_linear", prior_scale = 100) # large prior scale -> results should be similar to MLE
compare = lm(log(y) ~ x1 + x2 + f, data = prepare_data(parse_formula(y ~ x1 + x2 + f), test_data)) # MLE for comparison

test_that("coef gives similar results", {
  expect_equal(coef(fit), coef(compare), tolerance = 1e-3)
})

test_that("vcov gives similar results", {
  expect_equal(vcov(fit), vcov(compare), tolerance = 1e-1)
})

test_that("similar CI's", {
  expect_equal(posterior_interval(fit, prob = 0.95), confint(compare, level = 0.95), tolerance = 1e-1)
})

test_that("log_evidence similar to -0.5*BIC", {
  expect_equal(fit$log_evidence, -0.5*BIC(compare), tolerance = 1e-1)
})

test_that("emmeans (on the linear predictor scale) gives similar results", {
  expect_equal(summary(emmeans::emmeans(fit, specs = "f"))$emmean,
               summary(emmeans::emmeans(compare, specs = "f"))$emmean,
               tolerance = 1e-1)
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

test_that("runs with intercept-only model", {
  expect_no_error(update(fit, formula = y ~ 1))
})
