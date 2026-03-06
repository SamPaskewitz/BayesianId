data("penguins")
test_data = na.omit(penguins) %>% dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex) |> dplyr::mutate(y = ifelse(y > 5000, 5000, y))

fit = breg_laplace(y ~ x1 + x2, data = test_data, prior_scale = 100, family = "right_censored_linear") # large prior scale -> results should be similar to MLE
compare = censReg::censReg(y ~ x1 + x2, right = 5000, data = prepare_data(parse_formula(y ~ x1 + x2), test_data))

test_that("coef gives similar results", {
  expect_equal(coef(fit), coef(compare)[1:3], tolerance = 1e-2)
})

test_that("vcov gives similar results", {
  expect_equal(vcov(fit), vcov(compare)[1:3, 1:3], tolerance = 1e-1)
})

test_that("similar CI's", {
  expect_equal(posterior_interval(fit, prob = 0.95), confint(compare, level = 0.95)[1:3,], tolerance = 1e-2)
})

test_that("log_evidence similar to -0.5*BIC", {
  expect_equal(fit$log_evidence, -0.5*BIC(compare), tolerance = 1e-1)
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

test_that("plot method runs", {
  expect_no_error(plot(fit))
})
