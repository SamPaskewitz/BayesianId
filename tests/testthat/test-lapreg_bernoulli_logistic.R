data("penguins")
test_data = na.omit(penguins) %>% dplyr::rename(y = sex, x1 = flipper_len, x2 = bill_len)

fit = lapreg(y ~ x1 + x2, data = test_data, family = "bernoulli_logistic", prior_scale = 100) # large prior scale -> results should be similar to MLE
compare = glm(y ~ x1 + x2, data = prepare_data(parse_formula(y ~ x1 + x2), test_data), family = binomial())

test_that("coef gives similar results", {
  expect_equal(coef(fit), coef(compare), tolerance = 1e-3)
})

test_that("vcov gives similar results", {
  expect_equal(vcov(fit), vcov(compare), tolerance = 1e-1)
})

test_that("similar CI's", {
  expect_equal(posterior_interval(fit, prob = 0.95), confint(compare, level = 0.95), tolerance = 1e-2)
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
