# set up test data
data("penguins")
test_data = na.omit(penguins) |> dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex)
test_data = test_data |> dplyr::mutate(y_cens = ifelse(y > 5000, 5000, y), censoring = (y > 5000))

test_that("methods run with normal linear model", {
  fit = breg_mcmc(formula = y ~ x1*x2*f1,
             data = test_data,
             family = "normal_linear",
             center = TRUE,
             chains = 2,
             iter = 1000)
  expect_no_warning(print(fit))
  expect_no_warning(summary(fit))
  expect_no_warning(coef(fit))
  #expect_no_warning(plot(fit))
  expect_no_warning(vcov(fit))
  expect_no_warning(posterior_interval(fit))
  expect_no_warning(simulate(fit))
  expect_no_warning(posterior_predict(fit))
  expect_no_warning(posterior_linpred(fit))
})

test_that("methods run with Bernoulli logistic model", {
  fit = breg_mcmc(formula = f2 ~ x1*x2*f1,
             data = test_data,
             family = "bernoulli_logistic",
             center = TRUE,
             chains = 2,
             iter = 1000)
  expect_no_warning(print(fit))
  expect_no_warning(summary(fit))
  expect_no_warning(coef(fit))
  #expect_no_warning(plot(fit))
  expect_no_warning(vcov(fit))
  expect_no_warning(posterior_interval(fit))
  expect_no_warning(simulate(fit))
  expect_no_warning(posterior_predict(fit))
  expect_no_warning(posterior_linpred(fit))
  expect_no_warning(terms(fit))
})

test_that("methods run with right censored linear model", {
  fit = breg_mcmc(formula = y_cens ~ x1*x2*f1,
             data = test_data,
             family = "right_censored_linear",
             center = TRUE,
             chains = 2,
             iter = 1000)
  expect_no_warning(print(fit))
  expect_no_warning(summary(fit))
  expect_no_warning(coef(fit))
  #expect_no_warning(plot(fit))
  expect_no_warning(vcov(fit))
  expect_no_warning(posterior_interval(fit))
  expect_no_warning(simulate(fit))
  expect_no_warning(posterior_predict(fit))
  expect_no_warning(posterior_linpred(fit))
  expect_no_warning(terms(fit))
})
