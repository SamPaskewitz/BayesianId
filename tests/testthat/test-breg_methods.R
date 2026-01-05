# set up test data
data("penguins")
test_data = na.omit(penguins) |> dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex)
test_data$y_bin = as.integer(test_data$f2) - 1

test_that("methods run with normal linear model", {
  fit = breg(formula = y ~ x1*x2*f1,
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
  #expect_no_warning(posterior_predict(fit))
})

# bernoulli logistic model
