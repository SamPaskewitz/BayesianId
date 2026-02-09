library(emmeans)
data("penguins")
test_data = na.omit(penguins) |> dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex)
test_data = test_data |> dplyr::mutate(y_cens = ifelse(y > 5000, 5000, y), censoring = (y > 5000))

test_that("emmeans runs with normal linear model", {
  fit = breg(formula = y ~ f1 + x1,
             data = test_data,
             family = "normal_linear",
             center = TRUE,
             chains = 2,
             iter = 1000)
  expect_no_warning(emmeans(fit, "f1"))
})

test_that("emmeans runs with Bernoulli logistic model", {
  fit = breg(formula = f2 ~ f1 + x1,
             data = test_data,
             family = "bernoulli_logistic",
             center = TRUE,
             chains = 2,
             iter = 1000)
  expect_no_warning(emmeans(fit, "f1"))
})

test_that("emmeans runs with right censored linear model", {
  fit = breg(formula = y_cens ~ f1 + x1,
             data = test_data,
             family = "right_censored_linear",
             center = TRUE,
             chains = 2,
             iter = 1000)
  expect_no_warning(emmeans(fit, "f1"))
})
