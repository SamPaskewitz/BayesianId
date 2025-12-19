# set up test data
data("penguins")
test_data = na.omit(penguins) |> dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex)

test_that("normal linear model runs", {
  expect_no_error(breg(formula = y ~ x1*x2*f1,
                       data = test_data,
                       center = TRUE,
                       chains = 2,
                       iter = 1000))
})

test_that("normal intercept model runs", {
  expect_no_error(breg(formula = y ~ 1,
                       data = test_data,
                       chains = 2,
                       iter = 1000))
})
