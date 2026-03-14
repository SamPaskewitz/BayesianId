data("penguins")
penguins = penguins |> mutate(across(c("bill_len", "flipper_len"), ~ scale(.x, scale = FALSE, center = TRUE)))
contrasts(penguins$island) = BayesianId:::contr_banova(3)
contrasts(penguins$sex) = BayesianId:::contr_banova(2)
head(penguins)

test_that("BMA runs with lm", {
  lm_full = lm(body_mass ~ island + flipper_len,
               data = na.omit(penguins))
  expect_no_error(bma(lm_full))
})

test_that("BMA runs with breg_laplace", {
  breg_laplace_full = breg_laplace(body_mass ~ island + flipper_len,
                                   data = na.omit(penguins),
                                   prior_scale = 0.707)
  expect_no_error(bma(breg_laplace_full))
})

test_that("BMA runs with breg_mcmc", {
  breg_mcmc_full = breg_laplace(body_mass ~ island + flipper_len,
                                data = na.omit(penguins),
                                prior_scale = 0.707)
  expect_no_error(bma(breg_mcmc_full))
})
