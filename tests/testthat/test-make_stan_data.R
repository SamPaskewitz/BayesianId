# set up test data
data("penguins")
test_data = na.omit(penguins) |> dplyr::rename(y = body_mass, x1 = flipper_len, x2 = bill_len, f1 = island, f2 = sex)
centered_test_data = test_data
centered_test_data[,c("x1", "x2")] = scale(centered_test_data[,c("x1", "x2")], center = TRUE, scale = FALSE)

# COMPLEX DATA
correct_X = model.matrix(object = y ~ x1*x2*f1,
                         data = centered_test_data,
                         contrasts.arg = list(f1 = contr_banova(a = 3)))[,-1]
stan_data = make_stan_data(formula = y ~ x1*x2*f1, data = test_data, prior_scale = 1.0, center = TRUE)

test_that("N is correct", {
  expect_equal(stan_data$N,
               nrow(test_data))
})

test_that("Y is correct", {
  expect_equal(stan_data$Y,
               test_data$y)
})

test_that("Y is correct", {
  expect_equal(stan_data$Ymean,
               mean(test_data$y))
})

test_that("K is correct", {
  expect_equal(stan_data$K,
               11)
})

test_that("X is correct", {
  expect_equal(stan_data$X,
               correct_X)
})

test_that("Xcol_scale is correct", {
  correct_Xcol_scale = rep(0.0, times = 11)
  names(correct_Xcol_scale) = colnames(correct_X)
  correct_Xcol_scale[c("x1", "x1:f11", "x1:f12")] = sd(test_data$x1)
  correct_Xcol_scale[c("x2", "x2:f11", "x2:f12")] = sd(test_data$x2)
  correct_Xcol_scale[c("f11", "f12")] = 1.0
  correct_Xcol_scale[c("x1:x2", "x1:x2:f11", "x1:x2:f12")] = sd(test_data$x1)*sd(test_data$x2)
  correct_Xcol_scale = array(correct_Xcol_scale, dim = ncol(correct_X))
  names(correct_Xcol_scale) = colnames(correct_X)

  expect_equal(stan_data$Xcol_scale,
               correct_Xcol_scale)
})

test_that("prior_only is correct", {
  expect_equal(stan_data$prior_only,
               FALSE)
})

test_that("prior_scale is correct", {
  expect_equal(stan_data$prior_scale,
               1)
})
