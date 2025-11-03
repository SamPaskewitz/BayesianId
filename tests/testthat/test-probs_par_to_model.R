test_that("non-interactive model (uniform) works", {
  prior_par_probs = c(0.5, 0.5)
  names(prior_par_probs) = c("x1", "x2")
  model_list = restricted_models(formula(y ~ x1 + x2))
  correct_answer = rep(0.25, times = 4)
  names(correct_answer) = c("y ~ x1 + x2", "y ~ x1", "y ~ x2", "y ~ 1")
  expect_equal(probs_par_to_model(prior_par_probs, model_list),
               correct_answer)
})

test_that("non-interactive model (non-uniform) works", {
  prior_par_probs = c(0.2, 0.7)
  names(prior_par_probs) = c("x1", "x2")
  model_list = restricted_models(formula(y ~ x1 + x2))
  correct_answer = c(0.14, 0.06, 0.56, 0.24)
  names(correct_answer) = c("y ~ x1 + x2", "y ~ x1", "y ~ x2", "y ~ 1")
  expect_equal(probs_par_to_model(prior_par_probs, model_list),
               correct_answer)
})

#test_that("interactive model (uniform) works", {
  # FINISH THIS
#})
