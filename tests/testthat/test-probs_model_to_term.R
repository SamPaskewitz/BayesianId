test_that("model_to_term works", {
  model_probs = c(0.5, 0.0, 0.2, 0.3)
  names(model_probs) = c("~ x1 + x2", "~ x1", "~ x2", "~ 1")

  model_list = submodels(y ~ x1 + x2)
  correct_answer = c(0.5, 0.7)
  names(correct_answer) = c("x1", "x2")

  expect_equal(probs_model_to_term(model_probs, model_list),
               correct_answer)
})
