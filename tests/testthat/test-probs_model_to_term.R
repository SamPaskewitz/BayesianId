model_probs = c(0.5, 0.0, 0.2, 0.3)
names(model_probs) = c("y ~ x1 + x2", "y ~ x1", "y ~ x2", "y ~ 1")

model_list = restricted_models(y ~ x1 + x2)
correct_answer = c(0.5, 0.7)
names(correct_answer) = c("x1", "x2")

test_that("model_to_term works", {
  expect_equal(probs_model_to_term(model_probs, model_list),
               correct_answer)
})
