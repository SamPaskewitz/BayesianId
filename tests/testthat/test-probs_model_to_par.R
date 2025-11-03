model_probs = c(0.5, 0.0, 0.2, 0.3)
names(model_probs) = c("count ~ Trt + zBase", "count ~ Trt", "count ~ zBase", "count ~ 1")

model_list = restricted_models(count ~ Trt + zBase)
correct_answer = c(0.5, 0.7)
names(correct_answer) = c("Trt", "zBase")

test_that("test_model_to_par", {
  expect_equal(probs_model_to_par(model_probs, model_list),
               correct_answer)
})
