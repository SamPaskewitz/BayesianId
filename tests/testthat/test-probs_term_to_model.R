test_that("invalid probs cause an error message", {
  main_probs = c(-0.5, 20)
  names(main_probs) = c("x1", "x2")
  model_list = submodels(formula(y ~ x1 + x2))
  expect_error(probs_term_to_model(main_probs, NULL, model_list),
               "Main effect probabilities must be > 0 and <= 1.")
})

test_that("non-interactive model (uniform) works", {
  main_probs = c(0.5, 0.5)
  names(main_probs) = c("x1", "x2")
  model_list = submodels(formula(y ~ x1 + x2))
  correct_answer = list("~ x1 + x2" = 0.25,
                        "~ x1" = 0.25,
                        "~ x2" = 0.25,
                        "~ 1" = 0.25) |> unlist()
  expect_equal(probs_term_to_model(main_probs, NULL, model_list),
               correct_answer)
})

test_that("non-interactive model (non-uniform) works", {
  main_probs = c(0.2, 0.7)
  names(main_probs) = c("x1", "x2")
  model_list = submodels(formula(y ~ x1 + x2))
  correct_answer = list("~ x1 + x2" = 0.14,
                        "~ x1" = 0.06,
                        "~ x2" = 0.56,
                        "~ 1" = 0.24) |> unlist()
  expect_equal(probs_term_to_model(main_probs, NULL, model_list),
               correct_answer)
})

test_that("works with 2-way interactions (example 1)", {
  main_probs = list("x1" = 0.5, "x2" = 0.5) |> unlist()
  intr_condprobs = list("x1:x2" = 0.8) |> unlist()
  model_list = submodels(formula(y ~ x1 + x2 + x1:x2))
  correct_answer = list("~ x1 + x2 + x1:x2" = 0.2,
                        "~ x1 + x2" = 0.05,
                        "~ x1" = 0.25,
                        "~ x2" = 0.25,
                        "~ 1" = 0.25) |> unlist()
  expect_equal(probs_term_to_model(main_probs, intr_condprobs, model_list),
                                                                                                 correct_answer)
})

test_that("works with 2-way interactions (example 2)", {
    main_probs = list("x1" = 0.1, "x2" = 0.9) |> unlist()
    intr_condprobs = list("x1:x2" = 0.5) |> unlist()
    model_list = submodels(formula(y ~ x1 + x2 + x1:x2))
    correct_answer = list("~ x1 + x2 + x1:x2" = 0.045,
                          "~ x1 + x2" = 0.045,
                          "~ x1" = 0.01,
                          "~ x2" = 0.81,
                          "~ 1" = 0.09) |> unlist()
  expect_equal(probs_term_to_model(main_probs, intr_condprobs, model_list),
               correct_answer)
})
