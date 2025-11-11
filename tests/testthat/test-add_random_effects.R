model_list = submodels(formula(y ~ x1*x2))

test_that("adding 1 random effect works", {
  expect_setequal(add_random_effects(model_list, c("x1"), "id")$formulas,
                  list(formula(y ~ x1 + x2 + x1:x2 + (x1 | id)),
                       formula(y ~ x1 + x2 + (x1 | id)),
                       formula(y ~ x1 + (x1 | id)),
                       formula(y ~ x2 + (1 | id)),
                       formula(y ~ 1 + (1 | id))
                       )
  )
})

test_that("adding 2 random effects (plus interaction) works", {
  expect_setequal(add_random_effects(model_list, c("x1", "x2", "x1:x2"), "id")$formulas,
                  list(formula(y ~ x1 + x2 + x1:x2 + (x1 + x2 + x1:x2 | id)),
                       formula(y ~ x1 + x2 + (x1 + x2 | id)),
                       formula(y ~ x1 + (x1 | id)),
                       formula(y ~ x2 + (x2 | id)),
                       formula(y ~ 1 + (1 | id))
                       )
  )
})
