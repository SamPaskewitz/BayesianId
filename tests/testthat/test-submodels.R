model_list = submodels(formula(y ~ x1*x2 + (x1*x2 | g1) + (x2 | g2)))

test_that("model_names correct", {
  expect_setequal(model_list$model_names,
                  list("~ x1 + x2 + x1:x2",
                       "~ x1 + x2",
                       "~ x1",
                       "~ x2",
                       "~ 1")
                  )
})

test_that("formulas correct", {
  expect_setequal(model_list$formulas,
                  list(formula(y ~ 1 + x1 + x2 + x1:x2 + (1 + x1 + x2 + x1:x2 | g1) + (1 + x2 | g2)),
                       formula(y ~ 1 + x1 + x2 + (1 + x1 + x2 | g1) + (1 + x2 | g2)),
                       formula(y ~ 1 + x1 + (1 + x1 | g1) + (1 | g2)),
                       formula(y ~ 1 + x2 + (1 + x2 | g1) + (1 + x2 | g2)),
                       formula(y ~ 1 + (1 | g1) + (1 | g2)))
  )
})

test_that("included correct", {
  expect_setequal(model_list$included,
                  list(c("x1", "x2", "x1:x2"),
                       c("x1", "x2"),
                       c("x2"),
                       c("x1"),
                       NULL)
  )
})

test_that("omitted correct", {
  expect_setequal(model_list$omitted,
                  list(NULL,
                       c("x1:x2"),
                       c("x2", "x1:x2"),
                       c("x1", "x1:x2"),
                       c("x1", "x2", "x1:x2"))
                  )
})
