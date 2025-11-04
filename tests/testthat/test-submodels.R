model_list = submodels(formula(y ~ x1*x2))

test_that("model_names works", {
  expect_setequal(model_list$model_names,
                  list("y ~ x1 + x2 + x1:x2",
                       "y ~ x1 + x2",
                       "y ~ x1",
                       "y ~ x2",
                       "y ~ 1")
                  )
})

test_that("included works", {
  expect_setequal(model_list$included,
                  list(c("x1", "x2", "x1:x2"),
                       c("x1", "x2"),
                       c("x2"),
                       c("x1"),
                       NULL)
  )
})

test_that("omitted works", {
  expect_setequal(model_list$omitted,
                  list(NULL,
                       c("x1:x2"),
                       c("x2", "x1:x2"),
                       c("x1", "x1:x2"),
                       c("x1", "x2", "x1:x2"))
                  )
})
