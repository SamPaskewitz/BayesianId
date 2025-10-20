r_models = restricted_models(formula(y ~ x1*x2))


test_that("formula_strings works", {
  expect_setequal(r_models$formula_strings,
                  list("y ~ x1 + x2 + x1:x2",
                       "y ~ x1 + x2",
                       "y ~ x1",
                       "y ~ x2",
                       "y ~ 1")
                  )
})

test_that("omitted works", {
  expect_setequal(r_models$omitted,
                  list(NULL,
                       c("x1:x2"),
                       c("x2", "x1:x2"),
                       c("x1", "x1:x2"),
                       c("x1", "x2", "x1:x2"))
                  )
})
