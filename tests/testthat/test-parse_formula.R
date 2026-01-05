fmla1 = formula(y | cens(is_censored) ~ x1 + x2 + x3 + x1:x2 + (x3 | g1) + (x1*x2 | g2))
fmla2 = formula("sqrt(y) ~ x1 + x2")
fmla3 = formula(y ~ x1*x2*x3 + (x1 | g))
fmla4 = formula(y ~ 1)

test_that("lhs_works", {
  expect_match(parse_formula(fmla1)$lhs, "y | cens(is_censored)")
  expect_match(parse_formula(fmla2)$lhs, "sqrt(y)", fixed = TRUE)
  expect_match(parse_formula(fmla3)$lhs, "y")
  expect_match(parse_formula(fmla4)$lhs, "y")
})

test_that("fixed_works", {
  expect_setequal(parse_formula(fmla1)$fixed, c("x1", "x2", "x3", "x1:x2"))
  expect_setequal(parse_formula(fmla2)$fixed, c("x1", "x2"))
  expect_setequal(parse_formula(fmla3)$fixed, c("x1", "x2", "x3", "x1:x2", "x1:x3", "x2:x3", "x1:x2:x3"))
  expect_equal(parse_formula(fmla4)$fixed, character())
})

test_that("n_fixed_works", {
  expect_equal(parse_formula(fmla1)$n_fixed, 4)
  expect_equal(parse_formula(fmla2)$n_fixed, 2)
  expect_equal(parse_formula(fmla3)$n_fixed, 7)
  expect_equal(parse_formula(fmla4)$n_fixed, 0)
})

test_that("random_works", {
  expect_equal(parse_formula(fmla1)$random,
               list(g1 = c("1", "x3"), g2 = c("1", "x1", "x2", "x1:x2")))
  expect_null(parse_formula(fmla2)$random)
  expect_equal(parse_formula(fmla3)$random,
               list(g = c("1", "x1")))
  expect_null(parse_formula(fmla4)$random)
})

test_that("fixed_main_works", {
  expect_setequal(parse_formula(fmla1)$fixed_main, c("x1", "x2", "x3"))
  expect_setequal(parse_formula(fmla2)$fixed_main, c("x1", "x2"))
  expect_setequal(parse_formula(fmla3)$fixed_main, c("x1", "x2", "x3"))
  expect_equal(parse_formula(fmla4)$fixed_main, character())
})

test_that("fixed_interaction_works", {
  expect_setequal(parse_formula(fmla1)$fixed_interaction, c("x1:x2"))
  expect_null(parse_formula(fmla2)$fixed_interaction)
  expect_setequal(parse_formula(fmla3)$fixed_interaction, c("x1:x2", "x1:x3", "x2:x3", "x1:x2:x3"))
  expect_null(parse_formula(fmla4)$fixed_interaction)
})
