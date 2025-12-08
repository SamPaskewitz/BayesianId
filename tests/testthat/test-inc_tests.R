test_that("it runs with lm", {
  data("mtcars")
  expect_no_error(inc_tests(lm(mpg ~ disp*gear,
                               data = mtcars)))
})

test_that("it runs with lmer", {
  data("ChickWeight")
  expect_no_error(inc_tests(lme4::lmer(weight ~ Time*Diet + (Time | Chick),
                                       data = ChickWeight)))
})
