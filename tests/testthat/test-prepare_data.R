test_data = data.frame(y = c(-2, 9, 1, 8, 1),
                       x1 = factor(c("a", "b", "b", "a", "a")),
                       x1a = c("a", "b", "b", "a", "a"),
                       x2 = c(1, 2, 3, 1, 0))

mod_test_data = test_data
contrasts(mod_test_data$x1) = contr_banova(2)

test_that("factors and numeric variables are properly identified", {
  prep = prepare_data(test_data, y ~ x1 + x1a + x2)
  expect_equal(prep$x_numeric_names, c("x2"))
  expect_equal(prep$x_factor_names, c("x1", "x1a"))
})

test_that("factors are properly coded", {
  prep = prepare_data(test_data, y ~ x1)
  expect_equal(prep$data$y, test_data$y) # should not be changed
  expect_equal(prep$data$x1, mod_test_data$x1)
  expect_equal(prep$data$x2, test_data$x2) # should not be changed
})

test_that("character vectors are converted to factors and properly coded", {
  prep = prepare_data(test_data, y ~ x1a)
  expect_equal(prep$data$y, test_data$y) # should not be changed
  expect_equal(prep$data$x1a, mod_test_data$x1)
  expect_equal(prep$data$x2, test_data$x2) # should not be changed
})

test_that("centering works", {
  prep = prepare_data(test_data, y ~ x2, center = TRUE)
  expect_equal(prep$data$y, test_data$y) # should not be changed
  expect_equal(prep$data$x1, test_data$x1) # should not be changed
  expect_equal(prep$data$x2, test_data$x2 - mean(test_data$x2))
})

test_that("factors with centering num works together", {
  prep = prepare_data(test_data, y ~ x1 + x2, center = TRUE)
  expect_equal(prep$data$y, test_data$y) # should not be changed
  expect_equal(prep$data$x1, mod_test_data$x1)
  expect_equal(prep$data$x2, test_data$x2 - mean(test_data$x2))
})

test_that("factors with not centering num works together", {
  prep = prepare_data(test_data, y ~ x1 + x2, center = FALSE)
  expect_equal(prep$data$y, test_data$y) # should not be changed
  expect_equal(prep$data$x1, mod_test_data$x1)
  expect_equal(prep$data$x2, test_data$x2)
})
