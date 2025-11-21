test_that("it works for TRUE examples", {
  expect_true(test_marginality(c("a", "b", "a:b")))
  expect_true(test_marginality(c("a", "b", "c", "a:b", "a:c", "b:c", "a:b:c")))
  expect_true(test_marginality(c("a", "b", "c", "d", "a:b", "a:c", "a:d", "b:c", "b:d", "c:d", "a:b:c", "a:b:d", "a:c:d", "b:c:d", "a:b:c:d")))
})

test_that("it works for FALSE examples", {
  expect_false(test_marginality(c("a", "a:b")))
  expect_false(test_marginality(c("a", "b", "a:b", "a:c", "b:c", "a:b:c")))
  expect_false(test_marginality(c("a", "b", "c", "a:b", "b:c", "a:b:c")))
})
