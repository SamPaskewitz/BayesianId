test_that("works for a = 2", {
  # from section 7.1 of Rouder et al. (2012)
  expect_equal(contr_banova(2),
               matrix(c(sqrt(2)/2, -sqrt(2)/2), nrow = 2, ncol = 1),
               tolerance = 1e-2)
})

test_that("works for a = 5", {
  # from section 7.1 of Rouder et al. (2012)
  expect_equal(contr_banova(5),
               matrix(c(0.89, -0.22, -0.22, -0.22, -0.22,
                        0, 0.87, -0.29, -0.29, -0.29,
                        0, 0, 0.82, -0.41, -0.41,
                        0, 0, 0, 0.71, -0.71),
                      nrow = 4,
                      ncol = 5,
                      byrow = TRUE) %>% t(),
               tolerance = 1e-2)
})
