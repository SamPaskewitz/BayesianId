test_that("works for a = 2", {
  # from section 7.1 of Rouder et al. (2012)
  # note that contr_banova now divides by sqrt(2) as in bayestestR::contr.equalprior_pairs
  target = matrix(c(sqrt(2)/2, -sqrt(2)/2), nrow = 2, ncol = 1)
  expect_equal(contr_banova(2)*sqrt(2),
               target,
               tolerance = 1e-2)
})

test_that("works for a = 5", {
  target = matrix(c(0.89, -0.22, -0.22, -0.22, -0.22,
                    0, 0.87, -0.29, -0.29, -0.29,
                    0, 0, 0.82, -0.41, -0.41,
                    0, 0, 0, 0.71, -0.71),
                  nrow = 4,
                  ncol = 5,
                  byrow = TRUE) |> t()
  # from section 7.1 of Rouder et al. (2012)
  # note that contr_banova now divides by sqrt(2) as in bayestestR::contr.equalprior_pairs
  expect_equal(contr_banova(5)*sqrt(2),
               target,
               tolerance = 1e-2)
})
