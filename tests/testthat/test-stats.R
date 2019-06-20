context('Stats')

test_that("Build Formula", {
  answer <- as.formula(X ~ Y + Z + alpha)
  res <- build_formula(left = 'X',
                       right = c('Y', 'Z', 'alpha'))

  expect_equal(res, answer)
  expect_identical(class(res), 'formula')
})
