context('Data Frame')

test_that("Select columns without NA", {
  dat <- data.frame(observation = 1:20,
                    valueA = runif(n = 20),
                    valueB = runif(n = 20),
                    valueC = runif(n = 20),
                    valueD = runif(n = 20))
  dat[2:5, 3] <- NA
  dat[2:10, 4] <- NA
  dat[7:20, 5] <- NA
  answer <- dat %>%
    select_if(function(x) !any(is.na(x)))
  res <- select_nona_cols(dat)
  expect_equal(res, answer)
})

test_that("Delete all 0 columns", {
  dat <- iris %>%
    mutate(col1 = 0, col2 = 0)
  res <- del_all0_cols(dat)
  expect_equal(res, iris)
})

