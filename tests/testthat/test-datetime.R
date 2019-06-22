context('Datetime')

test_that("Unixtime to JST", {
  target <- 1352068320
  target_chr <- as.character(target)
  answer <- as.POSIXct(target, tz = 'Asia/Tokyo',
                       origin = "1970-01-01")
  res <- unixtime2jst(target)
  res2 <- unixtime2jst(target_chr)

  expect_equal(res, answer)
  expect_equal(res2, answer)
})

test_that("Claculate Age", {
  FROM <- '1994-10-17'
  TO <- '2019-06-22'
  TO2 <- '2019-10-17'
  answer <- 24
  answer2 <- 25
  res <- age(FROM, TO)
  res2 <- age(FROM, TO2)

  expect_equal(res, answer)
  expect_equal(res2, answer2)
})
