context('Characters')

test_that("hankana 2 Zenkana", {
  target <- "ｱｱﾏｼﾞﾃﾞﾔｯﾃﾗﾝﾈｴｴ"
  answer <- "アアマジデヤッテランネエエ"
  res <- hankana2zenkana(target)
  expect_equal(res, answer)
})

test_that("kansuji", {
  target1 <- '十二億九千六百万千三十三'
  answer1 <- 1296001033
  res1 <- kansuji2int(target1)

  target2 <- '三十三'
  answer2 <- 33
  res2 <- kansuji2int(target2)

  target3 <- '千三十三'
  answer3 <- 1033
  res3 <- kansuji2int(target3)


  expect_equal(res1, answer1)
  expect_equal(res2, answer2)
  expect_equal(res3, answer3)
})
