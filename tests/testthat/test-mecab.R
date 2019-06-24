context('MeCab')

test_that("MeCab Wakati", {
  target <- "私は大きい大学へ行く"
  answer <- "私 大きい 大学"
  res <- mecab_wakati(target, extract_pattern = "名詞|形容詞")
  expect_equal(res, answer)
})

test_that("MeCab Wakati without pattern", {
  target <- c("私は大きい大学へ行く", "すもももももももものうち")
  answer <- c("私 は 大きい 大学 へ 行く", "すもも も もも も もも の うち")
  res <- mecab_wakati(target)
  expect_equal(res, answer)
})
