context('MeCab')

test_that("MeCab Wakati", {
  target <- "私は大きい大学へ行く"
  answer <- "私 大きい 大学"
  res <- mecab_wakati(target, extract_pattern = "名詞|形容詞")
  expect_equal(res, answer)
})
