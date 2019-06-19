#' Hankaku Kana to Zenkaku Kana
#' @importFrom magrittr %>%
#' @importFrom stringi stri_unescape_unicode
#' @importFrom stringr str_replace_all
#' @param txt A Character
#' @export
hankana2zenkana <- function(txt){
  # character変換
  if (!is.character(txt)){ x <- as.character(txt) }

  # 濁点、半濁点文字の置換
  dh <- c("\\uff76\\uff9e", "\\uff77\\uff9e", "\\uff78\\uff9e", "\\uff79\\uff9e",
          "\\uff7a\\uff9e", "\\uff7b\\uff9e", "\\uff7c\\uff9e", "\\uff7d\\uff9e",
          "\\uff7e\\uff9e", "\\uff7f\\uff9e", "\\uff80\\uff9e", "\\uff81\\uff9e",
          "\\uff82\\uff9e", "\\uff83\\uff9e", "\\uff84\\uff9e", "\\uff8a\\uff9e",
          "\\uff8b\\uff9e", "\\uff8c\\uff9e", "\\uff8d\\uff9e", "\\uff8e\\uff9e",
          "\\uff8a\\uff9f", "\\uff8b\\uff9f", "\\uff8c\\uff9f", "\\uff8d\\uff9f",
          "\\uff8e\\uff9f") %>%
    stri_unescape_unicode()

  ## dh <- c("ｶﾞ","ｷﾞ","ｸﾞ","ｹﾞ","ｺﾞ","ｻﾞ","ｼﾞ","ｽﾞ","ｾﾞ","ｿﾞ","ﾀﾞ","ﾁﾞ","ﾂﾞ","ﾃﾞ","ﾄﾞ","ﾊﾞ","ﾋﾞ","ﾌﾞ","ﾍﾞ","ﾎﾞ","ﾊﾟ","ﾋﾟ","ﾌﾟ","ﾍﾟ","ﾎﾟ")
  dz <- c("\\u30ac", "\\u30ae", "\\u30b0", "\\u30b2", "\\u30b4", "\\u30b6", "\\u30b8",
          "\\u30ba", "\\u30bc", "\\u30be", "\\u30c0", "\\u30c2", "\\u30c5", "\\u30c7",
          "\\u30c9", "\\u30d0", "\\u30d3", "\\u30d6", "\\u30d9", "\\u30dc", "\\u30d1",
          "\\u30d4", "\\u30d7", "\\u30da", "\\u30dd") %>%
    stri_unescape_unicode()
  ## dz <- c("ガ","ギ","グ","ゲ","ゴ","ザ","ジ","ズ","ゼ","ゾ","ダ","ヂ","ヅ","デ","ド","バ","ビ","ブ","ベ","ボ","パ","ピ","プ","ペ","ポ")

  # 1byte 文字
  h1 <- c("\\uff71", "\\uff72", "\\uff73", "\\uff74", "\\uff75", "\\uff76", "\\uff77",
          "\\uff78", "\\uff79", "\\uff7a", "\\uff7b", "\\uff7c", "\\uff7d", "\\uff7e",
          "\\uff7f", "\\uff80", "\\uff81", "\\uff82", "\\uff83", "\\uff84", "\\uff85",
          "\\uff86", "\\uff87", "\\uff88", "\\uff89", "\\uff8a", "\\uff8b", "\\uff8c",
          "\\uff8d", "\\uff8e", "\\uff8f", "\\uff90", "\\uff91", "\\uff92", "\\uff93",
          "\\uff94", "\\uff95", "\\uff96", "\\uff97", "\\uff98", "\\uff99", "\\uff9a",
          "\\uff9b", "\\uff9c", "\\uff66", "\\uff9d", "\\uff61", "\\uff62", "\\uff63",
          "\\uff64", "\\uff65", "\\uff66", "\\uff67", "\\uff68", "\\uff69", "\\uff6a",
          "\\uff6b", "\\uff6c", "\\uff6d", "\\uff6e", "\\uff6f", "\\uff70") %>%
    stri_unescape_unicode()
  ## "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ"

  z1 <- c("\\u30a2", "\\u30a4", "\\u30a6", "\\u30a8", "\\u30aa", "\\u30ab", "\\u30ad",
          "\\u30af", "\\u30b1", "\\u30b3", "\\u30b5", "\\u30b7", "\\u30b9", "\\u30bb",
          "\\u30bd", "\\u30bf", "\\u30c1", "\\u30c4", "\\u30c6", "\\u30c8", "\\u30ca",
          "\\u30cb", "\\u30cc", "\\u30cd", "\\u30ce", "\\u30cf", "\\u30d2", "\\u30d5",
          "\\u30d8", "\\u30db", "\\u30de", "\\u30df", "\\u30e0", "\\u30e1", "\\u30e2",
          "\\u30e4", "\\u30e6", "\\u30e8", "\\u30e9", "\\u30ea", "\\u30eb", "\\u30ec",
          "\\u30ed", "\\u30ef", "\\u30f2", "\\u30f3", "\\u3002", "\\u300c", "\\u300d",
          "\\u3001", "\\u30fb", "\\u30f2", "\\u30a1", "\\u30a3", "\\u30a5", "\\u30a7",
          "\\u30a9", "\\u30e3", "\\u30e5", "\\u30e7", "\\u30c3", "\\u30fc") %>%
    stri_unescape_unicode()
    ## "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー"

  h <- c(dh, h1)
  z <- c(dz, z1)

  for(i in 1:length(z)){
    txt <- str_replace_all(txt, h[i], z[i])
  }

  return(txt)
}

#' Kansuji to Int
#' cf. https://gist.github.com/kos59125/bec05eab7c33072dc008
#' @param x A Character
#' @importFrom magrittr %>%
#' @importFrom stringi stri_unescape_unicode
#' @importFrom stringr str_sub
#' @importFrom glue glue
#' @importFrom purrr map map_dbl
#' @export
kansuji2int <- function(x) {
  # 万未満の位
  map_low <- c(0:10, 100, 1000)
  names(map_low) <- c("\\u3007", "\\u4e00", "\\u4e8c", "\\u4e09",
                      "\\u56db", "\\u4e94", "\\u516d", "\\u4e03",
                      "\\u516b", "\\u4e5d", "\\u5341", "\\u767e",
                      "\\u5343") %>%
    stri_unescape_unicode()
  # 〇 = 0, 一 = 1, 二 = 2, 三 = 3, 四 = 4,
  # 五 = 5, 六 = 6, 七 = 7, 八 = 8, 九 = 9,
  # 十 = 10, 百 = 100, 千 = 1000

  # 万以上の位
  map_up <- c(1e4, 1e8, 1e12)
  names(map_up) <- c("\\u4e07", "\\u5104", "\\u5146") %>%
    stri_unescape_unicode()

  # 全部
  map_all <- c(map_low, map_up)

  # (?<=(十|百|千)) みたいな桁の境界で分割する正規表現を作る
  pattern_low <- glue("(?<=({paste(names(map_low)[map_low >= 10], collapse = '|')}))")
  pattern_up <- glue("(?<=({paste(names(map_up), collapse = '|')}))")
  # 万以上の位で分解したときに末尾に位を表す文字がくるのでそれにマッチするパターン
  pattern_digit <- glue("(.*)({paste(names(map_up), collapse = '|')})$")

  # 万以上の位で分解
  # e.g. "十二億九千六百万千三十三" => ("十二億", "九千六百万", "千三十三")
  numbers <- strsplit(x, pattern_up, perl = TRUE)
  # 位のマッピング
  # e.g. ("十二億", "九千六百万", "千三十三")
  #      => ("億" = ("十二"), "万" = ("九千六百"), "一"  = ("千三十三"))
  numbers <- numbers %>%
    map(function(n) {
    # USE.NAMES = FALSE にして名前を明示的に位にする
    sapply(n, USE.NAMES = FALSE, function(k) {
      m <- regexpr(pattern_digit, k, perl = TRUE)
      if (m == -1) {
        # 万未満の場合は「一」の位とする
        structure(k, names = stri_unescape_unicode("\\u4e00"))
      } else {
        start <- attr(m, "capture.start")
        length <- attr(m, "capture.length")
        value <- str_sub(k, start[1], start[1] + length[1] - 1)
        digit <- str_sub(k, start[2], start[2] + length[2] - 1)
        structure(value, names = digit)
      }
    })
  })
  # 万未満の桁分解
  numbers <- numbers %>%
    map(~strsplit(., pattern_low, perl = TRUE))

  # 数値変換
  numbers %>% map_dbl(function(n) {
    # 万未満の数値化
    values <- n %>%
      map_dbl(function(d) {
        # 文字分解
        # e.g. "三十" => ("三", "十")
        char <- strsplit(d, "")
        # 各文字をマッピングに従って数値に変換して積を求める
        # e.g. ("三", "十") => 3 * 10 = 30
        values <- char %>% map_dbl(~prod(map_low[.]))
        # あとは足せば完了
        sum(values)
      })
    # 万以上の位の数値化
    # 一の位があるので map_up ではなく map_all を使う
    digits <- map_all[names(n)]
    # 位合わせ
    crossprod(values, digits)
  })
}
