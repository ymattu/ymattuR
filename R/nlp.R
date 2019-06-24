#' Install MeCab
#'
#' @importFrom glue glue
#' @export
install_mecab <- function() {
  os <- get_os()
  shfile <- system.file("mecab.sh", package = "ymattuR")

  if(os %in% c("mac", "linux")) {
    system(glue("sh {shfile}"))
  } else if(os == "win") {
    cat("WIP")
  } else {
    stop("Oops!, You cannot install mecab.")
  }
}

#' MeCab Wakati Impl
#' @param string string
#' @param tagger_param parameters of MeCab Tagger
#' @param extract_pattern POS regex
#' @importFrom purrr map
#' @importFrom stringr str_c str_detect
#' @importFrom tidyr separate
#' @importFrom dplyr mutate_if filter select summarise
mecab_wakati_impl <- function (string, tagger_param = list(l = 2, d = NULL),
                          extract_pattern = NULL) {
  if (length(x = tagger_param) > 0) {
    tagger_opt_str <- map(names(tagger_param), function (tg) {
        if (!is.null(tagger_param[[tg]]) & is.element(tg, c("l", "d"))) {
          return(str_c(str_c("-", tg), tagger_param[tg], sep = " "))
        }
      }
    ) %>%
      unlist %>%
      str_c(collapse = " ")
  } else {
    tagger_opt_str <- ""
  }

  ex_surface <- mecab_df(str = as.character(string), tagger_opt = tagger_opt_str) %>%
    separate(col = .data$feature,
             into = c("pos", "pos1", "pos2", "pos3", "ctype", "cform", "baseform", "orth", "pron"),
             sep = ",",
             fill = "right") %>%
    {
      if(!is.null(extract_pattern)) {
        filter(., str_detect(.$pos, extract_pattern))
      } else {
        .
      }
    } %>%
    mutate_if(is.factor, as.character) %>%
    select(.data$surface)

  if (nrow(ex_surface) < 1) {
    return("")
  } else {
    return(summarise(ex_surface, sentence = str_c(.data$surface, collapse = " ")) %>%
             .$sentence )
  }
}

#' MeCab Wakati
#' @param str string vector
#' @param tagger_param parameters of MeCab Tagger
#' @param extract_pattern POS regex
#' @examples
#' \dontrun{
#' meacb_wakati(string = "私は大きい大学へ行く",
#'   tagger_param = list(d = "/usr/local/lib/mecab/dic/mecab-ipadic-neologd"),
#'   extract_pattern = "名詞|形容詞")
#' }
#' @importFrom purrr map_chr
#' @export
mecab_wakati <- function(str, tagger_param = NULL, extract_pattern = NULL) {
  res <- map_chr(str, ~mecab_wakati_impl(.x,
                                         tagger_param = tagger_param,
                                         extract_pattern = extract_pattern))
  return(res)
}


