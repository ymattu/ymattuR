.onAttach <- function(...) {
  if(get_os() %in% c("mac", "linux")) {
    whichmecab <- system("which mecab", intern = T)
    if (is.null(whichmecab)) {
      packageStartupMessage("This pacakge contains some functions that depends on MeCab. Please install it by `install_mecab`")
    }
  }
  packageStartupMessage("Be carefull, this package is for ymattu only.")
}

.onUnload = function(libpath) {
  library.dynam.unload("ymattuR", libpath)
}
