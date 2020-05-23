#' Write to clipboard
#'
#' @param x dataframe or matrix
#' @importFrom utils write.table
#' @export
write_clipboard <- function(x) {
    os <- get_os()
    if (os == "mac") {
        if (is.data.frame(x) | is.matrix(x)) {
            write.table(x, pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)
        } else write.table(t(x), pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)
    } else if (os == "win") {
        if (is.data.frame(x) | is.matrix(x)) {
            write.table(x, file = "clipboard", sep = "\t", row.names = FALSE, quote = FALSE)
        } else write.table(t(x), file = "clipboard", sep = "\t", row.names = FALSE, quote = FALSE)
    } else {
        stop("This function is available only in Windows or Mac OS")
    }
}
