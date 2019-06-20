#' Find minimum R version required for package
#'
#' Recursively search dependencies for R version, and find the highest stated R
#' version requirement.
#' @source Based on ideas from
#'   http://stackoverflow.com/questions/38686427/determine-minimum-r-version-for-all-package-dependencies
#' @param pkg string with name of package to check
#' @examples
#' base <- c(
#'   "base", "compiler", "datasets", "grDevices", "graphics",
#'   "grid", "methods", "parallel", "profile", "splines", "stats",
#'   "stats4", "tcltk", "tools", "translations"
#' )
#' \dontrun{
#' base_reqs <- lapply(base, min_r_version)
#' contrib <- c(
#'   "KernSmooth", "MASS", "Matrix", "boot",
#'   "class", "cluster", "codetools", "foreign", "lattice",
#'   "mgcv", "nlme", "nnet", "rpart", "spatial", "survival"
#' )
#' contrib_reqs <- lapply(contrib, min_r_version)
#' min_r_version("icd")
#' }
#' @importFrom magrittr %>%
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages contrib.url compareVersion
#' @importFrom stringr str_subset str_replace_all
#' @export
min_r_version <- function(pkg) {
  avail <- available.packages(
    contrib.url("https://cloud.r-project.org")
  )
  deps <- package_dependencies(pkg, db = avail, recursive = TRUE)
  if (is.null(deps)) stop("package not found")
  pkgs <- deps[[1]]
  repo <- getOption("repo")
  if (is.null(repo)) repo <- "https://cloud.r-project.org"
  matches <- avail[, "Package"] %in% pkgs
  pkg_list <- avail[matches, "Depends"]
  vers <- str_subset(pkg_list, "^R$|^R \\(.*\\)$") %>%
    str_replace_all(., "[^0-9.]", "")
  if (length(vers) == 0) {
    return("Not specified")
  }
  max_ver <- vers[1]
  if (length(vers) == 1) {
    return(max_ver)
  }
  for (v in 2:length(vers)) {
    if (compareVersion(vers[v], max_ver) > 0) max_ver <- vers[v]
  }
  max_ver
}
