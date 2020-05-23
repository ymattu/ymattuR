#' Create .proj File
#' @param dir Directory to make .proj file
#' @export
create_proj_file <- function(dir) {
  path <- file.path(dir, paste0(basename(dir), ".Rproj"))
  template_path <- system.file("templates/template.Rproj",
                               package = "usethis")
  x <- c("Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
         "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
         "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8",
         "", "RnwWeave: knitr", "LaTeX: pdfLaTeX")

  cat(paste(x, collapse="\n"), file = template_path)

  file.copy(template_path, path)
  message(path, " has been created")
}
