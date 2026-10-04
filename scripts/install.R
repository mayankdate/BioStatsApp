# Run from the project root: source("scripts/install.R")
# Includes local app, export, and static-preview dependencies.
local({
  packages <- c("shiny", "bslib", "ggplot2", "scales", "shinylive", "httpuv")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) || "@CRAN@" %in% repos) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  if (length(missing)) install.packages(missing, repos = repos)
  unavailable <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(unavailable)) {
    stop("Installation incomplete: ", paste(unavailable, collapse = ", "))
  }
  message("Dependencies ready. Run shiny::runApp() from the project root.")
})
