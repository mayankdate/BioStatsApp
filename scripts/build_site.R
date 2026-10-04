# Run from the project root: source("scripts/build_site.R")
# Only explicitly listed app files are exported, never the whole repository.
local({
  if (!file.exists("app.R") || !file.exists("BiostatsApp.R")) {
    stop("Run this script from the BioStatsApp project root.")
  }
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    stop('Install dependencies first: source("scripts/install.R")')
  }

  app_files <- c("app.R", "BiostatsApp.R")
  stage <- tempfile("biostats-shinylive-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  copied <- file.copy(app_files, stage, overwrite = TRUE)
  if (!all(copied)) stop("Could not stage all app files.")

  # site/ is generated output. Clear it so deleted assets cannot linger.
  if (dir.exists("site")) unlink("site", recursive = TRUE)
  shinylive::export(
    appdir = stage,
    destdir = "site",
    quiet = FALSE,
    template_params = list(title = "Sample Size Studio | BioStatsApp")
  )
  if (!file.exists("site/index.html")) stop("Export did not produce index.html.")
  file.create("site/.nojekyll")
  message('Website ready in site/. Preview: httpuv::runStaticServer("site")')
})
