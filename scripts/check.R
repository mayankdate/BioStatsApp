# Startup check used before deployment. Does not start an HTTP server.
# Run from the project root: source("scripts/check.R")
local({
  stopifnot(file.exists("app.R"), file.exists("BiostatsApp.R"))
  parse("app.R", encoding = "UTF-8")
  parse("BiostatsApp.R", encoding = "UTF-8")
  app_env <- new.env(parent = globalenv())
  app <- source("app.R", local = app_env, encoding = "UTF-8")$value
  stopifnot(inherits(app, "shiny.appobj"), is.function(app_env$server))
  # Resolves HTML dependencies and compiles the Bootstrap theme.
  html <- htmltools::renderTags(app_env$ui)
  stopifnot(nzchar(html$html), length(html$dependencies) > 0L)
  message("App entry point and UI dependencies loaded successfully.")
})
