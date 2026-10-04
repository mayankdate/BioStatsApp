# BioStatsApp: contributor and AI coding guidance

## Purpose

This is an R Shiny biostatistics app, branded **Sample Size Studio**. It must run
both as ordinary Shiny in RStudio and as a Shinylive export on GitHub Pages.
Preserve both execution paths when making changes.

## Architecture

- `BiostatsApp.R` is the single source of truth for the app. It contains design
  tokens and CSS, UI/plot helpers, sample-size functions, distribution definitions
  and examples, inference helpers, the UI, the server and a final `shinyApp()` call.
- `app.R` sources that file and returns its Shiny application object. Keep it thin.
- Runtime dependencies: `shiny`, `bslib`, `ggplot2`, and explicit `scales::` calls.
- Build dependency: `shinylive`; local static preview uses `httpuv`.
- `scripts/build_site.R` stages an explicit file allowlist in a temporary folder
  and exports to `site/`. It never exports the repository root directly.
- GitHub Actions builds and deploys pushes to `main`; the workflow also has a
  manual trigger. Pages source must be set to GitHub Actions in repository settings.

## Commands (from the repository root)

```r
# R console
source("scripts/install.R")
shiny::runApp()

# Check / export / serve the browser version
source("scripts/check.R")
source("scripts/build_site.R")
httpuv::runStaticServer("site")
```

```sh
# Terminal / CI
Rscript scripts/install.R
Rscript scripts/check.R
Rscript scripts/build_site.R
```

## Change guidelines

- Keep the app easy to edit in one R file unless the owner asks for a refactor.
- Use relative paths. Never introduce machine-specific paths or a hard-coded
  working directory. Preserve filename case: Linux CI is case-sensitive.
- Keep all installation and export commands out of application startup.
- Use system fonts / explicit font collections. Do not reintroduce startup font
  downloads that can fail inside WebAssembly.
- Any new package must work in native R and have compatible webR binaries.
  Declare its use statically with `library(package)` or `package::function` so
  Shinylive can discover it. Update the install script and CI package list.
- If adding assets or modules, update the build staging logic and README;
  recursively copy required directories. Never publish `.Rhistory`, `.RData`,
  `.Rproj.user`, secrets, configuration or unrelated research files.
- No shell commands, local databases, persistent filesystem assumptions, server
  credentials or remote analysis services in the browser app without explicitly
  redesigning the hosting approach with the owner.
- Keep `site/` generated and untracked. Never hand-edit exported HTML to fix
  behavior that belongs in R source or build settings.
- Preserve explanations, distribution conventions, rounding rules, credits and
  citations. Mathematical changes require a clear reason and checks against
  an independent reference or R's established statistical functions.
- Validate inputs at reactive boundaries. Show actionable validation messages for
  impossible values instead of presenting `NaN`, `Inf` or broken plots as results.
- Avoid claiming the website is fully offline: initial assets and MathJax may
  need network access. Browser execution is different from persistent storage.
- Do not add a licence or remove attribution without the owner's instruction.

## Verification

Run `scripts/check.R` after runtime changes. It checks syntax, sources the app
launcher and resolves the UI/theme dependencies; it does not launch a server or
exercise all reactivity. For deployment changes, also export the app and check
it through a local HTTP server, not a `file://` URL.

For changed calculations, add focused checks with independently known answers.
Manually check affected controls, plots and validation messages in native Shiny
and the browser export. Before a release, cover rate/proportion/mean sample-size
tabs in all three modes, discrete and continuous distributions, example loading,
and all five inference tabs. Include one-sided and two-sided inference where
relevant. Do not describe an export or UI check as a full statistical validation.

Report what was tested and any environment limitation. A configured workflow is
not evidence that the site is deployed; confirm its successful deployment and URL
before making that claim.
