# BioStatsApp — Sample Size Studio

An interactive R Shiny app for sample-size planning, exploring probability
distributions, and statistical inference. One source runs in two places:

| Where | How it runs |
| --- | --- |
| RStudio / your computer | Ordinary Shiny, using your installed R and packages |
| GitHub Pages | Shinylive, using R compiled to WebAssembly inside the visitor's browser |

GitHub stores the source code. GitHub Pages serves the exported website; it does
not provide an R server. Visitors do not need R or RStudio installed.

## Features

- Sample size for rates, proportions and means: estimation, two-group comparisons,
  and cluster-randomised designs.
- Precision, power, allocation, nonresponse, finite-population and clustering
  adjustments where supported by each calculator.
- Explorer for 17 probability distributions, shaded probabilities and worked examples.
- Confidence intervals and tests for a mean, difference of means, proportion,
  difference of proportions, and variance / standard deviation.
- Explanations, formulas and plots alongside results.

## Start in RStudio

1. Open `BioStatsApp.Rproj`.
2. In the **R console**, install the local app dependencies once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "scales"))
   ```

3. Open `app.R` and click **Run App**, or run:

   ```r
   shiny::runApp()
   ```

Keep editing **`BiostatsApp.R`**. `app.R` is only a small launcher, so there is no
second copy of the application to maintain. Run these commands from the project
root. From elsewhere, use `shiny::runApp("path/to/BioStatsApp")`.

Use a current R release and current CRAN packages. The project disables workspace
restore/save and automatic history saving so old objects cannot hide missing setup.
Opening the `.code-workspace` file in VS Code also uses the project root.

## Publish on GitHub Pages — first time

### 1. Put these files in your repository root

Extract the archive and copy the **contents of `BioStatsApp/`** into your existing
project root, replacing the matching files. Avoid an extra nested `BioStatsApp/`
directory. Include the `.github` directory and `.gitignore` file.

Do not upload the ZIP itself as the application. Keep your existing `.git`
directory if you already use Git. `.Rhistory` is intentionally excluded from the
deliverable and ignored by Git; your existing local history need not be deleted.

### 2. Create or use a GitHub repository

For the simplest free setup, use a **public repository**, for example `BioStatsApp`.
An ordinary repository name is fine; it does not have to be `USERNAME.github.io`.
This workflow publishes pushes to **`main`**. If your branch is `master`, change
`branches: [main]` in `.github/workflows/deploy-pages.yml` to `branches: [master]`.

If the project is already linked to GitHub, commit and push through VS Code's
Source Control view as usual. Or use its **terminal**, from the project root:

```sh
git add .
git status
git commit -m "Set up Shiny and Shinylive GitHub Pages deployment"
git push
```

For a completely new local repository, create an **empty** GitHub repository
(without an initial README), then run the following with your actual URL:

```sh
git init
git branch -M main
git add .
git commit -m "Add BioStatsApp and GitHub Pages deployment"
git remote add origin https://github.com/YOUR-USERNAME/BioStatsApp.git
git push -u origin main
```

Use the new-repository commands only if you have not already configured Git and
its remote. Authentication is handled by your usual Git / VS Code setup.

### 3. Enable Pages

On GitHub, open the repository and choose:

**Settings → Pages → Build and deployment → Source → GitHub Actions**

This project uses an Actions deployment. You do not need a `docs/` folder or a
`gh-pages` branch, and you do not need to create a personal access token.

### 4. Run the workflow

Open **Actions → Deploy BioStatsApp to GitHub Pages → Run workflow** and select
`main` (or your configured branch). This also retries the initial deployment if
the first push happened before Pages was enabled.

The workflow installs dependencies, checks that the app and theme load, exports
the app with Shinylive, and publishes the generated `site/` directory. The first
build can take several minutes. Open the deployment link after both jobs pass.
For a standard project repository, the URL has this form:

```text
https://YOUR-USERNAME.github.io/BioStatsApp/
```

Use the exact URL shown in **Settings → Pages**. The repository name and its case
matter. Your computer can be switched off once the site is published.

## Everyday updates

1. Edit `BiostatsApp.R`.
2. Run `shiny::runApp()` locally and check the changed feature.
3. Commit and push to `main`.
4. Wait for the Pages workflow to finish; the website updates at the same URL.

You do not need to manually export the site for every change. GitHub does that.
Changes on other branches do not deploy unless you run the workflow manually;
the Pages environment may restrict which branches can deploy.

## Optional: preview the browser version locally

This is the closest check to what GitHub Pages will serve. In the R console,
from the project root:

```r
source("scripts/install.R")     # Installs missing app/build/preview dependencies
source("scripts/check.R")       # Loads the entry point and renders UI dependencies
source("scripts/build_site.R")  # Downloads assets on first build; writes site/
httpuv::runStaticServer("site")
```

Open the local HTTP address printed in the console in a browser. Stop it with
RStudio's Stop button or Escape. Do not double-click `site/index.html`: Shinylive
needs an HTTP server (localhost is fine) or an HTTPS host such as GitHub Pages.

`site/` is generated output and is ignored by Git. Each build replaces that
directory. Only `app.R` and `BiostatsApp.R` are staged for export. If you add
app assets such as `www/`, data, or sourced modules later, also update the explicit
copy logic in `scripts/build_site.R` to include them, including recursive copies
for directories. Do not export the entire repository.

## Files

| File | Purpose |
| --- | --- |
| `BiostatsApp.R` | All calculator logic, UI, plots and Shiny server; main editing file |
| `app.R` | Standard Shiny launcher |
| `BioStatsApp.Rproj` | RStudio project settings |
| `BioStatsApp.code-workspace` | Portable VS Code workspace |
| `scripts/install.R` | Install missing local app, export and preview packages |
| `scripts/check.R` | Startup and theme check, also run in CI |
| `scripts/build_site.R` | Stage app files and export the static website |
| `.github/workflows/deploy-pages.yml` | Build and publish after pushes to `main` |
| `.gitignore` | Exclude local history, caches, secrets and generated site files |
| `CLAUDE.md` | Project guidance for AI coding assistants |

## Runtime notes and troubleshooting

- **First load is slower than a normal webpage.** The browser loads R, Shiny and
  package assets. Subsequent loads may benefit from caching.
- **Internet access:** the published app needs its assets on first load. MathJax
  formula rendering also uses Shiny's external MathJax resource. Calculations do
  not call a remote analysis API. Fully offline startup is not guaranteed.
- **Fonts:** installed versions of the original font families are preferred, with
  system fallbacks. Startup no longer downloads Google Fonts; appearance can differ
  slightly between devices.
- **Package compatibility:** browser execution needs webR-compatible packages.
  A new package working in native R does not guarantee it works in Shinylive.
- **Visibility:** the browser receives the application source. Keep secrets out of
  app code and exported files. There is no persistent database or shared login.
- **Red workflow:** open the failed step under Actions. If Pages is not configured,
  select the GitHub Actions source in Settings, then rerun. For missing native
  dependencies, inspect the installation step. For browser package failures, check
  the [webR package repository](https://repo.r-wasm.org/).
- **No workflow visible:** check that `.github/workflows/deploy-pages.yml` was
  committed at the repository root, and that you pushed the expected branch.
- **App works locally but not online:** use the static preview above, then inspect
  the browser console (F12) for errors. Test in a current Chrome, Edge or Firefox.
- **Old online version:** first confirm the latest workflow passed, then hard-refresh
  or try a private window to rule out cached browser assets.
- **Old R packages:** the install helper installs missing packages; it does not
  force upgrades. Reinstall the listed packages with `install.packages()` if an
  older installed version lacks a required function.
- **Already tracked history/cache files:** `.gitignore` does not untrack files
  already in Git. If needed, run `git rm --cached --ignore-unmatch .Rhistory .RData
  .Ruserdata` and `git rm -r --cached --ignore-unmatch .Rproj.user`, then commit.
  These commands preserve the local files and do not erase earlier commits.

The startup check is not a full statistical audit or a browser interaction test.
Before sharing a release, check all three sample-size modes for each outcome,
the distribution explorer, all inference tabs, plots and formula rendering in
both ordinary Shiny and the exported browser version.

## Credits

Sample-size calculator concepts build on templates devised by
[Dr. Diwakar Mohan, Johns Hopkins Bloomberg School of Public Health](https://publichealth.jhu.edu/faculty/3214/diwakar-mohan).
The distribution explorer is inspired by
[Matt Bognar's probability applets](https://mabognar.github.io/apps/).
Cluster-trial formulas follow [Hayes & Bennett (1999)](https://doi.org/10.1093/ije/28.2.319).
The original application's statistical formulas and credits are retained.

## Deployment references

- [Posit: Shinylive for R](https://posit-dev.github.io/r-shinylive/)
- [Shinylive export reference](https://posit-dev.github.io/r-shinylive/reference/export.html)
- [GitHub: custom workflows with Pages](https://docs.github.com/en/pages/getting-started-with-github-pages/using-custom-workflows-with-github-pages)

