# =============================================================================
# Sample Size Studio
#
# Organised the way outcomes actually come: by what you are measuring
# (a rate, a proportion, a mean), and within that, by what you are doing with it
# (estimating one group, comparing two groups, or randomising clusters).
#
# Every calculator shows how the number was built, not just the number.
#
# Run with: shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# ---- Design tokens ----------------------------------------------------------
ink   <- "#0E2233"   # deep navy, primary text
paper <- "#F6F8FA"   # cool white ground
grid  <- "#DDE7EF"   # faint rules
teal  <- "#0B7285"   # structural accent
rose  <- "#C2255C"   # signal accent, reserved for the answer
muted <- "#5B7186"   # secondary text

app_theme <- bs_theme(
  version      = 5,
  bg           = paper,
  fg           = ink,
  primary      = teal,
  secondary    = muted,
  base_font    = font_google("Source Sans 3"),
  heading_font = font_google("Archivo"),
  code_font    = font_google("IBM Plex Mono")
)

# ---- Core statistics --------------------------------------------------------
z_conf  <- function(conf)  qnorm(1 - (1 - conf) / 2)   # 0.95 -> 1.96
z_power <- function(power) qnorm(power)                # 0.80 -> 0.8416

# Design effect from cluster sampling.
deff_of <- function(m, rho) 1 + (m - 1) * rho

# Finite population correction. N of 0 or NA means "population is effectively
# infinite", so no correction is applied.
apply_fpc <- function(n0, N) if (is.na(N) || N <= 0) n0 else n0 / (1 + (n0 - 1) / N)

# --- RATES -------------------------------------------------------------------
# Estimating one rate. Var(lambda_hat) = lambda / T, so T = z^2 * lambda / d^2.
# Person-time is what you need; expected events follow from it.
est_one_rate <- function(lambda, d_abs, conf, rho, m, nonresp) {
  z    <- z_conf(conf)
  T0   <- z^2 * lambda / d_abs^2
  de   <- deff_of(m, rho)
  Tdes <- T0 * de
  Tfin <- Tdes / (1 - nonresp)
  list(T0 = T0, deff = de, Tdes = Tdes, final = ceiling(Tfin),
       events = ceiling(lambda * Tfin))
}

# Comparing two rates, individually randomised. Returns person-time per group.
cmp_two_rate <- function(l1, l2, conf, power, r, nonresp) {
  za <- z_conf(conf); zb <- z_power(power)
  T1 <- (za + zb)^2 * (l1 + l2 / r) / (l1 - l2)^2
  list(raw = T1,
       T1 = ceiling(T1 / (1 - nonresp)),
       T2 = ceiling(r * T1 / (1 - nonresp)),
       rr = l2 / l1)
}

# --- PROPORTIONS -------------------------------------------------------------
est_one_prop <- function(p, d, conf, N, rho, m, nonresp) {
  z    <- z_conf(conf)
  n0   <- z^2 * p * (1 - p) / d^2
  nfpc <- apply_fpc(n0, N)
  de   <- deff_of(m, rho)
  ndes <- nfpc * de
  list(n0 = n0, nfpc = nfpc, deff = de, ndes = ndes,
       final = ceiling(ndes / (1 - nonresp)))
}

# Pooled-variance comparison with allocation ratio r = n2/n1.
cmp_two_prop <- function(p1, p2, conf, power, r, nonresp) {
  za   <- z_conf(conf); zb <- z_power(power)
  pbar <- (p1 + r * p2) / (1 + r)
  A    <- za * sqrt((1 + 1 / r) * pbar * (1 - pbar))
  B    <- zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / r)
  n1   <- (A + B)^2 / (p1 - p2)^2
  list(raw = n1,
       n1 = ceiling(n1 / (1 - nonresp)),
       n2 = ceiling(r * n1 / (1 - nonresp)),
       h  = abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))))
}

# --- MEANS -------------------------------------------------------------------
est_one_mean <- function(sd, d, conf, N, rho, m, nonresp) {
  z    <- z_conf(conf)
  n0   <- z^2 * sd^2 / d^2
  nfpc <- apply_fpc(n0, N)
  de   <- deff_of(m, rho)
  ndes <- nfpc * de
  list(n0 = n0, nfpc = nfpc, deff = de, ndes = ndes,
       final = ceiling(ndes / (1 - nonresp)))
}

cmp_two_mean <- function(mu1, mu2, sd, conf, power, r, nonresp) {
  za <- z_conf(conf); zb <- z_power(power)
  n1 <- (1 + 1 / r) * (za + zb)^2 * sd^2 / (mu1 - mu2)^2
  list(raw = n1,
       n1 = ceiling(n1 / (1 - nonresp)),
       n2 = ceiling(r * n1 / (1 - nonresp)),
       d  = abs(mu1 - mu2) / sd)
}

# --- Hayes & Bennett cluster-randomised trials -------------------------------
# All three return CLUSTERS PER ARM. k is the between-cluster coefficient of
# variation. Always round up: you cannot randomise part of a cluster.
clus_rate <- function(l1, l0, k1, k0, y, conf, power) {
  za <- z_conf(conf); zb <- z_power(power)
  1 + (za + zb)^2 * ((l1 + l0) / y + k1^2 * l1^2 + k0^2 * l0^2) / (l0 - l1)^2
}
clus_prop <- function(p1, p0, k1, k0, m, conf, power) {
  za <- z_conf(conf); zb <- z_power(power)
  1 + (za + zb)^2 *
    (p0 * (1 - p0) / m + p1 * (1 - p1) / m + k1^2 * p1^2 + k0^2 * p0^2) / (p0 - p1)^2
}
clus_mean <- function(mu1, mu0, s1, s0, k1, k0, m, conf, power) {
  za <- z_conf(conf); zb <- z_power(power)
  1 + (za + zb)^2 *
    ((s0^2 + s1^2) / m + k1^2 * mu1^2 + k0^2 * mu0^2) / (mu0 - mu1)^2
}

# ---- UI building blocks -----------------------------------------------------
# Every control carries its own explanation. That is the point of the rebuild.
param <- function(id, label, value, help, min = NA, max = NA, step = NA) {
  div(class = "param",
      numericInput(id, label, value = value, min = min, max = max, step = step),
      div(class = "param-help", help))
}

only_when <- function(prefix, mode, ...) {
  conditionalPanel(sprintf("input.%s_mode == '%s'", prefix, mode), ...)
}

mode_picker <- function(prefix) {
  radioButtons(paste0(prefix, "_mode"), "What are you doing?",
               choices = c("Estimate in one group"   = "estimate",
                           "Compare two groups"      = "compare",
                           "Cluster randomised trial" = "cluster"),
               selected = "estimate")
}

# Confidence always applies. Power only applies when testing a difference.
error_inputs <- function(prefix) {
  tagList(
    hr(),
    h5("Error rates"),
    param(paste0(prefix, "_conf"), "Confidence level", 0.95,
          "1 minus the significance level. Use 0.95 for a 95% interval.
           Entering 0.05 here is the classic slip.", 0.5, 0.999, 0.01),
    only_when(prefix, "compare",
              param(paste0(prefix, "_power"), "Power", 0.80,
                    "Chance of detecting the stated difference if it is real.
             0.80 is convention, 0.90 is safer and larger.", 0.5, 0.999, 0.01)),
    only_when(prefix, "cluster",
              param(paste0(prefix, "_powerc"), "Power", 0.80,
                    "Chance of detecting the stated difference if it is real.",
                    0.5, 0.999, 0.01))
  )
}

# Clustering and loss adjustments, shared by the estimation modes.
adjust_inputs <- function(prefix, with_fpc = TRUE) {
  tagList(
    hr(),
    h5("Real-world adjustments"),
    if (with_fpc)
      param(paste0(prefix, "_N"), "Population size (N)", 0,
            "Size of the full list you are sampling from. A small N reduces the
             sample needed, sometimes a lot. Leave at 0 if the population is large
             or unknown.", 0, NA, 1),
    param(paste0(prefix, "_rho"), "Intracluster correlation (\u03c1)", 0,
          "0 for a simple random sample. 0.05 to 0.20 is typical when you sample
           within villages, clinics or schools.", 0, 1, 0.01),
    param(paste0(prefix, "_m"), "Units per cluster (m)", 1,
          "How many units you take from each cluster. Leave at 1 if you are not
           sampling in clusters.", 1, NA, 1),
    param(paste0(prefix, "_nr"), "Expected non-response", 0,
          "Share you expect to lose to refusal, absence or closure. The sample is
           inflated to absorb it.", 0, 0.9, 0.01)
  )
}

ladder_row <- function(step, detail, value, emphasis = FALSE) {
  div(class = if (emphasis) "rung rung-final" else "rung",
      div(class = "rung-step", step),
      div(class = "rung-detail", detail),
      div(class = "rung-value", value))
}

answer_block <- function(value, unit, reading) {
  div(class = "answer",
      div(class = "answer-num", value),
      div(class = "answer-unit", unit),
      div(class = "answer-read", reading))
}

note_card <- function(title, ...) card(card_header(title, class = "note-head"), card_body(...))

theme_ss <- function() {
  theme_minimal(base_size = 13) +
    theme(text = element_text(colour = ink),
          panel.grid.major = element_line(colour = grid, linewidth = 0.4),
          panel.grid.minor = element_blank(),
          axis.title = element_text(colour = muted, size = 11),
          plot.subtitle = element_text(colour = muted, size = 11))
}

# The standard main-panel shell, identical across all three outcome tabs.
outcome_body <- function(prefix) {
  tagList(
    layout_columns(
      col_widths = c(5, 7),
      card(card_body(uiOutput(paste0(prefix, "_answer")))),
      card(card_header("How this number was built", class = "note-head"),
           card_body(uiOutput(paste0(prefix, "_ladder"))))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("What drives the number", class = "note-head"),
           card_body(plotOutput(paste0(prefix, "_plot"), height = "300px"))),
      card(card_header("Formula and assumptions", class = "note-head"),
           card_body(uiOutput(paste0(prefix, "_notes"))))
    )
  )
}

unit_input <- function(prefix, default) {
  textInput(paste0(prefix, "_unit"), "What are you counting?", value = default)
}

# ---- CSS --------------------------------------------------------------------
app_css <- sprintf("
  .navbar { border-bottom: 2px solid %s; }
  .navbar-brand { font-weight: 700; letter-spacing: -0.02em; }
  .lede { font-size: 1.02rem; color: %s; max-width: 62ch; line-height: 1.6; }
  .param { margin-bottom: 1.05rem; }
  .param label { font-weight: 600; font-size: 0.9rem; margin-bottom: 0.15rem; }
  .param-help { font-size: 0.78rem; color: %s; line-height: 1.45; margin-top: 0.3rem; }
  .answer { border-left: 4px solid %s; padding: 1rem 1.25rem; background: #fff;
            border-radius: 0 6px 6px 0; }
  .answer-num { font-family: 'IBM Plex Mono', monospace; font-size: 2.8rem;
                font-weight: 600; color: %s; line-height: 1; }
  .answer-unit { font-size: 0.95rem; color: %s; margin-top: 0.35rem; }
  .answer-read { margin-top: 0.75rem; font-size: 0.88rem; color: %s; max-width: 55ch;
                 line-height: 1.55; }
  .rung { display: grid; grid-template-columns: 1.5fr 2.5fr 1fr; gap: 0.75rem;
          padding: 0.55rem 0; border-bottom: 1px solid %s; align-items: baseline; }
  .rung-step { font-weight: 600; font-size: 0.87rem; }
  .rung-detail { font-size: 0.79rem; color: %s; }
  .rung-value { font-family: 'IBM Plex Mono', monospace; text-align: right; font-size: 1rem; }
  .rung-final { border-bottom: none; border-top: 2px solid %s; margin-top: 0.2rem;
                padding-top: 0.7rem; }
  .rung-final .rung-value { color: %s; font-weight: 600; font-size: 1.2rem; }
  .note-head { font-weight: 600; font-size: 0.9rem; background: #fff;
               border-bottom: 1px solid %s; }
  .assump li { font-size: 0.84rem; margin-bottom: 0.35rem; color: %s; }
  .matrix { width: 100%%; border-collapse: collapse; margin-top: 0.5rem; }
  .matrix th { font-size: 0.8rem; text-align: left; padding: 0.5rem 0.6rem;
               border-bottom: 2px solid %s; color: %s; font-weight: 600; }
  .matrix td { font-size: 0.85rem; padding: 0.6rem; border-bottom: 1px solid %s;
               vertical-align: top; }
  .matrix td:first-child { font-weight: 700; }
  .term { margin-bottom: 0.95rem; }
  .term-name { font-weight: 700; font-size: 0.9rem; }
  .term-sym { font-family: 'IBM Plex Mono', monospace; color: %s; }
  .term-def { font-size: 0.85rem; color: %s; line-height: 1.55; }
",
                   teal, muted, muted, rose, rose, muted, muted, grid, muted, ink, rose,
                   grid, muted, ink, muted, grid, teal, muted)

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title = "Sample Size Studio",
  theme = app_theme,
  header = tagList(withMathJax(), tags$style(HTML(app_css))),
  
  # -------------------------------------------------------------- Start here
  nav_panel(
    "Start here",
    layout_columns(
      col_widths = c(7, 5),
      card(card_body(
        h3("Two questions decide your formula"),
        p(class = "lede",
          "First: what kind of number is your outcome? Second: are you estimating it
           in one group, comparing two groups, or randomising whole clusters?
           Answer both and you are in the right place. Each tab below is one outcome
           type, and the three aims sit inside it as a switch."),
        tags$table(class = "matrix",
                   tags$tr(tags$th(""), tags$th("Estimate in one group"),
                           tags$th("Compare two groups"), tags$th("Cluster randomised")),
                   tags$tr(
                     tags$td("Rate"),
                     tags$td("Person-time needed to pin down an incidence rate"),
                     tags$td("Person-time per arm to detect a rate ratio"),
                     tags$td("Clusters per arm, Hayes & Bennett")),
                   tags$tr(
                     tags$td("Proportion"),
                     tags$td("Sample needed to estimate a prevalence"),
                     tags$td("Sample per arm to detect a difference in percentages"),
                     tags$td("Clusters per arm, Hayes & Bennett")),
                   tags$tr(
                     tags$td("Mean"),
                     tags$td("Sample needed to estimate an average"),
                     tags$td("Sample per arm to detect a difference in averages"),
                     tags$td("Clusters per arm, Hayes & Bennett"))
        ),
        br(),
        h5("Estimating is not testing"),
        p(class = "lede",
          "If you want a number with a confidence interval around it, you are
           estimating: you supply a margin of error, and power does not enter.
           If you want to detect a difference, you are testing: you supply the
           smallest difference worth finding, and power does. Picking the wrong one
           is the most common error in a protocol."),
        h5("Rates, proportions and means are not interchangeable"),
        p(class = "lede",
          "A proportion asks what share of people have something, out of a fixed
           denominator. A rate asks how often events happen per unit of person-time,
           so people contribute different amounts of follow-up. If your denominator
           is person-years rather than people, you need the rate tab.")
      )),
      card(
        card_header("Every term used here", class = "note-head"),
        card_body(
          div(class = "term",
              div(class = "term-name", "Confidence level ", span(class = "term-sym", "(1 - \u03b1)")),
              div(class = "term-def", "How often the interval would cover the truth on
                  repeated sampling. Use 0.95. This is one minus the significance level.")),
          div(class = "term",
              div(class = "term-name", "Power ", span(class = "term-sym", "(1 - \u03b2)")),
              div(class = "term-def", "Chance of detecting a real difference of the size
                  you specified. 0.80 by convention. Applies only when testing.")),
          div(class = "term",
              div(class = "term-name", "Margin of error ", span(class = "term-sym", "(d)")),
              div(class = "term-def", "Half the width of your confidence interval.
                  The strongest lever on sample size: halving it roughly quadruples n.")),
          div(class = "term",
              div(class = "term-name", "Absolute vs relative precision"),
              div(class = "term-def", "Absolute means \u00b10.005 events per person-year.
                  Relative means \u00b120% of whatever the rate turns out to be. For rare
                  events relative precision is usually the sensible one to specify.")),
          div(class = "term",
              div(class = "term-name", "Population size ", span(class = "term-sym", "(N)")),
              div(class = "term-def", "Size of the list you sample from. Small N pulls the
                  requirement down through the finite population correction. Set 0 to ignore.")),
          div(class = "term",
              div(class = "term-name", "Intracluster correlation ", span(class = "term-sym", "(\u03c1)")),
              div(class = "term-def", "How alike units are within a cluster. 0.05 to 0.20 is
                  typical for facility and household surveys.")),
          div(class = "term",
              div(class = "term-name", "Design effect ", span(class = "term-sym", "(DEFF)")),
              div(class = "term-def", "The penalty for cluster sampling,
                  1 + (m - 1)\u03c1. DEFF of 1.5 means 50% more units for the same precision.")),
          div(class = "term",
              div(class = "term-name", "Coefficient of variation ", span(class = "term-sym", "(k)")),
              div(class = "term-def", "Hayes & Bennett's between-cluster variation, typically
                  0.15 to 0.30. It drives the number of clusters far more than cluster size does.")),
          div(class = "term",
              div(class = "term-name", "Effect size ", span(class = "term-sym", "(h, d, RR)")),
              div(class = "term-def", "The difference expressed on a scale-free scale, so
                  studies can be compared. Cohen's h for proportions, Cohen's d for means,
                  the rate ratio for rates."))
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------- Rates
  nav_panel(
    "Rates",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        mode_picker("rate"),
        p(class = "param-help",
          "Rates are events per unit of person-time, so the answer is person-time,
           not a headcount."),
        only_when("rate", "estimate",
                  h5("The rate you expect"),
                  param("rate_lam", "Expected rate (\u03bb)", 0.02,
                        "Events per person-year. Take it from surveillance data or an earlier
                 study.", 0, NA, 0.005),
                  radioButtons("rate_prec", "Precision you need",
                               c("Relative (\u00b1% of the rate)" = "rel",
                                 "Absolute (\u00b1 fixed amount)"  = "abs"),
                               selected = "rel"),
                  conditionalPanel("input.rate_prec == 'rel'",
                                   param("rate_eps", "Relative precision", 0.20,
                                         "0.20 means the interval runs from 20% below to 20% above the rate.
                   Sensible for rare events, where an absolute margin is meaningless.",
                                         0.01, 1, 0.01)),
                  conditionalPanel("input.rate_prec == 'abs'",
                                   param("rate_d", "Absolute margin (d)", 0.005,
                                         "In events per person-year, the same units as the rate itself.",
                                         0.0001, NA, 0.001))),
        only_when("rate", "compare",
                  h5("The rates you expect"),
                  param("rate_l1", "Rate, group 1 (\u03bb\u2081)", 0.010,
                        "Events per person-year in the first group, often the unexposed or
                 control group.", 0, NA, 0.005),
                  param("rate_l2", "Rate, group 2 (\u03bb\u2082)", 0.020,
                        "Events per person-year in the second group. The ratio between the two
                 is your effect size.", 0, NA, 0.005),
                  param("rate_r", "Allocation ratio", 1,
                        "Person-time in group 2 relative to group 1. 1 means equal.",
                        0.1, 10, 0.1)),
        only_when("rate", "cluster",
                  h5("True event rates"),
                  param("rate_cl1", "Rate with intervention (\u03bb\u2081)", 0.010,
                        "Events per person-year in the intervention arm.", 0, NA, 0.001),
                  param("rate_cl0", "Rate in control (\u03bb\u2080)", 0.020,
                        "Events per person-year in the control arm.", 0, NA, 0.001),
                  h5("Between-cluster variation"),
                  param("rate_k1", "k\u2081 (intervention)", 0.25,
                        "Coefficient of variation between clusters. 0.15 to 0.30 is typical.",
                        0, NA, 0.01),
                  param("rate_k0", "k\u2080 (control)", 0.25,
                        "Usually set equal to k\u2081 unless you have evidence otherwise.",
                        0, NA, 0.01),
                  h5("Cluster size"),
                  param("rate_y", "Person-years per cluster (y)", 1000,
                        "Follow-up time accumulated in one cluster.", 1, NA, 10)),
        only_when("rate", "estimate",
                  hr(), h5("Real-world adjustments"),
                  param("rate_rho", "Intracluster correlation (\u03c1)", 0,
                        "0 if follow-up is not clustered.", 0, 1, 0.01),
                  param("rate_m", "Units per cluster (m)", 1,
                        "Leave at 1 if not clustered.", 1, NA, 1),
                  param("rate_nr", "Expected loss to follow-up", 0,
                        "Share of person-time you expect to lose.", 0, 0.9, 0.01)),
        only_when("rate", "compare",
                  hr(), h5("Real-world adjustments"),
                  param("rate_nr2", "Expected loss to follow-up", 0,
                        "Inflates person-time in both groups.", 0, 0.9, 0.01)),
        error_inputs("rate")
      ),
      outcome_body("rate")
    )
  ),
  
  # ------------------------------------------------------------- Proportions
  nav_panel(
    "Proportions",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        mode_picker("prop"),
        unit_input("prop", "participants"),
        only_when("prop", "estimate",
                  h5("What you are estimating"),
                  param("prop_p", "Expected proportion (p)", 0.5,
                        "Best guess at the true share. 0.5 needs the largest sample, so it is
                 the safe default when you have no prior.", 0, 1, 0.01),
                  param("prop_d", "Margin of error (d)", 0.05,
                        "Half-width of the interval. 0.05 gives \u00b15 percentage points.",
                        0.005, 0.5, 0.005)),
        only_when("prop", "compare",
                  h5("The difference worth detecting"),
                  param("prop_p1", "Proportion, group 1 (p\u2081)", 0.30,
                        "Expected share in the first group.", 0, 1, 0.01),
                  param("prop_p2", "Proportion, group 2 (p\u2082)", 0.50,
                        "Expected share in the second group. State the smallest gap that would
                 change a decision, not the one you hope for.", 0, 1, 0.01),
                  param("prop_r", "Allocation ratio (n\u2082/n\u2081)", 1,
                        "1 means equal groups, which is most efficient.", 0.1, 10, 0.1)),
        only_when("prop", "cluster",
                  h5("True proportions"),
                  param("prop_cp1", "Proportion with intervention (\u03c0\u2081)", 0.10,
                        "Expected prevalence in the intervention arm.", 0, 1, 0.01),
                  param("prop_cp0", "Proportion in control (\u03c0\u2080)", 0.20,
                        "Expected prevalence in the control arm.", 0, 1, 0.01),
                  h5("Between-cluster variation"),
                  param("prop_k1", "k\u2081 (intervention)", 0.25, "Typically 0.15 to 0.30.", 0, NA, 0.01),
                  param("prop_k0", "k\u2080 (control)", 0.25, "Usually equal to k\u2081.", 0, NA, 0.01),
                  h5("Cluster size"),
                  param("prop_cm", "Individuals per cluster (m)", 100,
                        "People sampled or followed in each cluster.", 1, NA, 10)),
        only_when("prop", "estimate", adjust_inputs("prop")),
        only_when("prop", "compare",
                  hr(), h5("Real-world adjustments"),
                  param("prop_nr2", "Expected non-response", 0,
                        "Inflates both groups.", 0, 0.9, 0.01)),
        error_inputs("prop")
      ),
      outcome_body("prop")
    )
  ),
  
  # ------------------------------------------------------------------- Means
  nav_panel(
    "Means",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        mode_picker("mean"),
        unit_input("mean", "participants"),
        only_when("mean", "estimate",
                  h5("What you are estimating"),
                  param("mean_sd", "Expected standard deviation (\u03c3)", 10,
                        "Spread in the population. From a pilot, the literature, or roughly the
                 range divided by four.", 0.001, NA, 0.5),
                  param("mean_d", "Margin of error (d)", 2,
                        "How close the estimate must be, in the same units as the measure.",
                        0.001, NA, 0.5)),
        only_when("mean", "compare",
                  h5("The difference worth detecting"),
                  param("mean_mu1", "Mean, group 1 (\u03bc\u2081)", 10, "Expected average in group 1.",
                        NA, NA, 0.5),
                  param("mean_mu2", "Mean, group 2 (\u03bc\u2082)", 12, "Expected average in group 2.",
                        NA, NA, 0.5),
                  param("mean_csd", "Common standard deviation (\u03c3)", 5,
                        "Assumed equal in both groups.", 0.001, NA, 0.5),
                  param("mean_r", "Allocation ratio (n\u2082/n\u2081)", 1, "1 means equal groups.",
                        0.1, 10, 0.1)),
        only_when("mean", "cluster",
                  h5("True means and within-cluster spread"),
                  param("mean_cmu1", "Mean with intervention (\u03bc\u2081)", 10, "", NA, NA, 0.5),
                  param("mean_cmu0", "Mean in control (\u03bc\u2080)", 12, "", NA, NA, 0.5),
                  param("mean_s1", "Within-cluster SD, intervention", 5,
                        "Spread among individuals inside a cluster, not between clusters.",
                        0.001, NA, 0.5),
                  param("mean_s0", "Within-cluster SD, control", 5, "", 0.001, NA, 0.5),
                  h5("Between-cluster variation"),
                  param("mean_k1", "k\u2081 (intervention)", 0.15, "Typically 0.10 to 0.25 for means.",
                        0, NA, 0.01),
                  param("mean_k0", "k\u2080 (control)", 0.15, "Usually equal to k\u2081.", 0, NA, 0.01),
                  h5("Cluster size"),
                  param("mean_cm", "Individuals per cluster (m)", 50, "", 1, NA, 5)),
        only_when("mean", "estimate", adjust_inputs("mean")),
        only_when("mean", "compare",
                  hr(), h5("Real-world adjustments"),
                  param("mean_nr2", "Expected non-response", 0, "Inflates both groups.", 0, 0.9, 0.01)),
        error_inputs("mean")
      ),
      outcome_body("mean")
    )
  ),
  
  nav_spacer(),
  nav_item(tags$a("Hayes & Bennett (1999)",
                  href = "https://doi.org/10.1093/ije/28.2.319",
                  target = "_blank", class = "nav-link"))
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {
  
  fmt <- function(x) format(x, big.mark = ",")
  # Fall back to a neutral word if the user clears the unit box.
  unit_of <- function(txt) if (is.null(txt) || !nzchar(trimws(txt))) "units" else trimws(txt)
  
  # =========================================================== RATES ==========
  rate_res <- reactive({
    req(input$rate_mode)
    if (input$rate_mode == "estimate") {
      req(input$rate_lam, input$rate_conf, input$rate_m)
      validate(need(input$rate_lam > 0, "The expected rate must be above zero."))
      d_abs <- if (input$rate_prec == "rel") input$rate_eps * input$rate_lam else input$rate_d
      validate(need(d_abs > 0, "Precision must be above zero."))
      est_one_rate(input$rate_lam, d_abs, input$rate_conf,
                   input$rate_rho, input$rate_m, input$rate_nr)
    } else if (input$rate_mode == "compare") {
      req(input$rate_l1, input$rate_l2, input$rate_conf, input$rate_power, input$rate_r)
      validate(need(input$rate_l1 != input$rate_l2, "The two rates must differ."))
      cmp_two_rate(input$rate_l1, input$rate_l2, input$rate_conf,
                   input$rate_power, input$rate_r, input$rate_nr2)
    } else {
      req(input$rate_cl1, input$rate_cl0, input$rate_y, input$rate_conf, input$rate_powerc)
      validate(need(input$rate_cl1 != input$rate_cl0, "The two rates must differ."))
      raw <- clus_rate(input$rate_cl1, input$rate_cl0, input$rate_k1, input$rate_k0,
                       input$rate_y, input$rate_conf, input$rate_powerc)
      list(raw = raw, clusters = ceiling(raw))
    }
  })
  
  output$rate_answer <- renderUI({
    r <- rate_res()
    if (input$rate_mode == "estimate") {
      answer_block(fmt(r$final), "person-years of observation",
                   sprintf("Over that follow-up you would expect about %s events. Estimating a
                 rate is really about accumulating events: the person-time is just how
                 you buy them.", fmt(r$events)))
    } else if (input$rate_mode == "compare") {
      answer_block(fmt(r$T1 + r$T2), "person-years in total",
                   sprintf("%s in group 1 and %s in group 2, to detect a rate ratio of %.2f.",
                           fmt(r$T1), fmt(r$T2), r$rr))
    } else {
      answer_block(r$clusters, "clusters per arm",
                   sprintf("%s clusters in total, each contributing about %s person-years.
                 Unrounded the formula gives %.2f; always round up.",
                           r$clusters * 2, fmt(input$rate_y), r$raw))
    }
  })
  
  output$rate_ladder <- renderUI({
    r <- rate_res()
    if (input$rate_mode == "estimate") {
      prec <- if (input$rate_prec == "rel")
        sprintf("\u00b1%.0f%% of the rate", input$rate_eps * 100)
      else sprintf("\u00b1%s per person-year", input$rate_d)
      tagList(
        ladder_row("Raw requirement", sprintf("z\u00b2\u03bb/d\u00b2 at %s", prec), fmt(round(r$T0))),
        ladder_row("After design effect",
                   sprintf("DEFF = %.2f from m = %s, \u03c1 = %s",
                           r$deff, input$rate_m, input$rate_rho), fmt(round(r$Tdes))),
        ladder_row("After loss to follow-up",
                   sprintf("divided by %.0f%% retained", (1 - input$rate_nr) * 100),
                   fmt(r$final), emphasis = TRUE),
        div(style = "margin-top:0.8rem;font-size:0.82rem;color:#5B7186;",
            sprintf("Expected events: %s. For relative precision the events needed are
                     fixed by the margin alone, so a rarer outcome simply costs more
                     person-time to reach the same number.", fmt(r$events)))
      )
    } else if (input$rate_mode == "compare") {
      tagList(
        ladder_row("Effect to detect",
                   sprintf("%s vs %s per person-year", input$rate_l1, input$rate_l2),
                   sprintf("RR = %.2f", r$rr)),
        ladder_row("Raw requirement, group 1",
                   sprintf("%s%% confidence, %s%% power",
                           input$rate_conf * 100, input$rate_power * 100),
                   fmt(round(r$raw))),
        ladder_row("After allocation ratio",
                   sprintf("group 2 is %s\u00d7 group 1", input$rate_r),
                   fmt(round(r$raw * (1 + input$rate_r)))),
        ladder_row("After loss to follow-up",
                   sprintf("divided by %.0f%% retained", (1 - input$rate_nr2) * 100),
                   fmt(r$T1 + r$T2), emphasis = TRUE)
      )
    } else {
      tagList(
        ladder_row("Effect to detect",
                   sprintf("%s vs %s per person-year", input$rate_cl1, input$rate_cl0),
                   sprintf("RR = %.2f", input$rate_cl1 / input$rate_cl0)),
        ladder_row("Between-cluster variation",
                   sprintf("k\u2081 = %s, k\u2080 = %s", input$rate_k1, input$rate_k0), ""),
        ladder_row("Cluster size",
                   sprintf("%s person-years each", fmt(input$rate_y)), ""),
        ladder_row("Clusters per arm", "rounded up from the formula",
                   r$clusters, emphasis = TRUE)
      )
    }
  })
  
  output$rate_plot <- renderPlot({
    r <- rate_res()
    if (input$rate_mode == "estimate") {
      eps_seq <- seq(0.05, 0.5, by = 0.01)
      ev <- vapply(eps_seq, function(e) ceiling(z_conf(input$rate_conf)^2 / e^2), numeric(1))
      here_eps <- if (input$rate_prec == "rel") input$rate_eps else input$rate_d / input$rate_lam
      ggplot(data.frame(e = eps_seq, n = ev), aes(e, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(e = here_eps, n = ceiling(z_conf(input$rate_conf)^2 / here_eps^2)),
                   colour = rose, size = 3.5) +
        scale_x_continuous(labels = scales::percent) +
        labs(x = "Relative precision", y = "Events needed",
             subtitle = "Events needed depend only on precision, never on how rare the outcome is.") +
        theme_ss()
    } else if (input$rate_mode == "compare") {
      lo <- max(0.0005, min(input$rate_l1, input$rate_l2) * 0.4)
      hi <- max(input$rate_l1, input$rate_l2) * 1.8
      l2_seq <- seq(lo, hi, length.out = 80)
      l2_seq <- l2_seq[abs(l2_seq - input$rate_l1) > input$rate_l1 * 0.05]
      tot <- vapply(l2_seq, function(ll) {
        x <- cmp_two_rate(input$rate_l1, ll, input$rate_conf, input$rate_power,
                          input$rate_r, input$rate_nr2); x$T1 + x$T2
      }, numeric(1))
      ggplot(data.frame(l = l2_seq, n = tot), aes(l, n)) +
        geom_vline(xintercept = input$rate_l1, linetype = "dotted", colour = muted) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(l = input$rate_l2, n = r$T1 + r$T2),
                   colour = rose, size = 3.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Rate in group 2", y = "Total person-years",
             subtitle = "Cost explodes as group 2 approaches group 1 (dotted line).") +
        theme_ss()
    } else {
      y_seq <- seq(input$rate_y / 5, input$rate_y * 3, length.out = 60)
      c_seq <- vapply(y_seq, function(yy)
        ceiling(clus_rate(input$rate_cl1, input$rate_cl0, input$rate_k1, input$rate_k0,
                          yy, input$rate_conf, input$rate_powerc)), numeric(1))
      ggplot(data.frame(y = y_seq, c = c_seq), aes(y, c)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(y = input$rate_y, c = r$clusters),
                   colour = rose, size = 3.5) +
        labs(x = "Person-years per cluster", y = "Clusters per arm",
             subtitle = "The curve flattens: past a point only more clusters help, not bigger ones.") +
        theme_ss()
    }
  })
  
  output$rate_notes <- renderUI({
    if (input$rate_mode == "estimate") {
      tagList(
        withMathJax(helpText("$$T=\\frac{z^2\\lambda}{d^2}\\times\\text{DEFF}\\div(1-\\text{loss})$$")),
        tags$ul(class = "assump",
                tags$li("Events follow a Poisson process: independent, at a constant rate."),
                tags$li("Person-time is measured, not assumed. Unequal follow-up is fine."),
                tags$li("The normal approximation needs roughly 20 or more expected events."),
                tags$li("No finite population correction here: person-time is not a fixed list.")))
    } else if (input$rate_mode == "compare") {
      tagList(
        withMathJax(helpText("$$T_1=\\frac{(z_{\\alpha/2}+z_\\beta)^2(\\lambda_1+\\lambda_2/r)}
                              {(\\lambda_1-\\lambda_2)^2}$$")),
        tags$ul(class = "assump",
                tags$li("Two independent groups followed over person-time, two-sided test."),
                tags$li("Constant rates over the follow-up period. If the rate changes with
                   time, a survival approach fits better."),
                tags$li("Individuals are independent. For randomised clusters use the
                   cluster mode instead.")))
    } else {
      tagList(
        withMathJax(helpText("$$c=1+(z_{\\alpha/2}+z_\\beta)^2
          \\frac{(\\lambda_1+\\lambda_0)/y+k^2(\\lambda_1^2+\\lambda_0^2)}
          {(\\lambda_0-\\lambda_1)^2}$$")),
        tags$ul(class = "assump",
                tags$li("Clusters are of roughly equal size."),
                tags$li("k, not cluster size, dominates: adding person-years to existing
                   clusters helps far less than adding clusters."),
                tags$li("Always round up. Part of a cluster cannot be randomised."),
                tags$li("Below about four clusters per arm the approximation fails; treat
                   small answers as a floor, not a target.")))
    }
  })
  
  # ===================================================== PROPORTIONS ==========
  prop_res <- reactive({
    req(input$prop_mode)
    if (input$prop_mode == "estimate") {
      req(input$prop_p, input$prop_d, input$prop_conf, input$prop_m)
      est_one_prop(input$prop_p, input$prop_d, input$prop_conf,
                   input$prop_N, input$prop_rho, input$prop_m, input$prop_nr)
    } else if (input$prop_mode == "compare") {
      req(input$prop_p1, input$prop_p2, input$prop_conf, input$prop_power, input$prop_r)
      validate(need(input$prop_p1 != input$prop_p2, "The two proportions must differ."))
      cmp_two_prop(input$prop_p1, input$prop_p2, input$prop_conf,
                   input$prop_power, input$prop_r, input$prop_nr2)
    } else {
      req(input$prop_cp1, input$prop_cp0, input$prop_cm, input$prop_conf, input$prop_powerc)
      validate(need(input$prop_cp1 != input$prop_cp0, "The two proportions must differ."))
      raw <- clus_prop(input$prop_cp1, input$prop_cp0, input$prop_k1, input$prop_k0,
                       input$prop_cm, input$prop_conf, input$prop_powerc)
      list(raw = raw, clusters = ceiling(raw))
    }
  })
  
  output$prop_answer <- renderUI({
    r <- prop_res(); u <- unit_of(input$prop_unit)
    if (input$prop_mode == "estimate") {
      answer_block(fmt(r$final), paste(u, "to sample"),
                   sprintf("Estimates the proportion to within \u00b1%s at %s%% confidence, so the
                 interval would run about %s to %s.",
                           input$prop_d, input$prop_conf * 100,
                           round(input$prop_p - input$prop_d, 3),
                           round(input$prop_p + input$prop_d, 3)))
    } else if (input$prop_mode == "compare") {
      answer_block(fmt(r$n1 + r$n2), paste(u, "in total"),
                   sprintf("%s in group 1 and %s in group 2. Cohen's h = %.2f, a %s effect.",
                           fmt(r$n1), fmt(r$n2), r$h,
                           if (r$h < 0.2) "very small" else if (r$h < 0.5) "small"
                           else if (r$h < 0.8) "medium" else "large"))
    } else {
      answer_block(r$clusters, "clusters per arm",
                   sprintf("%s clusters in total, about %s %s each, so roughly %s %s overall.
                 Unrounded: %.2f.",
                           r$clusters * 2, input$prop_cm, u,
                           fmt(r$clusters * 2 * input$prop_cm), u, r$raw))
    }
  })
  
  output$prop_ladder <- renderUI({
    r <- prop_res(); u <- unit_of(input$prop_unit)
    if (input$prop_mode == "estimate") {
      fpc_detail <- if (is.na(input$prop_N) || input$prop_N <= 0)
        "not applied, population treated as large"
      else sprintf("frame of %s %s", fmt(input$prop_N), u)
      tagList(
        ladder_row("Raw requirement", "z\u00b2p(1-p)/d\u00b2, infinite population", fmt(round(r$n0))),
        ladder_row("After finite population", fpc_detail, fmt(round(r$nfpc))),
        ladder_row("After design effect",
                   sprintf("DEFF = %.2f from m = %s, \u03c1 = %s",
                           r$deff, input$prop_m, input$prop_rho), fmt(round(r$ndes))),
        ladder_row("After non-response",
                   sprintf("divided by %.0f%% response", (1 - input$prop_nr) * 100),
                   fmt(r$final), emphasis = TRUE))
    } else if (input$prop_mode == "compare") {
      tagList(
        ladder_row("Difference to detect",
                   sprintf("%s vs %s, a gap of %s points", input$prop_p1, input$prop_p2,
                           round(abs(input$prop_p1 - input$prop_p2) * 100, 1)),
                   sprintf("h = %.2f", r$h)),
        ladder_row("Raw requirement, group 1",
                   sprintf("%s%% confidence, %s%% power",
                           input$prop_conf * 100, input$prop_power * 100), fmt(round(r$raw))),
        ladder_row("After allocation ratio",
                   sprintf("group 2 is %s\u00d7 group 1", input$prop_r),
                   fmt(round(r$raw * (1 + input$prop_r)))),
        ladder_row("After non-response",
                   sprintf("divided by %.0f%% response", (1 - input$prop_nr2) * 100),
                   fmt(r$n1 + r$n2), emphasis = TRUE))
    } else {
      tagList(
        ladder_row("Difference to detect",
                   sprintf("%s vs %s", input$prop_cp1, input$prop_cp0),
                   sprintf("%.0f%% points", abs(input$prop_cp1 - input$prop_cp0) * 100)),
        ladder_row("Between-cluster variation",
                   sprintf("k\u2081 = %s, k\u2080 = %s", input$prop_k1, input$prop_k0), ""),
        ladder_row("Cluster size", sprintf("%s %s each", input$prop_cm, u), ""),
        ladder_row("Clusters per arm", "rounded up from the formula",
                   r$clusters, emphasis = TRUE))
    }
  })
  
  output$prop_plot <- renderPlot({
    r <- prop_res(); u <- unit_of(input$prop_unit)
    if (input$prop_mode == "estimate") {
      d_seq <- seq(0.02, 0.15, by = 0.002)
      n_seq <- vapply(d_seq, function(dd)
        est_one_prop(input$prop_p, dd, input$prop_conf, input$prop_N,
                     input$prop_rho, input$prop_m, input$prop_nr)$final, numeric(1))
      p <- ggplot(data.frame(d = d_seq, n = n_seq), aes(d, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(d = input$prop_d, n = r$final),
                   colour = rose, size = 3.5) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Margin of error", y = paste(u, "needed"),
             subtitle = "Halving the margin roughly quadruples the sample.") +
        theme_ss()
      if (!is.na(input$prop_N) && input$prop_N > 0)
        p <- p + geom_hline(yintercept = input$prop_N, linetype = "dotted", colour = muted) +
        annotate("text", x = max(d_seq), y = input$prop_N, label = "whole population",
                 hjust = 1, vjust = -0.6, colour = muted, size = 3.2)
      p
    } else if (input$prop_mode == "compare") {
      lo <- max(0.01, min(input$prop_p1, input$prop_p2) - 0.10)
      hi <- min(0.99, max(input$prop_p1, input$prop_p2) + 0.10)
      p2_seq <- seq(lo, hi, length.out = 80)
      p2_seq <- p2_seq[abs(p2_seq - input$prop_p1) > 0.01]
      n_seq <- vapply(p2_seq, function(pp) {
        x <- cmp_two_prop(input$prop_p1, pp, input$prop_conf, input$prop_power,
                          input$prop_r, input$prop_nr2); x$n1 + x$n2 }, numeric(1))
      ggplot(data.frame(p2 = p2_seq, n = n_seq), aes(p2, n)) +
        geom_vline(xintercept = input$prop_p1, linetype = "dotted", colour = muted) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(p2 = input$prop_p2, n = r$n1 + r$n2),
                   colour = rose, size = 3.5) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Proportion in group 2", y = paste("Total", u),
             subtitle = "Cost rises steeply as group 2 approaches group 1 (dotted line).") +
        theme_ss()
    } else {
      m_seq <- seq(10, max(input$prop_cm * 3, 300), length.out = 60)
      c_seq <- vapply(m_seq, function(mm)
        ceiling(clus_prop(input$prop_cp1, input$prop_cp0, input$prop_k1, input$prop_k0,
                          mm, input$prop_conf, input$prop_powerc)), numeric(1))
      ggplot(data.frame(m = m_seq, c = c_seq), aes(m, c)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(m = input$prop_cm, c = r$clusters),
                   colour = rose, size = 3.5) +
        labs(x = paste(u, "per cluster"), y = "Clusters per arm",
             subtitle = "Between-cluster variation sets a floor that cluster size cannot break.") +
        theme_ss()
    }
  })
  
  output$prop_notes <- renderUI({
    if (input$prop_mode == "estimate") {
      tagList(
        withMathJax(helpText("$$n_0=\\frac{z^2p(1-p)}{d^2}\\qquad
                              n=\\frac{n_0}{1+\\frac{n_0-1}{N}}\\times\\text{DEFF}
                              \\div(1-\\text{NR})$$")),
        tags$ul(class = "assump",
                tags$li("Simple random sampling, or random sampling within clusters."),
                tags$li("Normal approximation: unreliable if n\u00b7p or n\u00b7(1-p) is below about 5."),
                tags$li("The sampling frame is complete."),
                tags$li("Non-response is unrelated to the outcome. Inflating the sample fixes
                   precision, never bias.")))
    } else if (input$prop_mode == "compare") {
      tagList(
        withMathJax(helpText("$$n_1=\\frac{\\left[z_{\\alpha/2}\\sqrt{(1+1/r)\\bar p\\bar q}
                              +z_\\beta\\sqrt{p_1q_1+p_2q_2/r}\\right]^2}{(p_1-p_2)^2}$$")),
        tags$ul(class = "assump",
                tags$li("Two independent groups, two-sided test."),
                tags$li("The stated difference is the smallest worth detecting, not the hoped-for one."),
                tags$li("No interim analyses or multiple comparisons; both inflate the requirement.")))
    } else {
      tagList(
        withMathJax(helpText("$$c=1+(z_{\\alpha/2}+z_\\beta)^2
          \\frac{\\pi_1(1-\\pi_1)/m+\\pi_0(1-\\pi_0)/m+k^2(\\pi_1^2+\\pi_0^2)}
          {(\\pi_0-\\pi_1)^2}$$")),
        tags$ul(class = "assump",
                tags$li("Equal cluster sizes and equal numbers of clusters per arm."),
                tags$li("The two proportions must differ or the formula divides by zero."),
                tags$li("Round up to whole clusters."),
                tags$li("Matched or stratified designs need fewer clusters than this.")))
    }
  })
  
  # =========================================================== MEANS ==========
  mean_res <- reactive({
    req(input$mean_mode)
    if (input$mean_mode == "estimate") {
      req(input$mean_sd, input$mean_d, input$mean_conf, input$mean_m)
      est_one_mean(input$mean_sd, input$mean_d, input$mean_conf,
                   input$mean_N, input$mean_rho, input$mean_m, input$mean_nr)
    } else if (input$mean_mode == "compare") {
      req(input$mean_mu1, input$mean_mu2, input$mean_csd,
          input$mean_conf, input$mean_power, input$mean_r)
      validate(need(input$mean_mu1 != input$mean_mu2, "The two means must differ."))
      cmp_two_mean(input$mean_mu1, input$mean_mu2, input$mean_csd,
                   input$mean_conf, input$mean_power, input$mean_r, input$mean_nr2)
    } else {
      req(input$mean_cmu1, input$mean_cmu0, input$mean_cm,
          input$mean_conf, input$mean_powerc)
      validate(need(input$mean_cmu1 != input$mean_cmu0, "The two means must differ."))
      raw <- clus_mean(input$mean_cmu1, input$mean_cmu0, input$mean_s1, input$mean_s0,
                       input$mean_k1, input$mean_k0, input$mean_cm,
                       input$mean_conf, input$mean_powerc)
      list(raw = raw, clusters = ceiling(raw))
    }
  })
  
  output$mean_answer <- renderUI({
    r <- mean_res(); u <- unit_of(input$mean_unit)
    if (input$mean_mode == "estimate") {
      answer_block(fmt(r$final), paste(u, "to sample"),
                   sprintf("Estimates the mean to within \u00b1%s units at %s%% confidence, given a
                 standard deviation of %s.", input$mean_d, input$mean_conf * 100, input$mean_sd))
    } else if (input$mean_mode == "compare") {
      answer_block(fmt(r$n1 + r$n2), paste(u, "in total"),
                   sprintf("%s in group 1 and %s in group 2. Cohen's d = %.2f, a %s effect.",
                           fmt(r$n1), fmt(r$n2), r$d,
                           if (r$d < 0.2) "very small" else if (r$d < 0.5) "small"
                           else if (r$d < 0.8) "medium" else "large"))
    } else {
      answer_block(r$clusters, "clusters per arm",
                   sprintf("%s clusters in total, about %s %s each, so roughly %s %s overall.
                 Unrounded: %.2f.",
                           r$clusters * 2, input$mean_cm, u,
                           fmt(r$clusters * 2 * input$mean_cm), u, r$raw))
    }
  })
  
  output$mean_ladder <- renderUI({
    r <- mean_res(); u <- unit_of(input$mean_unit)
    if (input$mean_mode == "estimate") {
      fpc_detail <- if (is.na(input$mean_N) || input$mean_N <= 0)
        "not applied, population treated as large"
      else sprintf("frame of %s %s", fmt(input$mean_N), u)
      tagList(
        ladder_row("Raw requirement", "z\u00b2\u03c3\u00b2/d\u00b2", fmt(round(r$n0))),
        ladder_row("After finite population", fpc_detail, fmt(round(r$nfpc))),
        ladder_row("After design effect", sprintf("DEFF = %.2f", r$deff), fmt(round(r$ndes))),
        ladder_row("After non-response",
                   sprintf("divided by %.0f%% response", (1 - input$mean_nr) * 100),
                   fmt(r$final), emphasis = TRUE))
    } else if (input$mean_mode == "compare") {
      tagList(
        ladder_row("Difference to detect",
                   sprintf("%s vs %s, SD %s", input$mean_mu1, input$mean_mu2, input$mean_csd),
                   sprintf("d = %.2f", r$d)),
        ladder_row("Raw requirement, group 1",
                   sprintf("%s%% confidence, %s%% power",
                           input$mean_conf * 100, input$mean_power * 100), fmt(round(r$raw))),
        ladder_row("After allocation ratio",
                   sprintf("group 2 is %s\u00d7 group 1", input$mean_r),
                   fmt(round(r$raw * (1 + input$mean_r)))),
        ladder_row("After non-response",
                   sprintf("divided by %.0f%% response", (1 - input$mean_nr2) * 100),
                   fmt(r$n1 + r$n2), emphasis = TRUE))
    } else {
      tagList(
        ladder_row("Difference to detect",
                   sprintf("%s vs %s", input$mean_cmu1, input$mean_cmu0),
                   sprintf("gap = %s", abs(input$mean_cmu1 - input$mean_cmu0))),
        ladder_row("Between-cluster variation",
                   sprintf("k\u2081 = %s, k\u2080 = %s", input$mean_k1, input$mean_k0), ""),
        ladder_row("Cluster size", sprintf("%s %s each", input$mean_cm, u), ""),
        ladder_row("Clusters per arm", "rounded up from the formula",
                   r$clusters, emphasis = TRUE))
    }
  })
  
  output$mean_plot <- renderPlot({
    r <- mean_res(); u <- unit_of(input$mean_unit)
    if (input$mean_mode == "estimate") {
      d_seq <- seq(input$mean_d / 4, input$mean_d * 3, length.out = 60)
      n_seq <- vapply(d_seq, function(dd)
        est_one_mean(input$mean_sd, dd, input$mean_conf, input$mean_N,
                     input$mean_rho, input$mean_m, input$mean_nr)$final, numeric(1))
      ggplot(data.frame(d = d_seq, n = n_seq), aes(d, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(d = input$mean_d, n = r$final),
                   colour = rose, size = 3.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Margin of error (measurement units)", y = paste(u, "needed"),
             subtitle = "Halving the margin roughly quadruples the sample.") +
        theme_ss()
    } else if (input$mean_mode == "compare") {
      d_seq <- seq(0.1, 1.5, by = 0.02)
      n_seq <- vapply(d_seq, function(dd) {
        x <- cmp_two_mean(0, dd * input$mean_csd, input$mean_csd, input$mean_conf,
                          input$mean_power, input$mean_r, input$mean_nr2)
        x$n1 + x$n2 }, numeric(1))
      ggplot(data.frame(d = d_seq, n = n_seq), aes(d, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(d = r$d, n = r$n1 + r$n2),
                   colour = rose, size = 3.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Standardised effect size (Cohen's d)", y = paste("Total", u),
             subtitle = "Small effects are expensive: d of 0.2 costs about 25 times d of 1.0.") +
        theme_ss()
    } else {
      k_seq <- seq(0.02, 0.5, by = 0.01)
      c_seq <- vapply(k_seq, function(kk)
        ceiling(clus_mean(input$mean_cmu1, input$mean_cmu0, input$mean_s1, input$mean_s0,
                          kk, kk, input$mean_cm, input$mean_conf, input$mean_powerc)),
        numeric(1))
      ggplot(data.frame(k = k_seq, c = c_seq), aes(k, c)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(k = input$mean_k1, c = r$clusters),
                   colour = rose, size = 3.5) +
        labs(x = "Coefficient of variation (k)", y = "Clusters per arm",
             subtitle = "k moves the answer more than anything else in this formula.") +
        theme_ss()
    }
  })
  
  output$mean_notes <- renderUI({
    if (input$mean_mode == "estimate") {
      tagList(
        withMathJax(helpText("$$n_0=\\frac{z^2\\sigma^2}{d^2}$$")),
        tags$ul(class = "assump",
                tags$li("The measure is roughly normal, or n is large enough for the central
                   limit theorem."),
                tags$li("\u03c3 is a genuine prior estimate. If it is a guess, rerun across a
                   range of plausible values."),
                tags$li("d is in measurement units, not percentages.")))
    } else if (input$mean_mode == "compare") {
      tagList(
        withMathJax(helpText("$$n_1=\\frac{(1+1/r)(z_{\\alpha/2}+z_\\beta)^2\\sigma^2}
                              {(\\mu_1-\\mu_2)^2}$$")),
        tags$ul(class = "assump",
                tags$li("Both groups share the same standard deviation."),
                tags$li("Normal approximation. Below about 30 per group the t-distribution
                   adds one or two units."),
                tags$li("Observations are independent within each group.")))
    } else {
      tagList(
        withMathJax(helpText("$$c=1+(z_{\\alpha/2}+z_\\beta)^2
          \\frac{(\\sigma_{W1}^2+\\sigma_{W0}^2)/m+k^2(\\mu_1^2+\\mu_0^2)}
          {(\\mu_0-\\mu_1)^2}$$")),
        tags$ul(class = "assump",
                tags$li("Within-cluster SD is the spread among individuals; between-cluster
                   variation is carried by k."),
                tags$li("k is relative to the mean, so it is sensitive to the outcome's scale."),
                tags$li("Round up to whole clusters.")))
    }
  })
}

shinyApp(ui = ui, server = server)