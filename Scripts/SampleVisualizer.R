# =============================================================================
# Sample Size Studio
#
#   Sample size   by outcome type (rate / proportion / mean) x aim
#                 (estimate one group / compare two / cluster randomised)
#   Distributions 17 probability distributions with shaded-tail probabilities
#   Inference     confidence intervals and tests for mu, mu1-mu2, p, p1-p2, sigma^2
#
# Everything runs offline. Every control explains itself, and every answer shows
# the distribution it came from with the relevant area shaded.
#
# Run with: shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# ============================================================ DESIGN TOKENS ===
ink   <- "#0E2233"
paper <- "#F6F8FA"
grid  <- "#DDE7EF"
teal  <- "#0B7285"
rose  <- "#C2255C"
amber <- "#B8860B"
muted <- "#5B7186"

app_theme <- bs_theme(
  version = 5, bg = paper, fg = ink, primary = teal, secondary = muted,
  base_font    = font_google("Source Sans 3"),
  heading_font = font_google("Archivo"),
  code_font    = font_google("IBM Plex Mono")
)

app_css <- sprintf("
  .navbar { border-bottom: 2px solid %s; }
  .navbar-brand { font-weight: 700; letter-spacing: -0.02em; }
  .lede { font-size: 1.02rem; color: %s; max-width: 62ch; line-height: 1.6; }
  .param { margin-bottom: 1.05rem; }
  .param label { font-weight: 600; font-size: 0.9rem; margin-bottom: 0.15rem; }
  .param-help { font-size: 0.78rem; color: %s; line-height: 1.45; margin-top: 0.3rem; }
  .answer { border-left: 4px solid %s; padding: 1rem 1.25rem; background: #fff;
            border-radius: 0 6px 6px 0; }
  .answer-num { font-family: 'IBM Plex Mono', monospace; font-size: 2.6rem;
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
  .stat { display: grid; grid-template-columns: 1.3fr 1fr; gap: 0.5rem;
          padding: 0.45rem 0; border-bottom: 1px solid %s; align-items: baseline; }
  .stat-l { font-size: 0.85rem; color: %s; }
  .stat-v { font-family: 'IBM Plex Mono', monospace; text-align: right; font-size: 0.98rem; }
  .verdict { margin-top: 0.9rem; padding: 0.75rem 0.9rem; background: #fff;
             border-left: 3px solid %s; font-size: 0.87rem; line-height: 1.55; }
",
                   teal, muted, muted, rose, rose, muted, muted, grid, muted, ink, rose,
                   grid, muted, ink, muted, grid, teal, muted, grid, muted, teal)

# ============================================================== UI HELPERS ===
param <- function(id, label, value, help, min = NA, max = NA, step = NA) {
  div(class = "param",
      numericInput(id, label, value = value, min = min, max = max, step = step),
      div(class = "param-help", help))
}
only_when <- function(prefix, mode, ...)
  conditionalPanel(sprintf("input.%s_mode == '%s'", prefix, mode), ...)
ladder_row <- function(step, detail, value, emphasis = FALSE)
  div(class = if (emphasis) "rung rung-final" else "rung",
      div(class = "rung-step", step), div(class = "rung-detail", detail),
      div(class = "rung-value", value))
answer_block <- function(value, unit, reading)
  div(class = "answer", div(class = "answer-num", value),
      div(class = "answer-unit", unit), div(class = "answer-read", reading))
stat_row <- function(l, v) div(class = "stat", div(class = "stat-l", l),
                               div(class = "stat-v", v))
note_card <- function(title, ...) card(card_header(title, class = "note-head"), card_body(...))

theme_ss <- function() {
  theme_minimal(base_size = 13) +
    theme(text = element_text(colour = ink),
          panel.grid.major = element_line(colour = grid, linewidth = 0.4),
          panel.grid.minor = element_blank(),
          axis.title = element_text(colour = muted, size = 11),
          plot.subtitle = element_text(colour = muted, size = 11),
          legend.position = "none")
}

# ================================================== THE SHADING ENGINE ========
# One picture, one idea: the curve is the distribution, the shaded area is the
# probability. Everything visual in this app is built from these two functions.

shade_continuous <- function(dfun, xlim, regions = list(), marks = NULL,
                             xlab = "x", subtitle = NULL) {
  x  <- seq(xlim[1], xlim[2], length.out = 1024)
  df <- data.frame(x = x, y = dfun(x))
  p  <- ggplot(df, aes(x, y))
  for (rg in regions) {
    lo <- max(rg$from, xlim[1]); hi <- min(rg$to, xlim[2])
    if (is.finite(lo) && is.finite(hi) && hi > lo) {
      sx <- seq(lo, hi, length.out = 512)
      p <- p + geom_area(data = data.frame(x = sx, y = dfun(sx)),
                         aes(x, y), fill = rg$fill, alpha = 0.5)
    }
  }
  p <- p + geom_line(linewidth = 0.9, colour = ink)
  for (mk in marks)
    p <- p + geom_vline(xintercept = mk$at, colour = mk$colour,
                        linetype = "dashed", linewidth = 0.6)
  p + labs(x = xlab, y = "Density", subtitle = subtitle) + theme_ss()
}

shade_discrete <- function(support, dvals, keep, xlab = "x", subtitle = NULL) {
  df <- data.frame(x = support, y = dvals, hit = keep)
  ggplot(df, aes(x, y, fill = hit)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = c(`TRUE` = rose, `FALSE` = grid)) +
    labs(x = xlab, y = "Probability", subtitle = subtitle) + theme_ss()
}

# =========================================================== SAMPLE SIZE ======
z_conf  <- function(conf)  qnorm(1 - (1 - conf) / 2)
z_power <- function(power) qnorm(power)
deff_of <- function(m, rho) 1 + (m - 1) * rho
apply_fpc <- function(n0, N) if (is.na(N) || N <= 0) n0 else n0 / (1 + (n0 - 1) / N)

est_one_rate <- function(lambda, d_abs, conf, rho, m, nonresp) {
  T0 <- z_conf(conf)^2 * lambda / d_abs^2
  de <- deff_of(m, rho); Tdes <- T0 * de; Tfin <- Tdes / (1 - nonresp)
  list(T0 = T0, deff = de, Tdes = Tdes, final = ceiling(Tfin),
       events = ceiling(lambda * Tfin))
}
cmp_two_rate <- function(l1, l2, conf, power, r, nonresp) {
  T1 <- (z_conf(conf) + z_power(power))^2 * (l1 + l2 / r) / (l1 - l2)^2
  list(raw = T1, T1 = ceiling(T1 / (1 - nonresp)),
       T2 = ceiling(r * T1 / (1 - nonresp)), rr = l2 / l1)
}
est_one_prop <- function(p, d, conf, N, rho, m, nonresp) {
  n0 <- z_conf(conf)^2 * p * (1 - p) / d^2
  nfpc <- apply_fpc(n0, N); de <- deff_of(m, rho); ndes <- nfpc * de
  list(n0 = n0, nfpc = nfpc, deff = de, ndes = ndes,
       final = ceiling(ndes / (1 - nonresp)))
}
cmp_two_prop <- function(p1, p2, conf, power, r, nonresp) {
  za <- z_conf(conf); zb <- z_power(power)
  pbar <- (p1 + r * p2) / (1 + r)
  n1 <- (za * sqrt((1 + 1/r) * pbar * (1 - pbar)) +
           zb * sqrt(p1 * (1-p1) + p2 * (1-p2) / r))^2 / (p1 - p2)^2
  list(raw = n1, n1 = ceiling(n1 / (1 - nonresp)),
       n2 = ceiling(r * n1 / (1 - nonresp)),
       h = abs(2*asin(sqrt(p1)) - 2*asin(sqrt(p2))))
}
est_one_mean <- function(sd, d, conf, N, rho, m, nonresp) {
  n0 <- z_conf(conf)^2 * sd^2 / d^2
  nfpc <- apply_fpc(n0, N); de <- deff_of(m, rho); ndes <- nfpc * de
  list(n0 = n0, nfpc = nfpc, deff = de, ndes = ndes,
       final = ceiling(ndes / (1 - nonresp)))
}
cmp_two_mean <- function(mu1, mu2, sd, conf, power, r, nonresp) {
  n1 <- (1 + 1/r) * (z_conf(conf) + z_power(power))^2 * sd^2 / (mu1 - mu2)^2
  list(raw = n1, n1 = ceiling(n1 / (1 - nonresp)),
       n2 = ceiling(r * n1 / (1 - nonresp)), d = abs(mu1 - mu2) / sd)
}
clus_rate <- function(l1, l0, k1, k0, y, conf, power)
  1 + (z_conf(conf) + z_power(power))^2 *
  ((l1+l0)/y + k1^2*l1^2 + k0^2*l0^2) / (l0-l1)^2
clus_prop <- function(p1, p0, k1, k0, m, conf, power)
  1 + (z_conf(conf) + z_power(power))^2 *
  (p0*(1-p0)/m + p1*(1-p1)/m + k1^2*p1^2 + k0^2*p0^2) / (p0-p1)^2
clus_mean <- function(mu1, mu0, s1, s0, k1, k0, m, conf, power)
  1 + (z_conf(conf) + z_power(power))^2 *
  ((s0^2+s1^2)/m + k1^2*mu1^2 + k0^2*mu0^2) / (mu0-mu1)^2

# The picture of power: two sampling distributions, alpha in one tail of the
# null, beta the overlap under the alternative. This is the plot students stare
# at until it clicks, and no amount of prose replaces it.
power_picture <- function(se, delta, conf, power_target) {
  zc   <- z_conf(conf)
  crit <- zc * se
  lo <- min(-4*se, delta - 4*se); hi <- max(4*se, delta + 4*se)
  f0 <- function(x) dnorm(x, 0, se)
  f1 <- function(x) dnorm(x, delta, se)
  x  <- seq(lo, hi, length.out = 1024)
  df <- rbind(data.frame(x = x, y = f0(x), g = "H0"),
              data.frame(x = x, y = f1(x), g = "H1"))
  beta_x <- seq(lo, crit, length.out = 512)
  a_hi   <- seq(crit, hi, length.out = 512)
  a_lo   <- seq(lo, -crit, length.out = 512)
  ggplot() +
    geom_area(data = data.frame(x = beta_x, y = f1(beta_x)), aes(x, y),
              fill = amber, alpha = 0.55) +
    geom_area(data = data.frame(x = a_hi, y = f0(a_hi)), aes(x, y),
              fill = rose, alpha = 0.55) +
    geom_area(data = data.frame(x = a_lo, y = f0(a_lo)), aes(x, y),
              fill = rose, alpha = 0.55) +
    geom_line(data = df, aes(x, y, group = g), colour = ink, linewidth = 0.9) +
    geom_vline(xintercept = c(-crit, crit), linetype = "dashed",
               colour = muted, linewidth = 0.6) +
    annotate("text", x = delta, y = f1(delta), label = "under H1: a real effect",
             vjust = -0.8, colour = muted, size = 3.3) +
    annotate("text", x = 0, y = f0(0), label = "under H0: no effect",
             vjust = -0.8, colour = muted, size = 3.3) +
    labs(x = "Observed difference", y = "Density",
         subtitle = sprintf(
           "Rose = alpha (%.0f%% chance of a false positive). Amber = beta (%.0f%% chance of missing a real effect). Dashed = the decision cut-off.",
           (1 - conf) * 100, (1 - power_target) * 100)) +
    theme_ss()
}

# ========================================================== DISTRIBUTIONS =====
# A registry, not seventeen applets. Each entry declares its parameters, its
# d/p/q functions and a plain description. Adding one is four lines.
# Pareto Type I is not in base R, so define it.
dpareto <- function(x, a, m) ifelse(x >= m, a * m^a / x^(a+1), 0)
ppareto <- function(q, a, m) ifelse(q >= m, 1 - (m/q)^a, 0)
qpareto <- function(p, a, m) m / (1 - p)^(1/a)

P <- function(id, label, value, help, min = NA, max = NA, step = NA)
  list(id = id, label = label, value = value, help = help,
       min = min, max = max, step = step)

DISTS <- list(
  # ---- discrete ----
  binom = list(label = "Binomial", type = "discrete",
               about = "Counts successes in a fixed number of independent trials, each with the
             same success probability. Use it for 'how many out of n'.",
               pars = list(P("n", "Number of trials (n)", 10,
                             "How many independent attempts. Fixed in advance.", 1, NA, 1),
                           P("p", "Success probability (p)", 0.5,
                             "Chance of success on any single trial. Constant across trials.",
                             0, 1, 0.01)),
               d = function(x, v) dbinom(x, v[1], v[2]),
               pf = function(q, v) pbinom(q, v[1], v[2]),
               qf = function(pr, v) qbinom(pr, v[1], v[2]),
               sup = function(v) 0:v[1],
               mv = function(v) c(v[1]*v[2], v[1]*v[2]*(1-v[2]))),
  
  geo1 = list(label = "Geometric I (trials until first success)", type = "discrete",
              about = "Counts the trial on which the first success happens, so the smallest
             possible value is 1. Use it for 'how many attempts until it works'.",
              pars = list(P("p", "Success probability (p)", 0.3,
                            "Chance of success on each independent attempt.", 0.0001, 1, 0.01)),
              d = function(x, v) dgeom(x - 1, v[1]),
              pf = function(q, v) pgeom(q - 1, v[1]),
              qf = function(pr, v) qgeom(pr, v[1]) + 1,
              sup = function(v) 1:(qgeom(0.9999, v[1]) + 1),
              mv = function(v) c(1/v[1], (1-v[1])/v[1]^2)),
  
  geo2 = list(label = "Geometric II (failures before first success)", type = "discrete",
              about = "Counts the failures that occur before the first success, so it starts
             at 0. Same process as Geometric I, shifted down by one.",
              pars = list(P("p", "Success probability (p)", 0.3,
                            "Chance of success on each independent attempt.", 0.0001, 1, 0.01)),
              d = function(x, v) dgeom(x, v[1]),
              pf = function(q, v) pgeom(q, v[1]),
              qf = function(pr, v) qgeom(pr, v[1]),
              sup = function(v) 0:qgeom(0.9999, v[1]),
              mv = function(v) c((1-v[1])/v[1], (1-v[1])/v[1]^2)),
  
  hyper = list(label = "Hypergeometric", type = "discrete",
               about = "Counts successes when you draw WITHOUT replacement from a finite
             population, so draws are not independent. The finite-population cousin
             of the binomial.",
               pars = list(P("N", "Population size (N)", 50,
                             "Total items in the population you are drawing from.", 1, NA, 1),
                           P("K", "Successes in population (K)", 10,
                             "How many of those N items count as a success.", 0, NA, 1),
                           P("n", "Number drawn (n)", 5,
                             "Sample size drawn without replacement.", 1, NA, 1)),
               d = function(x, v) dhyper(x, v[2], v[1]-v[2], v[3]),
               pf = function(q, v) phyper(q, v[2], v[1]-v[2], v[3]),
               qf = function(pr, v) qhyper(pr, v[2], v[1]-v[2], v[3]),
               sup = function(v) max(0, v[3]-(v[1]-v[2])):min(v[3], v[2]),
               mv = function(v) { p <- v[2]/v[1]
               c(v[3]*p, v[3]*p*(1-p)*(v[1]-v[3])/(v[1]-1)) }),
  
  nb1 = list(label = "Negative Binomial I (trials until r-th success)", type = "discrete",
             about = "Counts the trial on which the r-th success occurs. Generalises
             Geometric I from one success to r of them.",
             pars = list(P("r", "Successes required (r)", 3,
                           "How many successes you are waiting for.", 1, NA, 1),
                         P("p", "Success probability (p)", 0.3,
                           "Chance of success on each trial.", 0.0001, 1, 0.01)),
             d = function(x, v) dnbinom(x - v[1], v[1], v[2]),
             pf = function(q, v) pnbinom(q - v[1], v[1], v[2]),
             qf = function(pr, v) qnbinom(pr, v[1], v[2]) + v[1],
             sup = function(v) v[1]:(qnbinom(0.9999, v[1], v[2]) + v[1]),
             mv = function(v) c(v[1]/v[2], v[1]*(1-v[2])/v[2]^2)),
  
  nb2 = list(label = "Negative Binomial II (failures before r-th success)", type = "discrete",
             about = "Counts failures accumulated before the r-th success. Same process as
             Negative Binomial I, shifted down by r.",
             pars = list(P("r", "Successes required (r)", 3, "Successes you are waiting for.",
                           1, NA, 1),
                         P("p", "Success probability (p)", 0.3, "Chance of success per trial.",
                           0.0001, 1, 0.01)),
             d = function(x, v) dnbinom(x, v[1], v[2]),
             pf = function(q, v) pnbinom(q, v[1], v[2]),
             qf = function(pr, v) qnbinom(pr, v[1], v[2]),
             sup = function(v) 0:qnbinom(0.9999, v[1], v[2]),
             mv = function(v) c(v[1]*(1-v[2])/v[2], v[1]*(1-v[2])/v[2]^2)),
  
  pois = list(label = "Poisson", type = "discrete",
              about = "Counts events in a fixed window of time or space when events happen
             independently at a constant average rate. The distribution behind
             incidence rates.",
              pars = list(P("lambda", "Mean count (lambda)", 4,
                            "Expected number of events in the window. Also equals the variance.",
                            0.0001, NA, 0.5)),
              d = function(x, v) dpois(x, v[1]),
              pf = function(q, v) ppois(q, v[1]),
              qf = function(pr, v) qpois(pr, v[1]),
              sup = function(v) 0:qpois(0.9999, v[1]),
              mv = function(v) c(v[1], v[1])),
  
  # ---- continuous ----
  norm = list(label = "Normal", type = "continuous",
              about = "The bell curve. Describes measurements clustered symmetrically around a
             mean, and the sampling distribution of most estimators once n is large.",
              pars = list(P("mu", "Mean (mu)", 0, "Centre of the distribution.", NA, NA, 0.5),
                          P("sigma", "Standard deviation (sigma)", 1,
                            "Spread. About 95% of values fall within two of these of the mean.",
                            0.0001, NA, 0.1)),
              d = function(x, v) dnorm(x, v[1], v[2]),
              pf = function(q, v) pnorm(q, v[1], v[2]),
              qf = function(pr, v) qnorm(pr, v[1], v[2]),
              mv = function(v) c(v[1], v[2]^2)),
  
  t = list(label = "Student's t", type = "continuous",
           about = "Like the normal but with heavier tails, reflecting the extra uncertainty
             when the standard deviation is estimated from the data. Approaches the
             normal as degrees of freedom grow.",
           pars = list(P("df", "Degrees of freedom", 10,
                         "Usually n - 1 for a one-sample problem. Small df means fat tails.",
                         1, NA, 1)),
           d = function(x, v) dt(x, v[1]), pf = function(q, v) pt(q, v[1]),
           qf = function(pr, v) qt(pr, v[1]),
           mv = function(v) c(0, if (v[1] > 2) v[1]/(v[1]-2) else NA)),
  
  chisq = list(label = "Chi-square", type = "continuous",
               about = "Sum of squared standard normals. Shows up in variance inference and in
             goodness-of-fit and independence tests. Right-skewed, always positive.",
               pars = list(P("df", "Degrees of freedom", 5,
                             "Often n - 1, or (rows-1)(cols-1) in a contingency table.", 1, NA, 1)),
               d = function(x, v) dchisq(x, v[1]), pf = function(q, v) pchisq(q, v[1]),
               qf = function(pr, v) qchisq(pr, v[1]),
               mv = function(v) c(v[1], 2*v[1])),
  
  f = list(label = "F", type = "continuous",
           about = "Ratio of two scaled chi-squares. The distribution behind ANOVA and any
             test comparing two variances.",
           pars = list(P("df1", "Numerator degrees of freedom", 5, "Often groups - 1.", 1, NA, 1),
                       P("df2", "Denominator degrees of freedom", 20, "Often n - groups.", 1, NA, 1)),
           d = function(x, v) df(x, v[1], v[2]), pf = function(q, v) pf(q, v[1], v[2]),
           qf = function(pr, v) qf(pr, v[1], v[2]),
           # Mean needs df2 > 2, variance needs df2 > 4; below those they do not exist.
           mv = function(v) c(
             if (v[2] > 2) v[2]/(v[2]-2) else NA,
             if (v[2] > 4) 2*v[2]^2*(v[1]+v[2]-2)/(v[1]*(v[2]-2)^2*(v[2]-4)) else NA)),
  
  exp = list(label = "Exponential", type = "continuous",
             about = "Waiting time until the next event when events occur at a constant rate.
             Memoryless: how long you have already waited tells you nothing.",
             pars = list(P("rate", "Rate", 1,
                           "Events per unit time. The mean waiting time is 1 divided by this.",
                           0.0001, NA, 0.1)),
             d = function(x, v) dexp(x, v[1]), pf = function(q, v) pexp(q, v[1]),
             qf = function(pr, v) qexp(pr, v[1]),
             mv = function(v) c(1/v[1], 1/v[1]^2)),
  
  gamma = list(label = "Gamma", type = "continuous",
               about = "Waiting time until the k-th event, and a flexible model for positive
             right-skewed quantities such as costs or durations.",
               pars = list(P("shape", "Shape", 2, "Number of events waited for. Shape 1 is exponential.",
                             0.0001, NA, 0.5),
                           P("rate", "Rate", 1, "Events per unit time.", 0.0001, NA, 0.1)),
               d = function(x, v) dgamma(x, v[1], v[2]), pf = function(q, v) pgamma(q, v[1], v[2]),
               qf = function(pr, v) qgamma(pr, v[1], v[2]),
               mv = function(v) c(v[1]/v[2], v[1]/v[2]^2)),
  
  beta = list(label = "Beta", type = "continuous",
              about = "Lives strictly between 0 and 1, so it models proportions and
             probabilities. The standard prior for a binomial probability.",
              pars = list(P("a", "Shape 1 (alpha)", 2, "Pulls mass toward 1 as it grows.",
                            0.0001, NA, 0.5),
                          P("b", "Shape 2 (beta)", 5, "Pulls mass toward 0 as it grows.",
                            0.0001, NA, 0.5)),
              d = function(x, v) dbeta(x, v[1], v[2]), pf = function(q, v) pbeta(q, v[1], v[2]),
              qf = function(pr, v) qbeta(pr, v[1], v[2]),
              mv = function(v) { a<-v[1]; b<-v[2]
              c(a/(a+b), a*b/((a+b)^2*(a+b+1))) }),
  
  lnorm = list(label = "Log-normal", type = "continuous",
               about = "A quantity whose logarithm is normal. Right-skewed and positive, so it
             fits incomes, concentrations and lengths of stay.",
               pars = list(P("meanlog", "Mean of log(X)", 0,
                             "On the log scale, not the original scale.", NA, NA, 0.1),
                           P("sdlog", "SD of log(X)", 0.5, "On the log scale.", 0.0001, NA, 0.1)),
               d = function(x, v) dlnorm(x, v[1], v[2]), pf = function(q, v) plnorm(q, v[1], v[2]),
               qf = function(pr, v) qlnorm(pr, v[1], v[2]),
               mv = function(v) c(exp(v[1]+v[2]^2/2),
                                  (exp(v[2]^2)-1)*exp(2*v[1]+v[2]^2))),
  
  weibull = list(label = "Weibull", type = "continuous",
                 about = "Time to failure when the hazard changes with age. Shape below 1 means
             failures decline over time, above 1 means wear-out.",
                 pars = list(P("shape", "Shape", 1.5, "Below 1 decreasing hazard, above 1 increasing.",
                               0.0001, NA, 0.1),
                             P("scale", "Scale", 1, "Stretches the time axis.", 0.0001, NA, 0.1)),
                 d = function(x, v) dweibull(x, v[1], v[2]), pf = function(q, v) pweibull(q, v[1], v[2]),
                 qf = function(pr, v) qweibull(pr, v[1], v[2]),
                 mv = function(v) c(v[2]*gamma(1+1/v[1]),
                                    v[2]^2*(gamma(1+2/v[1]) - gamma(1+1/v[1])^2))),
  
  pareto = list(label = "Pareto (Type I)", type = "continuous",
                about = "A heavy-tailed power law with a hard minimum. The classic model for
             wealth, city sizes and insurance claims, where a few huge values dominate.",
                pars = list(P("a", "Shape (alpha)", 3,
                              "Tail heaviness. Smaller means a fatter tail. The mean only exists above 1.",
                              0.0001, NA, 0.1),
                            P("m", "Minimum (x_m)", 1, "The smallest possible value.", 0.0001, NA, 0.1)),
                d = function(x, v) dpareto(x, v[1], v[2]), pf = function(q, v) ppareto(q, v[1], v[2]),
                qf = function(pr, v) qpareto(pr, v[1], v[2]),
                # The heavy tail means the mean only exists for alpha > 1 and the variance
                # only for alpha > 2. That is the point of the distribution, not a defect.
                mv = function(v) c(
                  if (v[1] > 1) v[1]*v[2]/(v[1]-1) else NA,
                  if (v[1] > 2) v[1]*v[2]^2/((v[1]-1)^2*(v[1]-2)) else NA))
)

# ============================================================== INFERENCE =====
# Parse a free-text box of numbers. Anything non-numeric is an error the user
# should see, not something to quietly drop.
parse_data <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(numeric(0))
  bits <- strsplit(trimws(txt), "[,;[:space:]]+")[[1]]
  bits <- bits[nzchar(bits)]
  vals <- suppressWarnings(as.numeric(bits))
  if (any(is.na(vals)))
    stop("These entries are not numbers: ",
         paste(bits[is.na(vals)], collapse = ", "))
  vals
}

# Wilson score interval: behaves properly for small n and extreme p, unlike Wald.
wilson_ci <- function(x, n, conf) {
  z <- z_conf(conf); ph <- x/n
  den <- 1 + z^2/n
  ctr <- (ph + z^2/(2*n)) / den
  hw  <- z * sqrt(ph*(1-ph)/n + z^2/(4*n^2)) / den
  c(ctr - hw, ctr + hw)
}

# ==================================================================== UI ======
outcome_body <- function(prefix) {
  tagList(
    layout_columns(col_widths = c(5, 7),
                   card(card_body(uiOutput(paste0(prefix, "_answer")))),
                   card(card_header("How this number was built", class = "note-head"),
                        card_body(uiOutput(paste0(prefix, "_ladder"))))),
    layout_columns(col_widths = c(7, 5),
                   card(card_header("What drives the number", class = "note-head"),
                        card_body(plotOutput(paste0(prefix, "_plot"), height = "290px"))),
                   card(card_header("Formula and assumptions", class = "note-head"),
                        card_body(uiOutput(paste0(prefix, "_notes"))))),
    card(card_header(uiOutput(paste0(prefix, "_pichead")), class = "note-head"),
         card_body(plotOutput(paste0(prefix, "_power"), height = "300px")))
  )
}
mode_picker <- function(prefix)
  radioButtons(paste0(prefix, "_mode"), "What are you doing?",
               c("Estimate in one group" = "estimate", "Compare two groups" = "compare",
                 "Cluster randomised trial" = "cluster"), selected = "estimate")
error_inputs <- function(prefix) tagList(hr(), h5("Error rates"),
                                         param(paste0(prefix, "_conf"), "Confidence level", 0.95,
                                               "1 minus the significance level. Use 0.95. Entering 0.05 here is the classic slip.",
                                               0.5, 0.999, 0.01),
                                         only_when(prefix, "compare", param(paste0(prefix, "_power"), "Power", 0.80,
                                                                            "Chance of detecting the stated difference if it is real.", 0.5, 0.999, 0.01)),
                                         only_when(prefix, "cluster", param(paste0(prefix, "_powerc"), "Power", 0.80,
                                                                            "Chance of detecting the stated difference if it is real.", 0.5, 0.999, 0.01)))
adjust_inputs <- function(prefix) tagList(hr(), h5("Real-world adjustments"),
                                          param(paste0(prefix, "_N"), "Population size (N)", 0,
                                                "Size of the full list you sample from. Small N reduces the sample needed.
         Leave at 0 if the population is large or unknown.", 0, NA, 1),
                                          param(paste0(prefix, "_rho"), "Intracluster correlation (rho)", 0,
                                                "0 for a simple random sample. 0.05 to 0.20 when sampling within
         villages, clinics or schools.", 0, 1, 0.01),
                                          param(paste0(prefix, "_m"), "Units per cluster (m)", 1,
                                                "Units taken from each cluster. Leave at 1 if not clustered.", 1, NA, 1),
                                          param(paste0(prefix, "_nr"), "Expected non-response", 0,
                                                "Share you expect to lose. The sample is inflated to absorb it.", 0, 0.9, 0.01))
unit_input <- function(prefix, default)
  textInput(paste0(prefix, "_unit"), "What are you counting?", value = default)

infer_body <- function(prefix) tagList(
  layout_columns(col_widths = c(5, 7),
                 card(card_body(uiOutput(paste0(prefix, "_answer")))),
                 card(card_header("The numbers behind it", class = "note-head"),
                      card_body(uiOutput(paste0(prefix, "_stats"))))),
  card(card_header("The test, drawn", class = "note-head"),
       card_body(plotOutput(paste0(prefix, "_plot"), height = "300px"))),
  card(card_header("What this means", class = "note-head"),
       card_body(uiOutput(paste0(prefix, "_notes"))))
)
alt_picker <- function(prefix)
  radioButtons(paste0(prefix, "_alt"), "Alternative hypothesis",
               c("Two-sided (\u2260)" = "two.sided", "Greater (>)" = "greater",
                 "Less (<)" = "less"), selected = "two.sided")

ui <- page_navbar(
  title = "Sample Size Studio", theme = app_theme,
  header = tagList(withMathJax(), tags$style(HTML(app_css))),
  
  nav_panel("Start here", layout_columns(col_widths = c(7, 5),
                                         card(card_body(
                                           h3("Three tools, one place"),
                                           p(class = "lede",
                                             "Sample size works out how big a study needs to be. Distributions shows any
         probability as a shaded area under a curve. Inference takes data you already
         have and returns an interval or a test. Nothing here needs an internet
         connection once the app is open."),
                                           h5("Choosing a sample size calculator"),
                                           p(class = "lede",
                                             "Two questions decide it. What kind of number is your outcome, and are you
         estimating it in one group, comparing two, or randomising clusters? Each
         sample size tab is one outcome type, with the three aims as a switch inside."),
                                           tags$table(class = "matrix",
                                                      tags$tr(tags$th(""), tags$th("Estimate in one group"),
                                                              tags$th("Compare two groups"), tags$th("Cluster randomised")),
                                                      tags$tr(tags$td("Rate"), tags$td("Person-time to pin down an incidence rate"),
                                                              tags$td("Person-time per arm for a rate ratio"),
                                                              tags$td("Clusters per arm")),
                                                      tags$tr(tags$td("Proportion"), tags$td("Sample to estimate a prevalence"),
                                                              tags$td("Sample per arm for a difference in percentages"),
                                                              tags$td("Clusters per arm")),
                                                      tags$tr(tags$td("Mean"), tags$td("Sample to estimate an average"),
                                                              tags$td("Sample per arm for a difference in averages"),
                                                              tags$td("Clusters per arm"))),
                                           br(),
                                           h5("Estimating is not testing"),
                                           p(class = "lede",
                                             "Want a number with an interval around it? You are estimating: supply a margin
         of error, and power never enters. Want to detect a difference? You are
         testing: supply the smallest difference worth finding, and power does.
         Choosing wrong is the most common error in a protocol."),
                                           h5("Rates are not proportions"),
                                           p(class = "lede",
                                             "A proportion is a share of a fixed denominator of people. A rate is events
         per unit of person-time, so people contribute different amounts of follow-up.
         If your denominator is person-years, you need the rate tab.")
                                         )),
                                         card(card_header("Every term used here", class = "note-head"), card_body(
                                           div(class = "term", div(class = "term-name", "Confidence level ",
                                                                   span(class = "term-sym", "(1 - alpha)")),
                                               div(class = "term-def", "How often the interval would cover the truth on repeated
            sampling. Use 0.95. One minus the significance level.")),
                                           div(class = "term", div(class = "term-name", "Power ",
                                                                   span(class = "term-sym", "(1 - beta)")),
                                               div(class = "term-def", "Chance of detecting a real difference of the size you
            specified. 0.80 by convention. Applies only when testing.")),
                                           div(class = "term", div(class = "term-name", "Margin of error ",
                                                                   span(class = "term-sym", "(d)")),
                                               div(class = "term-def", "Half the width of your interval. The strongest lever on
            sample size: halving it roughly quadruples n.")),
                                           div(class = "term", div(class = "term-name", "p-value"),
                                               div(class = "term-def", "Probability of data at least this extreme if the null were
            true. It is not the probability the null is true, and not a measure of effect
            size.")),
                                           div(class = "term", div(class = "term-name", "Population size ",
                                                                   span(class = "term-sym", "(N)")),
                                               div(class = "term-def", "Size of the list you sample from. Small N pulls the
            requirement down. Set 0 to ignore.")),
                                           div(class = "term", div(class = "term-name", "Design effect ",
                                                                   span(class = "term-sym", "(DEFF)")),
                                               div(class = "term-def", "Penalty for cluster sampling, 1 + (m - 1)rho. DEFF of 1.5
            means 50% more units for the same precision.")),
                                           div(class = "term", div(class = "term-name", "Coefficient of variation ",
                                                                   span(class = "term-sym", "(k)")),
                                               div(class = "term-def", "Hayes & Bennett's between-cluster variation, typically
            0.15 to 0.30. Drives cluster count more than cluster size does.")),
                                           div(class = "term", div(class = "term-name", "Effect size ",
                                                                   span(class = "term-sym", "(h, d, RR)")),
                                               div(class = "term-def", "The difference on a scale-free scale so studies compare.
            Cohen's h for proportions, d for means, rate ratio for rates."))
                                         )))),
  
  # ------------------------------------------------------- sample size tabs
  nav_menu("Sample size",
           nav_panel("Rates", layout_sidebar(sidebar = sidebar(width = 340,
                                                               mode_picker("rate"),
                                                               p(class = "param-help", "Rates are events per unit of person-time, so the answer
         is person-time, not a headcount."),
                                                               only_when("rate", "estimate",
                                                                         h5("The rate you expect"),
                                                                         param("rate_lam", "Expected rate (lambda)", 0.02,
                                                                               "Events per person-year, from surveillance or an earlier study.", 0, NA, 0.005),
                                                                         radioButtons("rate_prec", "Precision you need",
                                                                                      c("Relative (\u00b1% of the rate)" = "rel", "Absolute (\u00b1 fixed amount)" = "abs"),
                                                                                      selected = "rel"),
                                                                         conditionalPanel("input.rate_prec == 'rel'",
                                                                                          param("rate_eps", "Relative precision", 0.20,
                                                                                                "0.20 means the interval runs 20% below to 20% above the rate. The
                 sensible choice for rare events.", 0.01, 1, 0.01)),
                                                                         conditionalPanel("input.rate_prec == 'abs'",
                                                                                          param("rate_d", "Absolute margin (d)", 0.005,
                                                                                                "In events per person-year, same units as the rate.", 0.0001, NA, 0.001))),
                                                               only_when("rate", "compare",
                                                                         h5("The rates you expect"),
                                                                         param("rate_l1", "Rate, group 1 (lambda 1)", 0.010,
                                                                               "Events per person-year in the first group.", 0, NA, 0.005),
                                                                         param("rate_l2", "Rate, group 2 (lambda 2)", 0.020,
                                                                               "Events per person-year in the second group.", 0, NA, 0.005),
                                                                         param("rate_r", "Allocation ratio", 1,
                                                                               "Person-time in group 2 relative to group 1.", 0.1, 10, 0.1)),
                                                               only_when("rate", "cluster",
                                                                         h5("True event rates"),
                                                                         param("rate_cl1", "Rate with intervention", 0.010, "Per person-year.", 0, NA, 0.001),
                                                                         param("rate_cl0", "Rate in control", 0.020, "Per person-year.", 0, NA, 0.001),
                                                                         h5("Between-cluster variation"),
                                                                         param("rate_k1", "k (intervention)", 0.25, "Typically 0.15 to 0.30.", 0, NA, 0.01),
                                                                         param("rate_k0", "k (control)", 0.25, "Usually equal to the other k.", 0, NA, 0.01),
                                                                         h5("Cluster size"),
                                                                         param("rate_y", "Person-years per cluster", 1000, "Follow-up in one cluster.",
                                                                               1, NA, 10)),
                                                               only_when("rate", "estimate", hr(), h5("Real-world adjustments"),
                                                                         param("rate_rho", "Intracluster correlation (rho)", 0, "0 if not clustered.", 0, 1, 0.01),
                                                                         param("rate_m", "Units per cluster (m)", 1, "Leave at 1 if not clustered.", 1, NA, 1),
                                                                         param("rate_nr", "Expected loss to follow-up", 0, "Share of person-time lost.",
                                                                               0, 0.9, 0.01)),
                                                               only_when("rate", "compare", hr(), h5("Real-world adjustments"),
                                                                         param("rate_nr2", "Expected loss to follow-up", 0, "Inflates both groups.",
                                                                               0, 0.9, 0.01)),
                                                               error_inputs("rate")), outcome_body("rate"))),
           
           nav_panel("Proportions", layout_sidebar(sidebar = sidebar(width = 340,
                                                                     mode_picker("prop"), unit_input("prop", "participants"),
                                                                     only_when("prop", "estimate", h5("What you are estimating"),
                                                                               param("prop_p", "Expected proportion (p)", 0.5,
                                                                                     "Best guess at the true share. 0.5 needs the largest sample, so it is the
               safe default with no prior.", 0, 1, 0.01),
                                                                               param("prop_d", "Margin of error (d)", 0.05,
                                                                                     "Half-width of the interval. 0.05 gives plus or minus 5 percentage points.",
                                                                                     0.005, 0.5, 0.005)),
                                                                     only_when("prop", "compare", h5("The difference worth detecting"),
                                                                               param("prop_p1", "Proportion, group 1", 0.30, "Expected share in group 1.", 0, 1, 0.01),
                                                                               param("prop_p2", "Proportion, group 2", 0.50,
                                                                                     "State the smallest gap that would change a decision.", 0, 1, 0.01),
                                                                               param("prop_r", "Allocation ratio", 1, "1 means equal groups.", 0.1, 10, 0.1)),
                                                                     only_when("prop", "cluster", h5("True proportions"),
                                                                               param("prop_cp1", "Proportion with intervention", 0.10, "Intervention arm.", 0, 1, 0.01),
                                                                               param("prop_cp0", "Proportion in control", 0.20, "Control arm.", 0, 1, 0.01),
                                                                               h5("Between-cluster variation"),
                                                                               param("prop_k1", "k (intervention)", 0.25, "Typically 0.15 to 0.30.", 0, NA, 0.01),
                                                                               param("prop_k0", "k (control)", 0.25, "Usually equal.", 0, NA, 0.01),
                                                                               h5("Cluster size"),
                                                                               param("prop_cm", "Individuals per cluster", 100, "People per cluster.", 1, NA, 10)),
                                                                     only_when("prop", "estimate", adjust_inputs("prop")),
                                                                     only_when("prop", "compare", hr(), h5("Real-world adjustments"),
                                                                               param("prop_nr2", "Expected non-response", 0, "Inflates both groups.", 0, 0.9, 0.01)),
                                                                     error_inputs("prop")), outcome_body("prop"))),
           
           nav_panel("Means", layout_sidebar(sidebar = sidebar(width = 340,
                                                               mode_picker("mean"), unit_input("mean", "participants"),
                                                               only_when("mean", "estimate", h5("What you are estimating"),
                                                                         param("mean_sd", "Expected standard deviation", 10,
                                                                               "Spread in the population, from a pilot or roughly the range over four.",
                                                                               0.001, NA, 0.5),
                                                                         param("mean_d", "Margin of error (d)", 2,
                                                                               "How close the estimate must be, in measurement units.", 0.001, NA, 0.5)),
                                                               only_when("mean", "compare", h5("The difference worth detecting"),
                                                                         param("mean_mu1", "Mean, group 1", 10, "Expected average in group 1.", NA, NA, 0.5),
                                                                         param("mean_mu2", "Mean, group 2", 12, "Expected average in group 2.", NA, NA, 0.5),
                                                                         param("mean_csd", "Common standard deviation", 5, "Assumed equal in both groups.",
                                                                               0.001, NA, 0.5),
                                                                         param("mean_r", "Allocation ratio", 1, "1 means equal groups.", 0.1, 10, 0.1)),
                                                               only_when("mean", "cluster", h5("True means and within-cluster spread"),
                                                                         param("mean_cmu1", "Mean with intervention", 10, "", NA, NA, 0.5),
                                                                         param("mean_cmu0", "Mean in control", 12, "", NA, NA, 0.5),
                                                                         param("mean_s1", "Within-cluster SD, intervention", 5,
                                                                               "Spread among individuals inside a cluster.", 0.001, NA, 0.5),
                                                                         param("mean_s0", "Within-cluster SD, control", 5, "", 0.001, NA, 0.5),
                                                                         h5("Between-cluster variation"),
                                                                         param("mean_k1", "k (intervention)", 0.15, "Typically 0.10 to 0.25 for means.",
                                                                               0, NA, 0.01),
                                                                         param("mean_k0", "k (control)", 0.15, "Usually equal.", 0, NA, 0.01),
                                                                         h5("Cluster size"),
                                                                         param("mean_cm", "Individuals per cluster", 50, "", 1, NA, 5)),
                                                               only_when("mean", "estimate", adjust_inputs("mean")),
                                                               only_when("mean", "compare", hr(), h5("Real-world adjustments"),
                                                                         param("mean_nr2", "Expected non-response", 0, "Inflates both groups.", 0, 0.9, 0.01)),
                                                               error_inputs("mean")), outcome_body("mean")))
  ),
  
  # ------------------------------------------------------------ distributions
  nav_panel("Distributions", layout_sidebar(sidebar = sidebar(width = 340,
                                                              selectInput("dist_which", "Distribution",
                                                                          choices = setNames(names(DISTS), vapply(DISTS, `[[`, "", "label"))),
                                                              div(class = "param-help", textOutput("dist_about")),
                                                              hr(), h5("Parameters"), uiOutput("dist_params"),
                                                              hr(), h5("Probability to compute"),
                                                              radioButtons("dist_q", NULL,
                                                                           c("P(X \u2264 x) lower tail"    = "le",
                                                                             "P(X \u2265 x) upper tail"    = "ge",
                                                                             "P(a \u2264 X \u2264 b) between" = "between",
                                                                             "P(X = x) exactly"          = "eq",
                                                                             "Find x for a given probability" = "quant"), selected = "le"),
                                                              conditionalPanel("input.dist_q == 'le' || input.dist_q == 'ge' || input.dist_q == 'eq'",
                                                                               param("dist_x", "x", 5, "The cut-off you are asking about.", NA, NA, 0.5)),
                                                              conditionalPanel("input.dist_q == 'between'",
                                                                               param("dist_a", "a (lower)", 2, "Lower edge, included.", NA, NA, 0.5),
                                                                               param("dist_b", "b (upper)", 8, "Upper edge, included.", NA, NA, 0.5)),
                                                              conditionalPanel("input.dist_q == 'quant'",
                                                                               param("dist_p", "Cumulative probability", 0.95,
                                                                                     "Returns the x with this much probability at or below it. 0.975 gives the
             familiar 1.96 on a standard normal.", 0.0001, 0.9999, 0.005))),
                                            tagList(
                                              layout_columns(col_widths = c(5, 7),
                                                             card(card_body(uiOutput("dist_answer"))),
                                                             card(card_header("About this distribution", class = "note-head"),
                                                                  card_body(uiOutput("dist_notes")))),
                                              card(card_header("The distribution, with your region shaded", class = "note-head"),
                                                   card_body(plotOutput("dist_plot", height = "340px"))))
  )),
  
  # --------------------------------------------------------------- inference
  nav_menu("Inference",
           nav_panel("Mean (mu)", layout_sidebar(sidebar = sidebar(width = 340,
                                                                   p(class = "param-help", "One-sample t procedures: a confidence interval for a
         population mean, and a test against a value you name."),
                                                                   radioButtons("im_src", "Data entry", c("Type raw data" = "raw",
                                                                                                          "Enter summary statistics" = "sum"), selected = "raw"),
                                                                   conditionalPanel("input.im_src == 'raw'",
                                                                                    textAreaInput("im_data", "Your observations",
                                                                                                  "12.1 13.4 11.8 14.2 12.9 13.1 12.4 13.8", height = "90px"),
                                                                                    div(class = "param-help", "Separate with spaces, commas or new lines.")),
                                                                   conditionalPanel("input.im_src == 'sum'",
                                                                                    param("im_n", "Sample size (n)", 25, "Number of observations.", 2, NA, 1),
                                                                                    param("im_xbar", "Sample mean", 13.0, "Average of your observations.", NA, NA, 0.1),
                                                                                    param("im_s", "Sample SD", 1.2, "Standard deviation of your observations.",
                                                                                          0.0001, NA, 0.1)),
                                                                   hr(),
                                                                   param("im_mu0", "Null value (mu0)", 12.0,
                                                                         "The mean you are testing against. The test asks whether your data are
             consistent with this.", NA, NA, 0.1),
                                                                   alt_picker("im"),
                                                                   param("im_conf", "Confidence level", 0.95, "For the interval.", 0.5, 0.999, 0.01)),
                                                 infer_body("im"))),
           
           nav_panel("Difference of means", layout_sidebar(sidebar = sidebar(width = 340,
                                                                             p(class = "param-help", "Two-sample t procedures for the difference between two
         independent group means."),
                                                                             textAreaInput("id_g1", "Group 1 observations",
                                                                                           "12.1 13.4 11.8 14.2 12.9 13.1", height = "80px"),
                                                                             textAreaInput("id_g2", "Group 2 observations",
                                                                                           "14.5 15.1 13.9 16.0 14.8 15.3", height = "80px"),
                                                                             div(class = "param-help", "Separate with spaces, commas or new lines."),
                                                                             hr(),
                                                                             radioButtons("id_var", "Variances",
                                                                                          c("Do not assume equal (Welch)" = "welch", "Assume equal (pooled)" = "pooled"),
                                                                                          selected = "welch"),
                                                                             div(class = "param-help", "Welch is the safer default and costs almost nothing."),
                                                                             alt_picker("id"),
                                                                             param("id_conf", "Confidence level", 0.95, "For the interval.", 0.5, 0.999, 0.01)),
                                                           infer_body("id"))),
           
           nav_panel("Proportion (p)", layout_sidebar(sidebar = sidebar(width = 340,
                                                                        p(class = "param-help", "Inference for a single population proportion."),
                                                                        param("ip_x", "Successes (x)", 42, "How many in your sample had the outcome.",
                                                                              0, NA, 1),
                                                                        param("ip_n", "Sample size (n)", 100, "Total observed.", 1, NA, 1),
                                                                        hr(),
                                                                        param("ip_p0", "Null value (p0)", 0.5,
                                                                              "The proportion you are testing against.", 0, 1, 0.01),
                                                                        alt_picker("ip"),
                                                                        param("ip_conf", "Confidence level", 0.95, "For the interval.", 0.5, 0.999, 0.01)),
                                                      infer_body("ip"))),
           
           nav_panel("Difference of proportions", layout_sidebar(sidebar = sidebar(width = 340,
                                                                                   p(class = "param-help", "Inference for the difference between two independent
         proportions."),
                                                                                   param("iq_x1", "Successes, group 1", 30, "Count with the outcome.", 0, NA, 1),
                                                                                   param("iq_n1", "Sample size, group 1", 100, "Total in group 1.", 1, NA, 1),
                                                                                   param("iq_x2", "Successes, group 2", 45, "Count with the outcome.", 0, NA, 1),
                                                                                   param("iq_n2", "Sample size, group 2", 100, "Total in group 2.", 1, NA, 1),
                                                                                   hr(), alt_picker("iq"),
                                                                                   param("iq_conf", "Confidence level", 0.95, "For the interval.", 0.5, 0.999, 0.01)),
                                                                 infer_body("iq"))),
           
           nav_panel("Variance (sigma squared)", layout_sidebar(sidebar = sidebar(width = 340,
                                                                                  p(class = "param-help", "Chi-square procedures for a population variance or
         standard deviation. Very sensitive to non-normality."),
                                                                                  param("iv_n", "Sample size (n)", 25, "Number of observations.", 2, NA, 1),
                                                                                  param("iv_s", "Sample SD (s)", 1.2, "Standard deviation of your observations.",
                                                                                        0.0001, NA, 0.1),
                                                                                  hr(),
                                                                                  param("iv_s0", "Null SD (sigma 0)", 1.0,
                                                                                        "The standard deviation you are testing against.", 0.0001, NA, 0.1),
                                                                                  alt_picker("iv"),
                                                                                  param("iv_conf", "Confidence level", 0.95, "For the interval.", 0.5, 0.999, 0.01)),
                                                                infer_body("iv")))
  ),
  
  nav_spacer(),
  nav_item(tags$a("Hayes & Bennett (1999)",
                  href = "https://doi.org/10.1093/ije/28.2.319", target = "_blank", class = "nav-link"))
)

# ================================================================ SERVER ======
server <- function(input, output, session) {
  
  fmt <- function(x) format(x, big.mark = ",")
  sig <- function(x, k = 4) formatC(x, digits = k, format = "g")
  unit_of <- function(t) if (is.null(t) || !nzchar(trimws(t))) "units" else trimws(t)
  pfmt <- function(p) if (p < 0.0001) "< 0.0001" else sig(p, 4)
  
  # ============================================================= SAMPLE SIZE
  rate_res <- reactive({
    req(input$rate_mode)
    if (input$rate_mode == "estimate") {
      req(input$rate_lam, input$rate_conf, input$rate_m)
      validate(need(input$rate_lam > 0, "The expected rate must be above zero."))
      d_abs <- if (input$rate_prec == "rel") input$rate_eps * input$rate_lam else input$rate_d
      validate(need(!is.null(d_abs) && d_abs > 0, "Precision must be above zero."))
      est_one_rate(input$rate_lam, d_abs, input$rate_conf, input$rate_rho,
                   input$rate_m, input$rate_nr)
    } else if (input$rate_mode == "compare") {
      req(input$rate_l1, input$rate_l2, input$rate_conf, input$rate_power, input$rate_r)
      validate(need(input$rate_l1 != input$rate_l2, "The two rates must differ."))
      cmp_two_rate(input$rate_l1, input$rate_l2, input$rate_conf, input$rate_power,
                   input$rate_r, input$rate_nr2)
    } else {
      req(input$rate_cl1, input$rate_cl0, input$rate_y, input$rate_conf, input$rate_powerc)
      validate(need(input$rate_cl1 != input$rate_cl0, "The two rates must differ."))
      raw <- clus_rate(input$rate_cl1, input$rate_cl0, input$rate_k1, input$rate_k0,
                       input$rate_y, input$rate_conf, input$rate_powerc)
      list(raw = raw, clusters = ceiling(raw))
    }
  })
  prop_res <- reactive({
    req(input$prop_mode)
    if (input$prop_mode == "estimate") {
      req(input$prop_p, input$prop_d, input$prop_conf, input$prop_m)
      est_one_prop(input$prop_p, input$prop_d, input$prop_conf, input$prop_N,
                   input$prop_rho, input$prop_m, input$prop_nr)
    } else if (input$prop_mode == "compare") {
      req(input$prop_p1, input$prop_p2, input$prop_conf, input$prop_power, input$prop_r)
      validate(need(input$prop_p1 != input$prop_p2, "The two proportions must differ."))
      cmp_two_prop(input$prop_p1, input$prop_p2, input$prop_conf, input$prop_power,
                   input$prop_r, input$prop_nr2)
    } else {
      req(input$prop_cp1, input$prop_cp0, input$prop_cm, input$prop_conf, input$prop_powerc)
      validate(need(input$prop_cp1 != input$prop_cp0, "The two proportions must differ."))
      raw <- clus_prop(input$prop_cp1, input$prop_cp0, input$prop_k1, input$prop_k0,
                       input$prop_cm, input$prop_conf, input$prop_powerc)
      list(raw = raw, clusters = ceiling(raw))
    }
  })
  mean_res <- reactive({
    req(input$mean_mode)
    if (input$mean_mode == "estimate") {
      req(input$mean_sd, input$mean_d, input$mean_conf, input$mean_m)
      est_one_mean(input$mean_sd, input$mean_d, input$mean_conf, input$mean_N,
                   input$mean_rho, input$mean_m, input$mean_nr)
    } else if (input$mean_mode == "compare") {
      req(input$mean_mu1, input$mean_mu2, input$mean_csd, input$mean_conf,
          input$mean_power, input$mean_r)
      validate(need(input$mean_mu1 != input$mean_mu2, "The two means must differ."))
      cmp_two_mean(input$mean_mu1, input$mean_mu2, input$mean_csd, input$mean_conf,
                   input$mean_power, input$mean_r, input$mean_nr2)
    } else {
      req(input$mean_cmu1, input$mean_cmu0, input$mean_cm, input$mean_conf, input$mean_powerc)
      validate(need(input$mean_cmu1 != input$mean_cmu0, "The two means must differ."))
      raw <- clus_mean(input$mean_cmu1, input$mean_cmu0, input$mean_s1, input$mean_s0,
                       input$mean_k1, input$mean_k0, input$mean_cm,
                       input$mean_conf, input$mean_powerc)
      list(raw = raw, clusters = ceiling(raw))
    }
  })
  
  # ---- Rates outputs ----
  output$rate_answer <- renderUI({
    r <- rate_res()
    if (input$rate_mode == "estimate")
      answer_block(fmt(r$final), "person-years of observation",
                   sprintf("You would expect about %s events over that follow-up. Estimating a
                 rate is really about accumulating events; person-time is how you buy them.",
                           fmt(r$events)))
    else if (input$rate_mode == "compare")
      answer_block(fmt(r$T1 + r$T2), "person-years in total",
                   sprintf("%s in group 1 and %s in group 2, to detect a rate ratio of %.2f.",
                           fmt(r$T1), fmt(r$T2), r$rr))
    else
      answer_block(r$clusters, "clusters per arm",
                   sprintf("%s clusters in total, each about %s person-years. Unrounded: %.2f.",
                           r$clusters * 2, fmt(input$rate_y), r$raw))
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
                   sprintf("DEFF = %.2f from m = %s, rho = %s", r$deff, input$rate_m, input$rate_rho),
                   fmt(round(r$Tdes))),
        ladder_row("After loss to follow-up",
                   sprintf("divided by %.0f%% retained", (1 - input$rate_nr) * 100),
                   fmt(r$final), emphasis = TRUE),
        div(style = "margin-top:0.8rem;font-size:0.82rem;color:#5B7186;",
            sprintf("Expected events: %s. Under relative precision the events needed are
                   fixed by the margin alone, so a rarer outcome simply costs more
                   person-time to reach the same count.", fmt(r$events))))
    } else if (input$rate_mode == "compare") {
      tagList(
        ladder_row("Effect to detect",
                   sprintf("%s vs %s per person-year", input$rate_l1, input$rate_l2),
                   sprintf("RR = %.2f", r$rr)),
        ladder_row("Raw requirement, group 1",
                   sprintf("%s%% confidence, %s%% power", input$rate_conf*100, input$rate_power*100),
                   fmt(round(r$raw))),
        ladder_row("After allocation ratio",
                   sprintf("group 2 is %s times group 1", input$rate_r),
                   fmt(round(r$raw * (1 + input$rate_r)))),
        ladder_row("After loss to follow-up",
                   sprintf("divided by %.0f%% retained", (1 - input$rate_nr2) * 100),
                   fmt(r$T1 + r$T2), emphasis = TRUE))
    } else {
      tagList(
        ladder_row("Effect to detect",
                   sprintf("%s vs %s per person-year", input$rate_cl1, input$rate_cl0),
                   sprintf("RR = %.2f", input$rate_cl1 / input$rate_cl0)),
        ladder_row("Between-cluster variation",
                   sprintf("k1 = %s, k0 = %s", input$rate_k1, input$rate_k0), ""),
        ladder_row("Cluster size", sprintf("%s person-years each", fmt(input$rate_y)), ""),
        ladder_row("Clusters per arm", "rounded up from the formula",
                   r$clusters, emphasis = TRUE))
    }
  })
  output$rate_plot <- renderPlot({
    r <- rate_res()
    if (input$rate_mode == "estimate") {
      eps <- seq(0.05, 0.5, by = 0.01)
      ev  <- ceiling(z_conf(input$rate_conf)^2 / eps^2)
      here <- if (input$rate_prec == "rel") input$rate_eps else input$rate_d / input$rate_lam
      ggplot(data.frame(e = eps, n = ev), aes(e, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(e = here, n = ceiling(z_conf(input$rate_conf)^2/here^2)),
                   colour = rose, size = 3.5) +
        scale_x_continuous(labels = scales::percent) +
        labs(x = "Relative precision", y = "Events needed",
             subtitle = "Events needed depend only on precision, never on how rare the outcome is.") +
        theme_ss()
    } else if (input$rate_mode == "compare") {
      lo <- max(1e-4, min(input$rate_l1, input$rate_l2) * 0.4)
      hi <- max(input$rate_l1, input$rate_l2) * 1.8
      ls <- seq(lo, hi, length.out = 80); ls <- ls[abs(ls - input$rate_l1) > input$rate_l1*0.05]
      tot <- vapply(ls, function(l) { x <- cmp_two_rate(input$rate_l1, l, input$rate_conf,
                                                        input$rate_power, input$rate_r, input$rate_nr2); x$T1 + x$T2 }, numeric(1))
      ggplot(data.frame(l = ls, n = tot), aes(l, n)) +
        geom_vline(xintercept = input$rate_l1, linetype = "dotted", colour = muted) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(l = input$rate_l2, n = r$T1 + r$T2),
                   colour = rose, size = 3.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Rate in group 2", y = "Total person-years",
             subtitle = "Cost explodes as group 2 approaches group 1 (dotted line).") +
        theme_ss()
    } else {
      ys <- seq(input$rate_y/5, input$rate_y*3, length.out = 60)
      cs <- vapply(ys, function(y) ceiling(clus_rate(input$rate_cl1, input$rate_cl0,
                                                     input$rate_k1, input$rate_k0, y, input$rate_conf, input$rate_powerc)), numeric(1))
      ggplot(data.frame(y = ys, c = cs), aes(y, c)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(y = input$rate_y, c = r$clusters),
                   colour = rose, size = 3.5) +
        labs(x = "Person-years per cluster", y = "Clusters per arm",
             subtitle = "The curve flattens: past a point only more clusters help.") +
        theme_ss()
    }
  })
  output$rate_notes <- renderUI({
    if (input$rate_mode == "estimate") tagList(
      withMathJax(helpText("$$T=\\frac{z^2\\lambda}{d^2}\\times\\text{DEFF}\\div(1-\\text{loss})$$")),
      tags$ul(class = "assump",
              tags$li("Events are Poisson: independent, at a constant rate."),
              tags$li("Person-time is measured, not assumed. Unequal follow-up is fine."),
              tags$li("The approximation needs roughly 20 or more expected events."),
              tags$li("No finite population correction: person-time is not a fixed list.")))
    else if (input$rate_mode == "compare") tagList(
      withMathJax(helpText("$$T_1=\\frac{(z_{\\alpha/2}+z_\\beta)^2(\\lambda_1+\\lambda_2/r)}{(\\lambda_1-\\lambda_2)^2}$$")),
      tags$ul(class = "assump",
              tags$li("Two independent groups followed over person-time, two-sided test."),
              tags$li("Rates constant over follow-up. If they change, use survival methods."),
              tags$li("Individuals independent. For randomised clusters use cluster mode.")))
    else tagList(
      withMathJax(helpText("$$c=1+(z_{\\alpha/2}+z_\\beta)^2\\frac{(\\lambda_1+\\lambda_0)/y+k^2(\\lambda_1^2+\\lambda_0^2)}{(\\lambda_0-\\lambda_1)^2}$$")),
      tags$ul(class = "assump",
              tags$li("Clusters of roughly equal size."),
              tags$li("k, not cluster size, dominates the answer."),
              tags$li("Always round up. Part of a cluster cannot be randomised."),
              tags$li("Below about four clusters per arm the approximation fails.")))
  })
  
  # ---- Proportions outputs ----
  output$prop_answer <- renderUI({
    r <- prop_res(); u <- unit_of(input$prop_unit)
    if (input$prop_mode == "estimate")
      answer_block(fmt(r$final), paste(u, "to sample"),
                   sprintf("Estimates the proportion to within \u00b1%s at %s%% confidence, so the
                 interval would run about %s to %s.", input$prop_d, input$prop_conf*100,
                           round(input$prop_p - input$prop_d, 3), round(input$prop_p + input$prop_d, 3)))
    else if (input$prop_mode == "compare")
      answer_block(fmt(r$n1 + r$n2), paste(u, "in total"),
                   sprintf("%s in group 1 and %s in group 2. Cohen's h = %.2f, a %s effect.",
                           fmt(r$n1), fmt(r$n2), r$h,
                           if (r$h < 0.2) "very small" else if (r$h < 0.5) "small"
                           else if (r$h < 0.8) "medium" else "large"))
    else
      answer_block(r$clusters, "clusters per arm",
                   sprintf("%s clusters in total, about %s %s each, so roughly %s %s overall.
                 Unrounded: %.2f.", r$clusters*2, input$prop_cm, u,
                           fmt(r$clusters*2*input$prop_cm), u, r$raw))
  })
  output$prop_ladder <- renderUI({
    r <- prop_res(); u <- unit_of(input$prop_unit)
    if (input$prop_mode == "estimate") {
      fd <- if (is.na(input$prop_N) || input$prop_N <= 0)
        "not applied, population treated as large"
      else sprintf("frame of %s %s", fmt(input$prop_N), u)
      tagList(
        ladder_row("Raw requirement", "z\u00b2p(1-p)/d\u00b2, infinite population", fmt(round(r$n0))),
        ladder_row("After finite population", fd, fmt(round(r$nfpc))),
        ladder_row("After design effect",
                   sprintf("DEFF = %.2f from m = %s, rho = %s", r$deff, input$prop_m, input$prop_rho),
                   fmt(round(r$ndes))),
        ladder_row("After non-response",
                   sprintf("divided by %.0f%% response", (1 - input$prop_nr) * 100),
                   fmt(r$final), emphasis = TRUE))
    } else if (input$prop_mode == "compare") tagList(
      ladder_row("Difference to detect",
                 sprintf("%s vs %s, a gap of %s points", input$prop_p1, input$prop_p2,
                         round(abs(input$prop_p1 - input$prop_p2)*100, 1)),
                 sprintf("h = %.2f", r$h)),
      ladder_row("Raw requirement, group 1",
                 sprintf("%s%% confidence, %s%% power", input$prop_conf*100, input$prop_power*100),
                 fmt(round(r$raw))),
      ladder_row("After allocation ratio",
                 sprintf("group 2 is %s times group 1", input$prop_r),
                 fmt(round(r$raw * (1 + input$prop_r)))),
      ladder_row("After non-response",
                 sprintf("divided by %.0f%% response", (1 - input$prop_nr2) * 100),
                 fmt(r$n1 + r$n2), emphasis = TRUE))
    else tagList(
      ladder_row("Difference to detect", sprintf("%s vs %s", input$prop_cp1, input$prop_cp0),
                 sprintf("%.0f%% points", abs(input$prop_cp1 - input$prop_cp0)*100)),
      ladder_row("Between-cluster variation",
                 sprintf("k1 = %s, k0 = %s", input$prop_k1, input$prop_k0), ""),
      ladder_row("Cluster size", sprintf("%s %s each", input$prop_cm, u), ""),
      ladder_row("Clusters per arm", "rounded up from the formula", r$clusters, emphasis = TRUE))
  })
  output$prop_plot <- renderPlot({
    r <- prop_res(); u <- unit_of(input$prop_unit)
    if (input$prop_mode == "estimate") {
      ds <- seq(0.02, 0.15, by = 0.002)
      ns <- vapply(ds, function(d) est_one_prop(input$prop_p, d, input$prop_conf,
                                                input$prop_N, input$prop_rho, input$prop_m, input$prop_nr)$final, numeric(1))
      p <- ggplot(data.frame(d = ds, n = ns), aes(d, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(d = input$prop_d, n = r$final), colour = rose, size = 3.5) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Margin of error", y = paste(u, "needed"),
             subtitle = "Halving the margin roughly quadruples the sample.") + theme_ss()
      if (!is.na(input$prop_N) && input$prop_N > 0)
        p <- p + geom_hline(yintercept = input$prop_N, linetype = "dotted", colour = muted)
      p
    } else if (input$prop_mode == "compare") {
      lo <- max(0.01, min(input$prop_p1, input$prop_p2) - 0.10)
      hi <- min(0.99, max(input$prop_p1, input$prop_p2) + 0.10)
      ps <- seq(lo, hi, length.out = 80); ps <- ps[abs(ps - input$prop_p1) > 0.01]
      ns <- vapply(ps, function(p) { x <- cmp_two_prop(input$prop_p1, p, input$prop_conf,
                                                       input$prop_power, input$prop_r, input$prop_nr2); x$n1 + x$n2 }, numeric(1))
      ggplot(data.frame(p = ps, n = ns), aes(p, n)) +
        geom_vline(xintercept = input$prop_p1, linetype = "dotted", colour = muted) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(p = input$prop_p2, n = r$n1 + r$n2),
                   colour = rose, size = 3.5) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Proportion in group 2", y = paste("Total", u),
             subtitle = "Cost rises steeply as group 2 approaches group 1 (dotted line).") +
        theme_ss()
    } else {
      ms <- seq(10, max(input$prop_cm*3, 300), length.out = 60)
      cs <- vapply(ms, function(m) ceiling(clus_prop(input$prop_cp1, input$prop_cp0,
                                                     input$prop_k1, input$prop_k0, m, input$prop_conf, input$prop_powerc)), numeric(1))
      ggplot(data.frame(m = ms, c = cs), aes(m, c)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(m = input$prop_cm, c = r$clusters),
                   colour = rose, size = 3.5) +
        labs(x = paste(u, "per cluster"), y = "Clusters per arm",
             subtitle = "Between-cluster variation sets a floor cluster size cannot break.") +
        theme_ss()
    }
  })
  output$prop_notes <- renderUI({
    if (input$prop_mode == "estimate") tagList(
      withMathJax(helpText("$$n_0=\\frac{z^2p(1-p)}{d^2}\\qquad n=\\frac{n_0}{1+\\frac{n_0-1}{N}}\\times\\text{DEFF}\\div(1-\\text{NR})$$")),
      tags$ul(class = "assump",
              tags$li("Simple random sampling, or random sampling within clusters."),
              tags$li("Normal approximation: unreliable if n\u00d7p or n\u00d7(1-p) is below about 5."),
              tags$li("The sampling frame is complete."),
              tags$li("Non-response unrelated to the outcome. Inflation fixes precision, never bias.")))
    else if (input$prop_mode == "compare") tagList(
      withMathJax(helpText("$$n_1=\\frac{\\left[z_{\\alpha/2}\\sqrt{(1+1/r)\\bar p\\bar q}+z_\\beta\\sqrt{p_1q_1+p_2q_2/r}\\right]^2}{(p_1-p_2)^2}$$")),
      tags$ul(class = "assump",
              tags$li("Two independent groups, two-sided test."),
              tags$li("The stated difference is the smallest worth detecting, not the hoped-for one."),
              tags$li("No interim analyses or multiple comparisons.")))
    else tagList(
      withMathJax(helpText("$$c=1+(z_{\\alpha/2}+z_\\beta)^2\\frac{\\pi_1(1-\\pi_1)/m+\\pi_0(1-\\pi_0)/m+k^2(\\pi_1^2+\\pi_0^2)}{(\\pi_0-\\pi_1)^2}$$")),
      tags$ul(class = "assump",
              tags$li("Equal cluster sizes and equal numbers of clusters per arm."),
              tags$li("The two proportions must differ or the formula divides by zero."),
              tags$li("Round up to whole clusters."),
              tags$li("Matched or stratified designs need fewer clusters.")))
  })
  
  # ---- Means outputs ----
  output$mean_answer <- renderUI({
    r <- mean_res(); u <- unit_of(input$mean_unit)
    if (input$mean_mode == "estimate")
      answer_block(fmt(r$final), paste(u, "to sample"),
                   sprintf("Estimates the mean to within \u00b1%s units at %s%% confidence, given a
                 standard deviation of %s.", input$mean_d, input$mean_conf*100, input$mean_sd))
    else if (input$mean_mode == "compare")
      answer_block(fmt(r$n1 + r$n2), paste(u, "in total"),
                   sprintf("%s in group 1 and %s in group 2. Cohen's d = %.2f, a %s effect.",
                           fmt(r$n1), fmt(r$n2), r$d,
                           if (r$d < 0.2) "very small" else if (r$d < 0.5) "small"
                           else if (r$d < 0.8) "medium" else "large"))
    else
      answer_block(r$clusters, "clusters per arm",
                   sprintf("%s clusters in total, about %s %s each, so roughly %s %s overall.
                 Unrounded: %.2f.", r$clusters*2, input$mean_cm, u,
                           fmt(r$clusters*2*input$mean_cm), u, r$raw))
  })
  output$mean_ladder <- renderUI({
    r <- mean_res(); u <- unit_of(input$mean_unit)
    if (input$mean_mode == "estimate") {
      fd <- if (is.na(input$mean_N) || input$mean_N <= 0)
        "not applied, population treated as large"
      else sprintf("frame of %s %s", fmt(input$mean_N), u)
      tagList(
        ladder_row("Raw requirement", "z\u00b2\u03c3\u00b2/d\u00b2", fmt(round(r$n0))),
        ladder_row("After finite population", fd, fmt(round(r$nfpc))),
        ladder_row("After design effect", sprintf("DEFF = %.2f", r$deff), fmt(round(r$ndes))),
        ladder_row("After non-response",
                   sprintf("divided by %.0f%% response", (1 - input$mean_nr)*100),
                   fmt(r$final), emphasis = TRUE))
    } else if (input$mean_mode == "compare") tagList(
      ladder_row("Difference to detect",
                 sprintf("%s vs %s, SD %s", input$mean_mu1, input$mean_mu2, input$mean_csd),
                 sprintf("d = %.2f", r$d)),
      ladder_row("Raw requirement, group 1",
                 sprintf("%s%% confidence, %s%% power", input$mean_conf*100, input$mean_power*100),
                 fmt(round(r$raw))),
      ladder_row("After allocation ratio",
                 sprintf("group 2 is %s times group 1", input$mean_r),
                 fmt(round(r$raw*(1 + input$mean_r)))),
      ladder_row("After non-response",
                 sprintf("divided by %.0f%% response", (1 - input$mean_nr2)*100),
                 fmt(r$n1 + r$n2), emphasis = TRUE))
    else tagList(
      ladder_row("Difference to detect",
                 sprintf("%s vs %s", input$mean_cmu1, input$mean_cmu0),
                 sprintf("gap = %s", abs(input$mean_cmu1 - input$mean_cmu0))),
      ladder_row("Between-cluster variation",
                 sprintf("k1 = %s, k0 = %s", input$mean_k1, input$mean_k0), ""),
      ladder_row("Cluster size", sprintf("%s %s each", input$mean_cm, u), ""),
      ladder_row("Clusters per arm", "rounded up from the formula", r$clusters, emphasis = TRUE))
  })
  output$mean_plot <- renderPlot({
    r <- mean_res(); u <- unit_of(input$mean_unit)
    if (input$mean_mode == "estimate") {
      ds <- seq(input$mean_d/4, input$mean_d*3, length.out = 60)
      ns <- vapply(ds, function(d) est_one_mean(input$mean_sd, d, input$mean_conf,
                                                input$mean_N, input$mean_rho, input$mean_m, input$mean_nr)$final, numeric(1))
      ggplot(data.frame(d = ds, n = ns), aes(d, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(d = input$mean_d, n = r$final), colour = rose, size = 3.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Margin of error (measurement units)", y = paste(u, "needed"),
             subtitle = "Halving the margin roughly quadruples the sample.") + theme_ss()
    } else if (input$mean_mode == "compare") {
      ds <- seq(0.1, 1.5, by = 0.02)
      ns <- vapply(ds, function(d) { x <- cmp_two_mean(0, d*input$mean_csd, input$mean_csd,
                                                       input$mean_conf, input$mean_power, input$mean_r, input$mean_nr2); x$n1 + x$n2 },
                   numeric(1))
      ggplot(data.frame(d = ds, n = ns), aes(d, n)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(d = r$d, n = r$n1 + r$n2), colour = rose, size = 3.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Standardised effect size (Cohen's d)", y = paste("Total", u),
             subtitle = "Small effects are expensive: d of 0.2 costs about 25 times d of 1.0.") +
        theme_ss()
    } else {
      ks <- seq(0.02, 0.5, by = 0.01)
      cs <- vapply(ks, function(k) ceiling(clus_mean(input$mean_cmu1, input$mean_cmu0,
                                                     input$mean_s1, input$mean_s0, k, k, input$mean_cm, input$mean_conf,
                                                     input$mean_powerc)), numeric(1))
      ggplot(data.frame(k = ks, c = cs), aes(k, c)) +
        geom_line(colour = teal, linewidth = 1) +
        geom_point(data = data.frame(k = input$mean_k1, c = r$clusters),
                   colour = rose, size = 3.5) +
        labs(x = "Coefficient of variation (k)", y = "Clusters per arm",
             subtitle = "k moves the answer more than anything else in this formula.") +
        theme_ss()
    }
  })
  output$mean_notes <- renderUI({
    if (input$mean_mode == "estimate") tagList(
      withMathJax(helpText("$$n_0=\\frac{z^2\\sigma^2}{d^2}$$")),
      tags$ul(class = "assump",
              tags$li("The measure is roughly normal, or n is large."),
              tags$li("Sigma is a genuine prior estimate; if a guess, rerun across a range."),
              tags$li("d is in measurement units, not percentages.")))
    else if (input$mean_mode == "compare") tagList(
      withMathJax(helpText("$$n_1=\\frac{(1+1/r)(z_{\\alpha/2}+z_\\beta)^2\\sigma^2}{(\\mu_1-\\mu_2)^2}$$")),
      tags$ul(class = "assump",
              tags$li("Both groups share the same standard deviation."),
              tags$li("Normal approximation; below about 30 per group the t adds a unit or two."),
              tags$li("Observations independent within each group.")))
    else tagList(
      withMathJax(helpText("$$c=1+(z_{\\alpha/2}+z_\\beta)^2\\frac{(\\sigma_{W1}^2+\\sigma_{W0}^2)/m+k^2(\\mu_1^2+\\mu_0^2)}{(\\mu_0-\\mu_1)^2}$$")),
      tags$ul(class = "assump",
              tags$li("Within-cluster SD is spread among individuals; k carries between-cluster."),
              tags$li("k is relative to the mean, so it is scale sensitive."),
              tags$li("Round up to whole clusters.")))
  })
  
  # ---- The alpha/beta picture, shared across all three sample size tabs ----
  make_picture <- function(mode, conf, power, se, delta, margin) {
    if (mode == "estimate") {
      z <- z_conf(conf)
      shade_continuous(function(x) dnorm(x, 0, 1),
                       c(-4, 4),
                       regions = list(list(from = -z, to = z, fill = teal)),
                       marks = list(list(at = -z, colour = muted), list(at = z, colour = muted)),
                       xlab = "Standard errors from the estimate",
                       subtitle = sprintf(
                         "Teal holds %.0f%% of the sampling distribution. Your margin of error is %.2f standard errors wide on each side, which is what the %.0f%% confidence level buys.",
                         conf*100, z, conf*100))
    } else power_picture(se, delta, conf, power)
  }
  output$rate_pichead <- renderUI(
    if (input$rate_mode == "estimate") "What the confidence level means"
    else "What power looks like")
  output$prop_pichead <- renderUI(
    if (input$prop_mode == "estimate") "What the confidence level means"
    else "What power looks like")
  output$mean_pichead <- renderUI(
    if (input$mean_mode == "estimate") "What the confidence level means"
    else "What power looks like")
  
  output$rate_power <- renderPlot({
    r <- rate_res()
    if (input$rate_mode == "estimate")
      make_picture("estimate", input$rate_conf, NULL, NULL, NULL, NULL)
    else if (input$rate_mode == "compare") {
      d <- abs(input$rate_l1 - input$rate_l2)
      se <- d / (z_conf(input$rate_conf) + z_power(input$rate_power))
      power_picture(se, d, input$rate_conf, input$rate_power)
    } else {
      d <- abs(input$rate_cl1 - input$rate_cl0)
      se <- d / (z_conf(input$rate_conf) + z_power(input$rate_powerc))
      power_picture(se, d, input$rate_conf, input$rate_powerc)
    }
  })
  output$prop_power <- renderPlot({
    if (input$prop_mode == "estimate")
      make_picture("estimate", input$prop_conf, NULL, NULL, NULL, NULL)
    else if (input$prop_mode == "compare") {
      d <- abs(input$prop_p1 - input$prop_p2)
      se <- d / (z_conf(input$prop_conf) + z_power(input$prop_power))
      power_picture(se, d, input$prop_conf, input$prop_power)
    } else {
      d <- abs(input$prop_cp1 - input$prop_cp0)
      se <- d / (z_conf(input$prop_conf) + z_power(input$prop_powerc))
      power_picture(se, d, input$prop_conf, input$prop_powerc)
    }
  })
  output$mean_power <- renderPlot({
    if (input$mean_mode == "estimate")
      make_picture("estimate", input$mean_conf, NULL, NULL, NULL, NULL)
    else if (input$mean_mode == "compare") {
      d <- abs(input$mean_mu1 - input$mean_mu2)
      se <- d / (z_conf(input$mean_conf) + z_power(input$mean_power))
      power_picture(se, d, input$mean_conf, input$mean_power)
    } else {
      d <- abs(input$mean_cmu1 - input$mean_cmu0)
      se <- d / (z_conf(input$mean_conf) + z_power(input$mean_powerc))
      power_picture(se, d, input$mean_conf, input$mean_powerc)
    }
  })
  
  # ============================================================ DISTRIBUTIONS
  cur_dist <- reactive({ req(input$dist_which); DISTS[[input$dist_which]] })
  
  output$dist_about <- renderText(gsub("\\s+", " ", cur_dist()$about))
  
  # Parameter controls are rebuilt whenever the distribution changes. Fixed slot
  # ids (dp1..dp3) keep the server side simple: no dynamic observers needed.
  output$dist_params <- renderUI({
    d <- cur_dist()
    lapply(seq_along(d$pars), function(i) {
      p <- d$pars[[i]]
      param(paste0("dp", i), p$label, p$value, p$help, p$min, p$max, p$step)
    })
  })
  
  dist_vals <- reactive({
    d <- cur_dist()
    v <- vapply(seq_along(d$pars), function(i) {
      x <- input[[paste0("dp", i)]]
      if (is.null(x)) d$pars[[i]]$value else x
    }, numeric(1))
    # Guard the parameter constraints that would otherwise produce NaN silently.
    if (input$dist_which == "hyper")
      validate(need(v[2] <= v[1] && v[3] <= v[1],
                    "Successes and draws cannot exceed the population size."))
    v
  })
  
  dist_calc <- reactive({
    d <- cur_dist(); v <- dist_vals(); disc <- d$type == "discrete"
    if (input$dist_q == "quant") {
      req(input$dist_p)
      x <- d$qf(input$dist_p, v)
      return(list(kind = "quant", x = x,
                  label = sprintf("x = %s", sig(x)),
                  text = sprintf("%.4g of the distribution lies at or below %s.",
                                 input$dist_p, sig(x))))
    }
    if (input$dist_q == "between") {
      req(input$dist_a, input$dist_b)
      a <- input$dist_a; b <- input$dist_b
      validate(need(b >= a, "The upper edge must be at least the lower edge."))
      pr <- if (disc) d$pf(b, v) - d$pf(a - 1, v) else d$pf(b, v) - d$pf(a, v)
      return(list(kind = "between", a = a, b = b, p = pr,
                  label = sprintf("P(%s \u2264 X \u2264 %s)", sig(a), sig(b))))
    }
    req(input$dist_x); x <- input$dist_x
    if (input$dist_q == "le")
      list(kind = "le", x = x, p = d$pf(x, v), label = sprintf("P(X \u2264 %s)", sig(x)))
    else if (input$dist_q == "ge")
      list(kind = "ge", x = x,
           p = if (disc) 1 - d$pf(x - 1, v) else 1 - d$pf(x, v),
           label = sprintf("P(X \u2265 %s)", sig(x)))
    else {
      validate(need(disc, "P(X = x) is zero for a continuous distribution.
                           Use a between-range instead."))
      list(kind = "eq", x = x, p = d$d(x, v), label = sprintf("P(X = %s)", sig(x)))
    }
  })
  
  output$dist_answer <- renderUI({
    r <- dist_calc()
    if (r$kind == "quant")
      answer_block(sig(r$x), "the value of x", r$text)
    else
      answer_block(sig(r$p, 5), r$label,
                   sprintf("That is about %s in %s.", round(r$p * 1000), "1,000"))
  })
  
  output$dist_notes <- renderUI({
    d <- cur_dist(); v <- dist_vals(); mv <- d$mv(v)
    tagList(
      p(style = "font-size:0.88rem;line-height:1.55;", gsub("\\s+", " ", d$about)),
      stat_row("Type", if (d$type == "discrete") "Discrete" else "Continuous"),
      stat_row("Mean", if (is.na(mv[1])) "does not exist" else sig(mv[1])),
      stat_row("Variance", if (is.na(mv[2])) "does not exist" else sig(mv[2])),
      stat_row("SD", if (is.na(mv[2])) "does not exist" else sig(sqrt(mv[2]))),
      div(class = "verdict",
          if (d$type == "discrete")
            "Discrete: probability sits on individual values, so each bar is a real
           probability and P(X = x) is meaningful."
          else
            "Continuous: probability is area under the curve. P(X = x) is exactly zero
           for any single point, which is why only ranges have probabilities.")
    )
  })
  
  output$dist_plot <- renderPlot({
    d <- cur_dist(); v <- dist_vals(); r <- dist_calc(); disc <- d$type == "discrete"
    if (disc) {
      sup <- d$sup(v)
      if (length(sup) > 300) sup <- sup[seq(1, length(sup), length.out = 300)]
      dv  <- d$d(sup, v)
      keep <- switch(r$kind,
                     le = sup <= r$x, ge = sup >= r$x, eq = sup == r$x,
                     between = sup >= r$a & sup <= r$b, quant = sup <= r$x)
      shade_discrete(sup, dv, keep, xlab = "x",
                     subtitle = if (r$kind == "quant")
                       sprintf("Shaded bars carry the lower %.4g of the probability.", input$dist_p)
                     else sprintf("Shaded bars sum to %s = %s.", r$label, sig(r$p, 5)))
    } else {
      lo <- d$qf(0.0005, v); hi <- d$qf(0.9995, v)
      if (!is.finite(lo)) lo <- d$qf(0.01, v)
      if (!is.finite(hi)) hi <- d$qf(0.99, v)
      dfun <- function(x) d$d(x, v)
      reg <- switch(r$kind,
                    le      = list(list(from = lo, to = r$x, fill = rose)),
                    ge      = list(list(from = r$x, to = hi, fill = rose)),
                    between = list(list(from = r$a, to = r$b, fill = rose)),
                    quant   = list(list(from = lo, to = r$x, fill = rose)))
      mk <- if (r$kind == "between")
        list(list(at = r$a, colour = muted), list(at = r$b, colour = muted))
      else list(list(at = r$x, colour = muted))
      shade_continuous(dfun, c(lo, hi), reg, mk, xlab = "x",
                       subtitle = if (r$kind == "quant")
                         sprintf("The shaded area is %.4g, and it ends at x = %s.", input$dist_p, sig(r$x))
                       else sprintf("The shaded area is %s = %s.", r$label, sig(r$p, 5)))
    }
  })
  
  # ================================================================ INFERENCE
  # --- Mean ---
  im_calc <- reactive({
    req(input$im_conf, input$im_mu0, input$im_alt)
    if (input$im_src == "raw") {
      x <- parse_data(input$im_data)
      validate(need(length(x) >= 2, "Enter at least two observations."))
      n <- length(x); xbar <- mean(x); s <- sd(x)
    } else {
      req(input$im_n, input$im_xbar, input$im_s)
      n <- input$im_n; xbar <- input$im_xbar; s <- input$im_s
      validate(need(n >= 2, "n must be at least 2."))
    }
    se <- s / sqrt(n); df <- n - 1
    tstat <- (xbar - input$im_mu0) / se
    p <- switch(input$im_alt,
                two.sided = 2 * pt(-abs(tstat), df), greater = pt(tstat, df, lower.tail = FALSE),
                less = pt(tstat, df))
    tc <- qt(1 - (1 - input$im_conf)/2, df)
    list(n = n, xbar = xbar, s = s, se = se, df = df, t = tstat, p = p,
         ci = c(xbar - tc*se, xbar + tc*se))
  })
  output$im_answer <- renderUI({
    r <- im_calc()
    answer_block(sprintf("%s to %s", sig(r$ci[1]), sig(r$ci[2])),
                 sprintf("%.0f%% confidence interval for the mean", input$im_conf*100),
                 sprintf("Best estimate %s. Testing against %s gives p = %s, so the data are %s
               with the null value.", sig(r$xbar), input$im_mu0, pfmt(r$p),
                         if (r$p < 1 - input$im_conf) "not consistent" else "consistent"))
  })
  output$im_stats <- renderUI(tagList(
    stat_row("Sample size (n)", im_calc()$n),
    stat_row("Sample mean", sig(im_calc()$xbar)),
    stat_row("Sample SD (s)", sig(im_calc()$s)),
    stat_row("Standard error", sig(im_calc()$se)),
    stat_row("Degrees of freedom", im_calc()$df),
    stat_row("t statistic", sig(im_calc()$t)),
    stat_row("p-value", pfmt(im_calc()$p))))
  output$im_plot <- renderPlot({
    r <- im_calc(); dfun <- function(x) dt(x, r$df)
    lim <- max(4, abs(r$t) * 1.3)
    reg <- switch(input$im_alt,
                  two.sided = list(list(from = -lim, to = -abs(r$t), fill = rose),
                                   list(from = abs(r$t), to = lim, fill = rose)),
                  greater   = list(list(from = r$t, to = lim, fill = rose)),
                  less      = list(list(from = -lim, to = r$t, fill = rose)))
    shade_continuous(dfun, c(-lim, lim), reg,
                     list(list(at = r$t, colour = ink)), xlab = "t",
                     subtitle = sprintf("t distribution on %s df. Shaded area is the p-value, %s: the
                          chance of a t this extreme if the true mean were %s.",
                                        r$df, pfmt(r$p), input$im_mu0))
  })
  output$im_notes <- renderUI(tagList(
    div(class = "verdict",
        "The interval is the range of population means your data would not reject. The
       p-value is the chance of seeing a difference this large if the null value were
       true, so a small p means the data sit awkwardly with the null, not that the
       effect is large or important."),
    tags$ul(class = "assump",
            tags$li("Observations are independent."),
            tags$li("The population is roughly normal, or n is large enough for the mean to be."),
            tags$li("t rather than z, because the SD is estimated from the same data."))))
  
  # --- Difference of means ---
  id_calc <- reactive({
    req(input$id_conf, input$id_alt)
    x <- parse_data(input$id_g1); y <- parse_data(input$id_g2)
    validate(need(length(x) >= 2 && length(y) >= 2,
                  "Enter at least two observations in each group."))
    tt <- t.test(x, y, var.equal = (input$id_var == "pooled"),
                 conf.level = input$id_conf, alternative = input$id_alt)
    list(n1 = length(x), n2 = length(y), m1 = mean(x), m2 = mean(y),
         s1 = sd(x), s2 = sd(y), t = unname(tt$statistic), df = unname(tt$parameter),
         p = tt$p.value, ci = tt$conf.int, diff = mean(x) - mean(y))
  })
  output$id_answer <- renderUI({
    r <- id_calc()
    answer_block(sprintf("%s to %s", sig(r$ci[1]), sig(r$ci[2])),
                 sprintf("%.0f%% interval for group 1 minus group 2", input$id_conf*100),
                 sprintf("Observed difference %s, p = %s. The interval %s zero, so the difference
               %s statistically significant at this level.",
                         sig(r$diff), pfmt(r$p),
                         if (r$ci[1] <= 0 && r$ci[2] >= 0) "includes" else "excludes",
                         if (r$ci[1] <= 0 && r$ci[2] >= 0) "is not" else "is"))
  })
  output$id_stats <- renderUI({ r <- id_calc(); tagList(
    stat_row("Group 1: n, mean, SD", sprintf("%s, %s, %s", r$n1, sig(r$m1), sig(r$s1))),
    stat_row("Group 2: n, mean, SD", sprintf("%s, %s, %s", r$n2, sig(r$m2), sig(r$s2))),
    stat_row("Difference in means", sig(r$diff)),
    stat_row("t statistic", sig(r$t)),
    stat_row("Degrees of freedom", sig(r$df)),
    stat_row("p-value", pfmt(r$p))) })
  output$id_plot <- renderPlot({
    r <- id_calc(); dfun <- function(x) dt(x, r$df)
    lim <- max(4, abs(r$t)*1.3)
    reg <- switch(input$id_alt,
                  two.sided = list(list(from = -lim, to = -abs(r$t), fill = rose),
                                   list(from = abs(r$t), to = lim, fill = rose)),
                  greater   = list(list(from = r$t, to = lim, fill = rose)),
                  less      = list(list(from = -lim, to = r$t, fill = rose)))
    shade_continuous(dfun, c(-lim, lim), reg, list(list(at = r$t, colour = ink)),
                     xlab = "t", subtitle = sprintf("t on %.1f df. Shaded area is p = %s.",
                                                    r$df, pfmt(r$p)))
  })
  output$id_notes <- renderUI(tagList(
    div(class = "verdict",
        "If the interval for the difference contains zero, the data are consistent with
       the two groups having the same mean. Welch does not assume equal variances and
       is the safer default; the pooled test is slightly more powerful only when the
       variances really are equal."),
    tags$ul(class = "assump",
            tags$li("The two groups are independent, not paired."),
            tags$li("Each group is roughly normal, or large enough."),
            tags$li("For paired data (before and after on the same people) use the
               one-sample tab on the differences instead."))))
  
  # --- Proportion ---
  ip_calc <- reactive({
    req(input$ip_x, input$ip_n, input$ip_p0, input$ip_conf, input$ip_alt)
    validate(need(input$ip_x <= input$ip_n, "Successes cannot exceed the sample size."))
    x <- input$ip_x; n <- input$ip_n; ph <- x/n; p0 <- input$ip_p0
    se0 <- sqrt(p0*(1-p0)/n)
    z <- (ph - p0)/se0
    p <- switch(input$ip_alt, two.sided = 2*pnorm(-abs(z)),
                greater = pnorm(z, lower.tail = FALSE), less = pnorm(z))
    ex <- binom.test(x, n, p0, alternative = input$ip_alt, conf.level = input$ip_conf)
    list(x = x, n = n, ph = ph, z = z, p = p, ci = wilson_ci(x, n, input$ip_conf),
         exact = ex$p.value, small = min(n*p0, n*(1-p0)) < 10)
  })
  output$ip_answer <- renderUI({
    r <- ip_calc()
    answer_block(sprintf("%s to %s", sig(r$ci[1]), sig(r$ci[2])),
                 sprintf("%.0f%% Wilson interval for p", input$ip_conf*100),
                 sprintf("Observed %s of %s, which is %.1f%%. Testing against %s gives p = %s
               (exact binomial %s).", r$x, r$n, r$ph*100, input$ip_p0,
                         pfmt(r$p), pfmt(r$exact)))
  })
  output$ip_stats <- renderUI({ r <- ip_calc(); tagList(
    stat_row("Successes / sample", sprintf("%s / %s", r$x, r$n)),
    stat_row("Sample proportion", sig(r$ph)),
    stat_row("z statistic", sig(r$z)),
    stat_row("p-value (normal approx)", pfmt(r$p)),
    stat_row("p-value (exact binomial)", pfmt(r$exact)),
    stat_row("Wilson interval", sprintf("%s to %s", sig(r$ci[1]), sig(r$ci[2])))) })
  output$ip_plot <- renderPlot({
    r <- ip_calc(); lim <- max(4, abs(r$z)*1.3)
    reg <- switch(input$ip_alt,
                  two.sided = list(list(from = -lim, to = -abs(r$z), fill = rose),
                                   list(from = abs(r$z), to = lim, fill = rose)),
                  greater   = list(list(from = r$z, to = lim, fill = rose)),
                  less      = list(list(from = -lim, to = r$z, fill = rose)))
    shade_continuous(function(x) dnorm(x), c(-lim, lim), reg,
                     list(list(at = r$z, colour = ink)), xlab = "z",
                     subtitle = sprintf("Standard normal. Shaded area is p = %s, the chance of a z
                          this extreme if p really were %s.", pfmt(r$p), input$ip_p0))
  })
  output$ip_notes <- renderUI({ r <- ip_calc(); tagList(
    div(class = "verdict",
        "The Wilson interval is shown rather than the textbook Wald interval because Wald
       misbehaves badly when p is near 0 or 1 or n is small, sometimes running past 0%
       or 100%. The exact binomial p-value makes no normal approximation at all."),
    if (r$small) div(class = "verdict",
                     "Warning: with these numbers n times p0 or n times (1 - p0) is below 10, so the
       normal approximation is shaky here. Trust the exact binomial p-value."),
    tags$ul(class = "assump",
            tags$li("Independent observations, constant probability."),
            tags$li("Each observation is a simple yes or no."))) })
  
  # --- Difference of proportions ---
  iq_calc <- reactive({
    req(input$iq_x1, input$iq_n1, input$iq_x2, input$iq_n2, input$iq_conf, input$iq_alt)
    validate(need(input$iq_x1 <= input$iq_n1 && input$iq_x2 <= input$iq_n2,
                  "Successes cannot exceed sample sizes."))
    p1 <- input$iq_x1/input$iq_n1; p2 <- input$iq_x2/input$iq_n2
    pp <- (input$iq_x1 + input$iq_x2)/(input$iq_n1 + input$iq_n2)
    se0 <- sqrt(pp*(1-pp)*(1/input$iq_n1 + 1/input$iq_n2))
    z <- (p1 - p2)/se0
    p <- switch(input$iq_alt, two.sided = 2*pnorm(-abs(z)),
                greater = pnorm(z, lower.tail = FALSE), less = pnorm(z))
    seu <- sqrt(p1*(1-p1)/input$iq_n1 + p2*(1-p2)/input$iq_n2)
    zc <- z_conf(input$iq_conf)
    list(p1 = p1, p2 = p2, diff = p1 - p2, z = z, p = p,
         ci = c(p1-p2 - zc*seu, p1-p2 + zc*seu))
  })
  output$iq_answer <- renderUI({
    r <- iq_calc()
    answer_block(sprintf("%s to %s", sig(r$ci[1]), sig(r$ci[2])),
                 sprintf("%.0f%% interval for p1 minus p2", input$iq_conf*100),
                 sprintf("%.1f%% versus %.1f%%, a difference of %.1f percentage points, p = %s.
               The interval %s zero.", r$p1*100, r$p2*100, r$diff*100, pfmt(r$p),
                         if (r$ci[1] <= 0 && r$ci[2] >= 0) "includes" else "excludes"))
  })
  output$iq_stats <- renderUI({ r <- iq_calc(); tagList(
    stat_row("Proportion, group 1", sig(r$p1)),
    stat_row("Proportion, group 2", sig(r$p2)),
    stat_row("Difference", sig(r$diff)),
    stat_row("z statistic", sig(r$z)),
    stat_row("p-value", pfmt(r$p))) })
  output$iq_plot <- renderPlot({
    r <- iq_calc(); lim <- max(4, abs(r$z)*1.3)
    reg <- switch(input$iq_alt,
                  two.sided = list(list(from = -lim, to = -abs(r$z), fill = rose),
                                   list(from = abs(r$z), to = lim, fill = rose)),
                  greater   = list(list(from = r$z, to = lim, fill = rose)),
                  less      = list(list(from = -lim, to = r$z, fill = rose)))
    shade_continuous(function(x) dnorm(x), c(-lim, lim), reg,
                     list(list(at = r$z, colour = ink)), xlab = "z",
                     subtitle = sprintf("Standard normal. Shaded area is p = %s.", pfmt(r$p)))
  })
  output$iq_notes <- renderUI(tagList(
    div(class = "verdict",
        "The test uses a pooled standard error, because under the null the two groups
       share one proportion. The interval uses an unpooled one, because there we are
       not assuming they are equal. That is why a p-value just under 0.05 can sit
       beside an interval whose edge is almost exactly zero."),
    tags$ul(class = "assump",
            tags$li("Two independent groups."),
            tags$li("At least about five successes and five failures in each group."))))
  
  # --- Variance ---
  iv_calc <- reactive({
    req(input$iv_n, input$iv_s, input$iv_s0, input$iv_conf, input$iv_alt)
    n <- input$iv_n; s <- input$iv_s; s0 <- input$iv_s0; df <- n - 1
    chi <- df * s^2 / s0^2
    p <- switch(input$iv_alt,
                two.sided = 2*min(pchisq(chi, df), pchisq(chi, df, lower.tail = FALSE)),
                greater   = pchisq(chi, df, lower.tail = FALSE),
                less      = pchisq(chi, df))
    a <- 1 - input$iv_conf
    ci_var <- c(df*s^2/qchisq(1-a/2, df), df*s^2/qchisq(a/2, df))
    list(n = n, s = s, df = df, chi = chi, p = p,
         ci_var = ci_var, ci_sd = sqrt(ci_var))
  })
  output$iv_answer <- renderUI({
    r <- iv_calc()
    answer_block(sprintf("%s to %s", sig(r$ci_sd[1]), sig(r$ci_sd[2])),
                 sprintf("%.0f%% interval for the standard deviation", input$iv_conf*100),
                 sprintf("Sample SD %s on %s df. Testing against %s gives p = %s. The variance
               interval is %s to %s.", sig(r$s), r$df, input$iv_s0, pfmt(r$p),
                         sig(r$ci_var[1]), sig(r$ci_var[2])))
  })
  output$iv_stats <- renderUI({ r <- iv_calc(); tagList(
    stat_row("Sample size (n)", r$n),
    stat_row("Sample SD", sig(r$s)),
    stat_row("Sample variance", sig(r$s^2)),
    stat_row("Degrees of freedom", r$df),
    stat_row("Chi-square statistic", sig(r$chi)),
    stat_row("p-value", pfmt(r$p))) })
  output$iv_plot <- renderPlot({
    r <- iv_calc(); hi <- max(qchisq(0.999, r$df), r$chi*1.2)
    reg <- switch(input$iv_alt,
                  greater   = list(list(from = r$chi, to = hi, fill = rose)),
                  less      = list(list(from = 0, to = r$chi, fill = rose)),
                  two.sided = if (pchisq(r$chi, r$df) < 0.5)
                    list(list(from = 0, to = r$chi, fill = rose))
                  else list(list(from = r$chi, to = hi, fill = rose)))
    shade_continuous(function(x) dchisq(x, r$df), c(0, hi), reg,
                     list(list(at = r$chi, colour = ink)), xlab = "Chi-square",
                     subtitle = sprintf("Chi-square on %s df. Shaded is the one-tailed area; the
                          two-sided p doubles the smaller tail. p = %s.",
                                        r$df, pfmt(r$p)))
  })
  output$iv_notes <- renderUI(tagList(
    div(class = "verdict",
        "Notice the interval is not symmetric around the sample SD: the chi-square
       distribution is skewed, so there is more room above than below. This procedure
       is far more sensitive to non-normality than the t procedures for means, so
       treat it cautiously unless the data really do look normal."),
    tags$ul(class = "assump",
            tags$li("The population is normal. This assumption is not optional here."),
            tags$li("Observations are independent."))))
}

shinyApp(ui = ui, server = server)