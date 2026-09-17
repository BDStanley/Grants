# ------------------------------------------------------------------
# stage4_compare.R — Stage 4: comparison and external validation (§7.5)
#
# Three estimates per item — observed, conventionally raked, selection-corrected
# — set against each other, against the external benchmarks, and (only because
# these data are synthetic) against the truth.
#
# This is where the project's actual deliverable is assembled: a per-item
# verdict with the numbers that justify it.
# ------------------------------------------------------------------

#' Conventional raking to population margins (§7.5)
#'
#' Sex, age group, education, settlement size and region — standard Polish
#' practice, and the benchmark this project tests.
rake_to_margins <- function(respondents, margins) {
  d <- respondents
  d$female <- factor(d$female)
  des <- survey::svydesign(ids = ~1, data = d)
  m_female <- margins$female; names(m_female) <- c("female", "Freq")
  survey::rake(des,
               sample.margins = list(~female, ~agegrp, ~edu, ~town, ~region),
               population.margins = list(m_female, margins$agegrp, margins$edu,
                                         margins$town, margins$region))
}

#' Observed and raked means for one outcome, in display units
observed_and_raked <- function(respondents, design, item, type) {
  v <- respondents[[item]]
  k <- if (type == "binary") 100 else 1
  obs <- k * mean(v, na.rm = TRUE)
  obs_se <- k * stats::sd(v, na.rm = TRUE) / sqrt(sum(!is.na(v)))
  w <- survey::svymean(stats::reformulate(item), design, na.rm = TRUE)
  data.frame(item = item,
             observed = obs, observed_se = obs_se,
             raked = k * as.numeric(stats::coef(w)[1]),
             raked_se = k * as.numeric(survey::SE(w)[1]))
}

#' The three-column comparison, plus truth and benchmarks (§7.5)
compare_estimators <- function(respondents, design, heckman, boot, truth,
                               benchmarks, reg = outcome_registry()) {
  items <- intersect(reg$item, names(respondents))
  base <- do.call(rbind, lapply(items, function(it) {
    observed_and_raked(respondents, design, it, reg$type[reg$item == it])
  }))

  out <- merge(base, reg[, c("item", "label", "type", "role", "hypothesis")], by = "item")
  out <- merge(out, heckman[, c("item", "est", "rho", "z_rho", "r2_m")],
               by = "item", all.x = TRUE)
  names(out)[names(out) == "est"] <- "corrected"
  if (!is.null(boot)) {
    out <- merge(out, boot[, c("item", "boot_se", "ci_lo", "ci_hi")],
                 by = "item", all.x = TRUE)
  }
  out <- merge(out, truth, by = "item", all.x = TRUE)
  out <- merge(out, benchmarks[, c("item", "benchmark", "source", "verified")],
               by = "item", all.x = TRUE)

  # errors against the truth exist only in the simulation; against the
  # benchmark they are what the real study will report
  out$err_observed  <- out$observed  - out$truth
  out$err_raked     <- out$raked     - out$truth
  out$err_corrected <- out$corrected - out$truth
  out$bench_observed  <- out$observed  - out$benchmark
  out$bench_raked     <- out$raked     - out$benchmark
  out$bench_corrected <- out$corrected - out$benchmark

  out[order(out$role, out$item), ]
}

#' Headline exhibit: estimators against the truth, per item (mirrors Bailey fig. 12.4)
#'
#' Deliberately plain. The house theme is applied when the report is rendered,
#' so the pipeline stays free of font dependencies.
plot_estimator_comparison <- function(comparison, items = NULL) {
  d <- comparison
  if (!is.null(items)) d <- d[d$item %in% items, ]
  long <- do.call(rbind, lapply(c("observed", "raked", "corrected"), function(k) {
    se <- if (k == "corrected") d$boot_se else d[[paste0(k, "_se")]]
    data.frame(label = d$label, estimator = k, est = d[[k]],
               se = if (is.null(se)) NA else se, truth = d$truth)
  }))
  long$estimator <- factor(long$estimator, c("observed", "raked", "corrected"),
                           c("Surowe", "Po ważeniu", "Model selekcji"))
  ggplot2::ggplot(long, ggplot2::aes(estimator, est)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = truth), linetype = "dashed") +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = est - 1.96 * se,
                                          ymax = est + 1.96 * se), na.rm = TRUE) +
    ggplot2::facet_wrap(~label, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Oszacowanie",
                  title = "Oszacowania wobec prawdy (dane syntetyczne)",
                  subtitle = "Linia przerywana = prawdziwa wartość w populacji")
}

#' Manski bounds at the observed response rate (§7.7)
#'
#' Assumption-free and therefore very wide. The honesty exhibit: this is what
#' the data alone can support before any modelling.
manski_bounds <- function(respondents, invited, item, type) {
  v <- respondents[[item]]
  p <- sum(!is.na(v)) / nrow(invited)
  k <- if (type == "binary") 100 else 1
  m <- k * mean(v, na.rm = TRUE)
  rng <- if (type == "binary") c(0, 100) else range(v, na.rm = TRUE)
  data.frame(item = item, response_rate = p,
             lower = p * m + (1 - p) * rng[1],
             upper = p * m + (1 - p) * rng[2])
}
