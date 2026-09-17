# ------------------------------------------------------------------
# stage1_instrument.R — Stage 1: is the instrument strong? (§7.2)
#
# Estimated on the invited sample, not on respondents. Everything downstream is
# conditional on this stage passing: a weak instrument makes the selection
# models useless and makes null results in Stage 2 uninformative.
# ------------------------------------------------------------------

#' Response rates by arm, with the difference that is the whole instrument
response_rates_by_arm <- function(invited) {
  agg <- stats::aggregate(responded ~ arm, data = invited,
                          FUN = function(x) c(n = length(x), completes = sum(x),
                                              rr = mean(x)))
  out <- data.frame(arm = agg$arm, invited = agg$responded[, "n"],
                    completes = agg$responded[, "completes"],
                    rr = agg$responded[, "rr"])
  out$gap_pp <- 100 * (out$rr[out$arm == "A"] - out$rr[out$arm == "B"])
  out
}

#' The response probit: R ~ Z + profile covariates (+ batch)
#'
#' The coefficient on the arm indicator is the inclusion condition. Bailey's
#' own application (tab. 12.1) reports z = -23.8; §7.2 sets our target at
#' |z| well above 10.
fit_first_stage <- function(invited) {
  f <- stats::reformulate(c("armB", profile_covariates()), response = "responded")
  stats::glm(f, family = stats::binomial(link = "probit"), data = invited)
}

#' Inclusion condition: effect size, precision, and the pre-registered verdict
#' @param min_gap_pp the *power* threshold from §4.2, not a validity one. The
#'   inclusion condition is easy to satisfy — a 3 pp gap already gives z of 7 to
#'   10 — and passing it says nothing about whether the study can detect
#'   anything. The thresholds below come from `gap_curve()`.
assess_inclusion <- function(first_stage, rates, params,
                             min_gap_pp = 7, caution_gap_pp = 5, min_abs_z = 10) {
  ct  <- summary(first_stage)$coefficients
  z   <- ct["armB", "z value"]
  gap <- rates$gap_pp[1]
  list(
    arm_coef = unname(ct["armB", "Estimate"]),
    arm_se   = unname(ct["armB", "Std. Error"]),
    z        = unname(z),
    gap_pp   = gap,
    valid    = abs(z) >= min_abs_z,
    passes   = abs(z) >= min_abs_z && gap >= min_gap_pp,
    verdict  = if (abs(z) < min_abs_z) {
      "warunek wlaczenia niespelniony - instrument nie dziala"
    } else if (gap >= min_gap_pp) {
      "przyrost wystarczajacy - analiza zgodnie z planem"
    } else if (gap >= caution_gap_pp) {
      "przyrost 5-7 pp - kontynuowac, raportujac obnizona moc; ciezar na tescie lacznym i tabeli oszacowan"
    } else {
      "przyrost < 5 pp - uruchomic klauzule eskalacji: czwarta fala przypomnien w ramieniu A"
    }
  )
}

#' Response curve by reminder wave in arm A (§7.2)
#'
#' Feeds the protocol recommendation for PGSW — how much each reminder actually
#' buys — and supplies the five-level propensity gradient used in Stage 2.
reminder_wave_curves <- function(invited) {
  a <- invited[invited$arm == "A", ]
  n <- nrow(a)
  waves <- stats::aggregate(list(completes = rep(1, sum(!is.na(a$wave)))),
                            by = list(wave = a$wave[!is.na(a$wave)]), FUN = length)
  waves$share_of_invited <- waves$completes / n
  waves$cumulative_rr <- cumsum(waves$share_of_invited)
  waves$lift_pp <- 100 * waves$share_of_invited
  waves
}

#' The response-propensity gradient (§3)
#'
#' Ordered by decreasing eagerness: arm B, then arm A by the reminder wave the
#' respondent completed on. Arm B respondents and arm A wave-0 respondents faced
#' an identical stimulus, so level 1 vs level 2 is the placebo comparison; from
#' level 2 onwards it is the continuum of resistance. With three reminders the
#' gradient has five levels, which is what makes it better powered than the
#' binary arm contrast — and it is not randomly assigned, which is why it is
#' pre-registered as secondary and always carries field-day controls (§6).
propensity_level <- function(respondents, max_wave = NULL) {
  w <- respondents$wave
  if (is.null(max_wave)) max_wave <- max(w[respondents$arm != "B"], na.rm = TRUE)
  lvl <- ifelse(respondents$arm == "B", 1L, pmin(w, max_wave) + 2L)
  factor(lvl, levels = seq_len(max_wave + 2),
         labels = c("B", sprintf("A-fala %d", 0:max_wave)))
}
