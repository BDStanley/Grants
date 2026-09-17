# ------------------------------------------------------------------
# stage2_diagnostic.R — Stage 2: the diagnostic test (§7.3)
#
# For every pre-registered outcome, among respondents:
#
#   Y = a + b*Z + X'g + e        (Bailey eq. 10.6)
#
# with HC2 standard errors and the covariate set fixed ex ante. The bivariate
# version of this test has a catastrophic false-positive rate (fig. 11.4),
# because arm assignment is random among invitees but not among respondents.
#
# Confirmatory claims come from this stage, not from the selection models.
# ------------------------------------------------------------------

#' HC2 variance without a package dependency
vcov_hc2 <- function(fit) {
  X <- stats::model.matrix(fit)
  XtXinv <- chol2inv(chol(crossprod(X)))
  h <- rowSums((X %*% XtXinv) * X)
  u <- stats::residuals(fit)^2 / (1 - h)
  meat <- crossprod(X, X * u)
  XtXinv %*% meat %*% XtXinv
}

#' The diagnostic test for one outcome
#'
#' Effects are reported in the unit the item is read in: percentage points for
#' binary items, standard deviations for scales and indices. The SD used is the
#' respondent-sample SD, because that is all the real study will have.
diagnostic_test_one <- function(respondents, item, type,
                                covariates = diagnostic_covariates(),
                                extra = character()) {
  d <- respondents[!is.na(respondents[[item]]), ]
  if (nrow(d) < 100) {
    return(data.frame(item, n = nrow(d), est = NA, se = NA, p = NA,
                      est_u = NA, se_u = NA, unit = NA))
  }
  f   <- stats::reformulate(c("armB", covariates, extra), response = item)
  fit <- stats::lm(f, data = d)
  V   <- vcov_hc2(fit)
  b   <- stats::coef(fit)[["armB"]]
  se  <- sqrt(V[which(names(stats::coef(fit)) == "armB"),
                which(names(stats::coef(fit)) == "armB")])
  dfres <- stats::df.residual(fit)
  p   <- 2 * stats::pt(abs(b / se), dfres, lower.tail = FALSE)

  sdy <- stats::sd(d[[item]], na.rm = TRUE)
  scale_by <- if (type == "binary") 100 else 1 / sdy
  data.frame(item = item, n = nrow(d), est = b, se = se, p = p,
             est_u = b * scale_by, se_u = se * scale_by,
             unit = if (type == "binary") "pp" else "SD",
             # SD units for every item, binary ones included: the equivalence
             # bounds are stated in SD throughout (§6, revised)
             est_sd = b / sdy, se_sd = se / sdy,
             stringsAsFactors = FALSE)
}

#' Run the diagnostic over every registered outcome
#'
#' @param extra covariates added on top of the fixed set; used for the
#'   with-political-interest robustness column (§7.3)
run_diagnostics <- function(respondents, reg = outcome_registry(),
                            extra = character()) {
  items <- intersect(reg$item, names(respondents))
  out <- do.call(rbind, lapply(items, function(it) {
    diagnostic_test_one(respondents, it,
                        type = reg$type[reg$item == it], extra = extra)
  }))
  merge(out, reg[, c("item", "label", "type", "role", "hypothesis")], by = "item")
}

#' Benjamini-Hochberg within the confirmatory family only (§6)
apply_bh <- function(diagnostics, alpha = 0.05) {
  fam <- diagnostics$item %in% confirmatory_items()
  diagnostics$q <- NA_real_
  diagnostics$q[fam] <- stats::p.adjust(diagnostics$p[fam], method = "BH")
  diagnostics$reject_bh <- !is.na(diagnostics$q) & diagnostics$q < alpha
  diagnostics
}

#' Equivalence test (TOST) for the items whose ignorability we claim (§6 H5)
#'
#' Ignorability is a positive claim and needs a positive test: we reject the
#' null of a difference at least as large as the smallest effect of interest.
run_tost <- function(diagnostics, alpha = 0.05) {
  d <- diagnostics[diagnostics$item %in% equivalence_items(), ]
  bound <- sesoi(d$type)                       # +/- 0.10 SD for every item type
  p_lo <- stats::pnorm((d$est_sd + bound) / d$se_sd, lower.tail = FALSE)
  p_hi <- stats::pnorm((d$est_sd - bound) / d$se_sd, lower.tail = TRUE)
  data.frame(item = d$item, label = d$label, est_sd = d$est_sd, se_sd = d$se_sd,
             bound = bound, p_tost = pmax(p_lo, p_hi),
             equivalent = pmax(p_lo, p_hi) < alpha, stringsAsFactors = FALSE)
}

#' The confirmatory analysis: four constructs, one-sided (§6, revised)
#'
#' Each hypothesis in the §6 table carries a signed prediction, so each test is
#' one-sided in that direction. A result that is significant in the *wrong*
#' direction is reported as such and treated as a failure of the hypothesis, not
#' as a discovery.
run_confirmatory <- function(respondents, creg = construct_registry(),
                             covariates = diagnostic_covariates(), alpha = 0.05) {
  rows <- lapply(seq_len(nrow(creg)), function(i) {
    item <- creg$construct[i]
    d <- respondents[!is.na(respondents[[item]]), ]
    f <- stats::reformulate(c("armB", covariates), response = item)
    fit <- stats::lm(f, data = d)
    V <- vcov_hc2(fit); nm <- names(stats::coef(fit))
    b  <- stats::coef(fit)[["armB"]]
    se <- sqrt(V[which(nm == "armB"), which(nm == "armB")])
    sdy <- stats::sd(d[[item]], na.rm = TRUE)
    z_dir <- creg$direction[i] * b / se          # positive = as predicted
    data.frame(construct = item, label = creg$label[i],
               hypothesis = creg$hypothesis[i], direction = creg$direction[i],
               n = nrow(d), est_sd = b / sdy, se_sd = se / sdy,
               z = b / se, z_directional = z_dir,
               p_one_sided = stats::pnorm(z_dir, lower.tail = FALSE),
               p_two_sided = 2 * stats::pnorm(abs(b / se), lower.tail = FALSE),
               ci_lo = (b - 1.96 * se) / sdy, ci_hi = (b + 1.96 * se) / sdy,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$q <- stats::p.adjust(out$p_one_sided, method = "BH")
  out$reject <- out$q < alpha
  out
}

#' The global test: is any of this non-ignorable? (§6, revised)
#'
#' Combines the four directional test statistics, correcting for the fact that
#' they come from four correlated outcomes measured on the same respondents
#' (Brown's method: the sum of z divided by the square root of the summed
#' correlation matrix). Far better powered than any single construct, and it is
#' the headline confirmatory claim — the per-construct tests say where.
global_test <- function(respondents, confirmatory, creg = construct_registry(),
                        covariates = diagnostic_covariates()) {
  items <- creg$construct
  d <- respondents[stats::complete.cases(respondents[, items]), ]
  resid <- vapply(items, function(it) {
    stats::residuals(stats::lm(stats::reformulate(c("armB", covariates),
                                                  response = it), data = d))
  }, numeric(nrow(d)))
  R <- stats::cor(resid)
  z <- confirmatory$z_directional[match(items, confirmatory$construct)]
  Z <- sum(z) / sqrt(sum(R))
  list(z = Z, p = stats::pnorm(Z, lower.tail = FALSE), R = R,
       components = stats::setNames(z, items))
}

#' The primary deliverable: estimates with intervals, not verdicts (§6, revised)
#'
#' A pilot that reports "rho-hat = 0.31, 95% CI [0.05, 0.55]" has produced
#' something the report, the paper and the OPUS power analysis can all use,
#' whether or not any test clears its threshold. The tests are secondary to
#' this table, not the other way round.
estimation_table <- function(confirmatory, heckman = NULL, boot = NULL,
                             truth = NULL) {
  out <- confirmatory[, c("construct", "label", "hypothesis", "n", "est_sd",
                          "se_sd", "ci_lo", "ci_hi", "p_one_sided", "q")]
  names(out)[names(out) == "est_sd"] <- "arm_diff_sd"
  if (!is.null(heckman) && nrow(heckman)) {
    h <- heckman[, c("item", "rho", "rho_se", "est")]
    names(h) <- c("construct", "rho", "rho_se", "corrected")
    out <- merge(out, h, by = "construct", all.x = TRUE)
    out$rho_lo <- out$rho - 1.96 * out$rho_se
    out$rho_hi <- out$rho + 1.96 * out$rho_se
  }
  if (!is.null(boot) && nrow(boot)) {
    names(boot)[names(boot) == "item"] <- "construct"
    out <- merge(out, boot[, c("construct", "ci_lo", "ci_hi")], by = "construct",
                 all.x = TRUE, suffixes = c("", "_corrected"))
  }
  if (!is.null(truth)) {
    out$truth <- truth$truth[match(out$construct, truth$item)]
  }
  out
}

#' The verdict table: the thing the project exists to produce (§6, rule (c))
#'
#' Verdicts are issued only where a pre-registered test can support one: the
#' four confirmatory constructs, which can be declared non-ignorable, and the
#' H5 items, whose ignorability is affirmed by equivalence testing. Everything
#' else in the questionnaire is reported as an estimate with an interval and
#' carries no verdict at all — which is the honest position, and the reason the
#' estimation table rather than this one is the primary output.
verdict_table <- function(confirmatory, tost, global = NULL) {
  con <- data.frame(
    name = confirmatory$construct, label = confirmatory$label,
    family = "konfirmacyjna", hypothesis = confirmatory$hypothesis,
    est_sd = confirmatory$est_sd, ci_lo = confirmatory$ci_lo,
    ci_hi = confirmatory$ci_hi, p = confirmatory$p_one_sided,
    q = confirmatory$q,
    verdict = ifelse(confirmatory$reject, "nieignorowalny", "nierozstrzygniete"),
    stringsAsFactors = FALSE)

  eq <- data.frame(
    name = tost$item, label = tost$label, family = "rownowaznosc (H5)",
    hypothesis = "H5", est_sd = tost$est_sd,
    ci_lo = tost$est_sd - 1.96 * tost$se_sd,
    ci_hi = tost$est_sd + 1.96 * tost$se_sd,
    p = tost$p_tost, q = NA_real_,
    verdict = ifelse(tost$equivalent, "ignorowalny", "nierozstrzygniete"),
    stringsAsFactors = FALSE)

  out <- rbind(con, eq)
  if (!is.null(global)) {
    attr(out, "global") <- sprintf("test laczny: Z = %.2f, p = %.4g", global$z,
                                   global$p)
  }
  out[order(out$family, match(out$verdict, c("nieignorowalny", "ignorowalny",
                                             "nierozstrzygniete"))), ]
}

#' Item-level results, secondary: estimates, never verdicts
flag_items <- function(diagnostics, tost, confirmatory = NULL) {
  v <- merge(diagnostics[, c("item", "label", "role", "hypothesis", "est_u",
                             "se_u", "unit", "est_sd", "se_sd", "p", "q")],
             tost[, c("item", "p_tost", "equivalent")], by = "item", all.x = TRUE)
  v$ci_lo <- v$est_sd - 1.96 * v$se_sd
  v$ci_hi <- v$est_sd + 1.96 * v$se_sd
  v[order(v$p), ]
}

#' Placebo: arm B vs arm A wave 0 (§6 exploratory)
#'
#' Both groups completed off the same day-0 invitation with no reminder behind
#' them. A difference here is a warning that something other than the
#' instrument is separating the arms.
placebo_wave0 <- function(respondents, reg = outcome_registry()) {
  d <- respondents[respondents$arm == "B" | respondents$wave == 0, ]
  run_diagnostics(d, reg)
}

#' Monotone trend across the four propensity levels (§3, §7.3)
#'
#' Guards against the non-monotonic response-propensity relationships that are
#' one of the known failure modes of this design (ch. 11.4).
gradient_trend <- function(respondents, reg = outcome_registry()) {
  d <- respondents
  d$level <- as.integer(propensity_level(d))
  items <- intersect(reg$item, names(d))
  do.call(rbind, lapply(items, function(it) {
    dd <- d[!is.na(d[[it]]), ]
    if (nrow(dd) < 100) return(NULL)
    f <- stats::reformulate(c("level", diagnostic_covariates()), response = it)
    fit <- stats::lm(f, data = dd)
    V <- vcov_hc2(fit); nm <- names(stats::coef(fit))
    b <- stats::coef(fit)[["level"]]
    se <- sqrt(V[which(nm == "level"), which(nm == "level")])
    means <- tapply(dd[[it]], propensity_level(dd), mean, na.rm = TRUE)
    data.frame(item = it, trend = b, se = se,
               p = 2 * stats::pnorm(abs(b / se), lower.tail = FALSE),
               monotone = all(diff(as.numeric(means)) >= 0) ||
                          all(diff(as.numeric(means)) <= 0),
               t(as.matrix(means)), stringsAsFactors = FALSE)
  }))
}

#' Manipulation check: political interest should be higher in arm B (§6)
#'
#' If this is null the instrument is not doing what the design assumes, and
#' §6's pre-specified inference rule says nulls elsewhere are uninformative.
manipulation_check <- function(respondents) {
  diagnostic_test_one(respondents, "intpol", type = "scale")
}

#' Item nonresponse and data quality by arm (§4.3 secondary outcomes)
quality_by_arm <- function(respondents) {
  agg <- function(v) tapply(respondents[[v]], respondents$arm, mean, na.rm = TRUE)
  data.frame(measure = c("mediana czasu (min)", "attention check zdany",
                         "straightlining"),
             A = c(stats::median(respondents$duration[respondents$arm == "A"], na.rm = TRUE),
                   agg("attention_pass")[["A"]], agg("straightline")[["A"]]),
             B = c(stats::median(respondents$duration[respondents$arm == "B"], na.rm = TRUE),
                   agg("attention_pass")[["B"]], agg("straightline")[["B"]]))
}
