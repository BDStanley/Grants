# ------------------------------------------------------------------
# stage3_selection.R — Stage 3: selection models for flagged items (§7.4)
#
# Heckman ML is the fixed, interpretable reference, as in Bailey ch. 12. The
# first stage is estimated on the *invited* sample — respondents and
# non-respondents together — which is the whole reason the contract requires
# invitee-level data (§4.4).
#
# These models corroborate; they do not adjudicate. The confirmatory claim
# belongs to Stage 2, because the Heckman likelihood rejects too often under
# misspecification (§6, rule (b)).
# ------------------------------------------------------------------

#' Attach one outcome to the invited frame, NA for non-respondents
invited_with_outcome <- function(invited, respondents, item) {
  d <- merge(invited, respondents[, c("id", item)], by = "id", all.x = TRUE)
  d$responded_obs <- as.numeric(!is.na(d[[item]]))   # observed = completed AND answered
  d
}

#' Heckman ML for one outcome
#'
#' @return the corrected population estimate, rho with its standard error, and
#'   the R2_M multicollinearity diagnostic that ch. 8.6 says to report as a
#'   matter of course.
fit_heckman_one <- function(invited, respondents, item, type,
                            selection_covariates = profile_covariates(),
                            outcome_covariates = diagnostic_covariates()) {
  d <- invited_with_outcome(invited, respondents, item)
  y <- d[[item]]
  d$y <- if (type == "binary") factor(y) else y

  sel <- stats::reformulate(c("armB", selection_covariates), response = "responded_obs")
  out <- stats::reformulate(outcome_covariates, response = "y")

  fit <- try(sampleSelection::selection(sel, out, data = d, method = "ml"), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(list(item = item, est = NA_real_, rho = NA_real_, rho_se = NA_real_,
                r2_m = NA_real_, converged = FALSE,
                message = as.character(fit)))
  }

  co <- summary(fit)$estimate
  b_out <- stats::coef(fit, part = "outcome")
  X <- stats::model.matrix(stats::reformulate(outcome_covariates), data = d)
  b_out <- b_out[colnames(X)]
  lin <- as.numeric(X %*% b_out)
  est <- if (type == "binary") 100 * mean(stats::pnorm(lin)) else mean(lin)

  # R2_M: how much of the inverse Mills ratio the outcome covariates already
  # explain. High values mean the correction is identified off functional form
  # rather than off the instrument.
  b_sel <- fit$estimate[fit$param$index$betaS]
  Xs <- stats::model.matrix(stats::reformulate(c("armB", selection_covariates)), data = d)
  zg <- as.numeric(Xs %*% b_sel[colnames(Xs)])
  imr <- stats::dnorm(zg) / stats::pnorm(zg)
  Xn <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  r2_m <- summary(stats::lm(imr ~ Xn))$r.squared

  list(item = item, est = est,
       rho = unname(co["rho", 1]), rho_se = unname(co["rho", 2]),
       r2_m = r2_m, converged = TRUE, message = NA_character_)
}

#' Heckman ML for every flagged item
fit_heckman <- function(invited, respondents, items, reg = outcome_registry()) {
  if (!length(items)) {
    return(data.frame(item = character(), est = numeric(), rho = numeric(),
                      rho_se = numeric(), z_rho = numeric(), r2_m = numeric(),
                      converged = logical(), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(items, function(it) {
    r <- fit_heckman_one(invited, respondents, it,
                         type = reg$type[reg$item == it][1])
    data.frame(item = r$item, est = r$est, rho = r$rho, rho_se = r$rho_se,
               z_rho = r$rho / r$rho_se, r2_m = r$r2_m,
               converged = r$converged, stringsAsFactors = FALSE)
  }))
}

#' Nonparametric bootstrap over the invited sample, stratified by arm (§7.4)
#'
#' Corrected estimates have no closed-form standard error worth trusting here,
#' and reporting them without an interval makes the most model-dependent
#' number on the page look like the most precise one.
bootstrap_heckman <- function(invited, respondents, items, n_boot = 200,
                              reg = outcome_registry(), seed = 67L) {
  if (!length(items) || n_boot < 2) {
    return(data.frame(item = character(), boot_se = numeric(), ci_lo = numeric(),
                      ci_hi = numeric(), n_ok = integer()))
  }
  set.seed(seed)
  idxA <- which(invited$arm == "A"); idxB <- which(invited$arm == "B")

  reps <- lapply(seq_len(n_boot), function(b) {
    take <- c(sample(idxA, length(idxA), replace = TRUE),
              sample(idxB, length(idxB), replace = TRUE))
    inv_b <- invited[take, ]
    inv_b$id <- seq_len(nrow(inv_b))                    # ids must stay unique
    key <- data.frame(id = inv_b$id, orig = invited$id[take])
    resp_b <- merge(key, respondents, by.x = "orig", by.y = "id")  # resampled rows
    vapply(items, function(it) {
      r <- try(fit_heckman_one(inv_b, resp_b, it,
                               type = reg$type[reg$item == it][1]), silent = TRUE)
      if (inherits(r, "try-error") || !isTRUE(r$converged)) NA_real_ else r$est
    }, numeric(1))
  })

  m <- do.call(rbind, reps)
  data.frame(item = items,
             boot_se = apply(m, 2, stats::sd, na.rm = TRUE),
             ci_lo   = apply(m, 2, stats::quantile, 0.025, na.rm = TRUE),
             ci_hi   = apply(m, 2, stats::quantile, 0.975, na.rm = TRUE),
             n_ok    = colSums(!is.na(m)), row.names = NULL)
}

#' Copula robustness models (§7.4)
#'
#' Not implemented yet. GJRM fits Gaussian, Frank, Gumbel and Joe copulas and
#' the AIC-selected model is reported alongside Heckman as a *range*, because
#' ch. 12 finds copulas over-detect: they bound the answer, they do not settle
#' it. Slotted into the pipeline now so the shape of the output is fixed before
#' the data arrive.
fit_copulas <- function(invited, respondents, items, reg = outcome_registry()) {
  data.frame(item = items, est = NA_real_, copula = NA_character_,
             note = "TODO: GJRM (§7.4) - do wdrozenia przed analiza danych rzeczywistych",
             stringsAsFactors = FALSE)
}
