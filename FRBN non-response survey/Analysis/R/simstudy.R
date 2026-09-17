# ------------------------------------------------------------------
# simstudy.R — the simulation study (§7.6)
#
# The same pipeline, run many times on synthetic data, to answer three
# questions before any real data exist:
#
#   1. Power. How often does the Stage 2 test detect the non-ignorability we
#      built in, at the parameters we are about to register? This is the power
#      analysis the pre-registration reports.
#   2. Size. How often does it fire on items where nonresponse is ignorable —
#      including the "indirect" items that depend on covariates which also
#      drive response, where a bivariate test would wrongly fire (fig. 11.4)?
#   3. Recovery. Do the selection models get back to the truth, and what does
#      it cost when the errors are heavy-tailed or the instrument is weak?
# ------------------------------------------------------------------

#' One replication: simulate a study and run the confirmatory analysis on it
#'
#' @param do_models also fit the selection models for `model_items`. An order of
#'   magnitude slower, so the pipeline runs many cheap replications for the test
#'   and fewer expensive ones for the estimators.
one_replication <- function(params, seed, do_models = FALSE,
                            model_items = construct_registry()$construct,
                            reg = outcome_registry(),
                            creg = construct_registry()) {
  sim <- simulate_study(params, seed = seed)
  ad  <- build_analysis_data(sim$invitees, sim$respondents, params)
  rates <- response_rates_by_arm(ad$invited)

  # --- the confirmatory analysis: four constructs, one-sided, plus the
  #     global test that is now the headline claim
  cf  <- run_confirmatory(ad$respondents, creg)
  gl  <- global_test(ad$respondents, cf, creg)
  cf$reject_two_sided <- stats::p.adjust(cf$p_two_sided, "BH") < 0.05
  cf$seed <- seed
  cf$gap_pp <- rates$gap_pp[1]
  cf$n_resp <- nrow(ad$respondents)
  cf$global_z <- gl$z
  cf$global_reject <- gl$p < 0.05

  # the gradient, pre-registered as a secondary test: better powered, not
  # randomised, and reported alongside rather than instead of the arm contrast
  gr <- gradient_trend(ad$respondents, creg_as_registry(creg))
  cf$p_gradient <- gr$p[match(cf$construct, gr$item)]
  cf$reject_gradient <- cf$p_gradient < 0.05

  # --- size: the control items, where the answer should be "ignorable"
  ctrl_items <- unique(c(equivalence_items(reg),
                         reg$item[reg$role %in% c("clean", "indirect")]))
  ctrl <- apply_bh(run_diagnostics(ad$respondents,
                                   reg[reg$item %in% ctrl_items, ], character()))
  tost <- run_tost(ctrl)
  size <- merge(ctrl[, c("item", "role", "est_sd", "se_sd", "p")],
                tost[, c("item", "p_tost", "equivalent")], by = "item", all.x = TRUE)
  size$seed <- seed
  size$reject_raw <- size$p < 0.05

  models <- NULL
  if (do_models) {
    h <- fit_heckman(ad$invited, ad$respondents, model_items, creg_as_registry(creg))
    models <- data.frame(
      seed = seed, item = model_items,
      truth = sim$truth$truth[match(model_items, sim$truth$item)],
      observed = vapply(model_items,
                        function(it) mean(ad$respondents[[it]], na.rm = TRUE),
                        numeric(1)),
      corrected = h$est[match(model_items, h$item)],
      rho = h$rho[match(model_items, h$item)],
      rho_se = h$rho_se[match(model_items, h$item)],
      z_rho = h$z_rho[match(model_items, h$item)],
      converged = h$converged[match(model_items, h$item)],
      row.names = NULL)
    models$err_observed  <- models$observed  - models$truth
    models$err_corrected <- models$corrected - models$truth
  }

  list(confirmatory = cf, size = size, models = models)
}

#' Run one scenario: `n_rep` replications at a given set of parameters
run_scenario <- function(label, params, n_rep = 200, do_models = FALSE,
                         n_rep_models = 50, base_seed = 6700L) {
  reps <- lapply(seq_len(n_rep), function(i) {
    one_replication(params, seed = base_seed + i,
                    do_models = do_models && i <= n_rep_models)
  })
  bind <- function(k) {
    out <- do.call(rbind, Filter(Negate(is.null), lapply(reps, `[[`, k)))
    if (!is.null(out)) out$scenario <- label
    out
  }
  list(confirmatory = bind("confirmatory"), size = bind("size"),
       models = bind("models"))
}

#' The scenarios the pre-registration reports (§7.6, §12)
#'
#' `base` is the revised design: three reminders in arm A. `two_reminders` is
#' the design as originally specified, kept so the cost of the change is
#' visible. The rest are the things that could still go wrong (§12).
scenario_grid <- function(base = dgp_params()) {
  mod <- function(...) { p <- base; p[names(list(...))] <- list(...); p }
  list(
    base            = base,
    two_reminders   = mod(n_reminders = 2L, reminder_days = c(0, 4, 9),
                          rr_arm_a = 0.13),
    weak_lift       = mod(rr_arm_a = 0.115),          # three reminders, poor lift
    three_arm       = mod(p_arm_a = 0.4, p_arm_mid = 0.2),
    heavy_tails     = mod(error_dist = "t", error_df = 5),
    exclusion_break = mod(beta_z_outcome = 0.10),
    small_sample    = mod(n_invited = 14000)
  )
}

#' Power of the confirmatory tests, by construct and scenario
#'
#' `power_bh` is the pre-registered primary: one-sided, BH across the four
#' constructs. `power_two_sided` shows what two-sided testing would have cost,
#' `power_gradient` what the (non-randomised) gradient test buys, and
#' `power_global` is the rate at which the combined test detects that something
#' in the family is non-ignorable.
summarise_power <- function(confirmatory) {
  agg <- stats::aggregate(
    cbind(power_bh = reject, power_two_sided = reject_two_sided,
          power_gradient = reject_gradient %in% TRUE,
          power_global = global_reject) ~ scenario + construct + hypothesis,
    data = confirmatory, FUN = mean)
  eff <- stats::aggregate(cbind(est_sd, se_sd, gap_pp, n_resp) ~ scenario + construct,
                          data = confirmatory, FUN = mean)
  out <- merge(agg, eff, by = c("scenario", "construct"))
  out[order(out$scenario, -out$power_bh), ]
}

#' Size of the test and the rate at which ignorability can be certified
#'
#' For the control items the rejection rate is the false-positive rate, and
#' `rate_equivalent` is how often TOST can affirm ignorability — the reassuring
#' finding, which is worth as much to a survey programme as the corrections.
summarise_size <- function(size) {
  out <- stats::aggregate(
    cbind(false_positive = reject_raw, rate_equivalent = equivalent %in% TRUE) ~
      scenario + item + role,
    data = size, FUN = mean)
  out[order(out$scenario, out$role, -out$false_positive), ]
}

#' Estimator behaviour: bias, RMSE and how often rho falsely corroborates
summarise_recovery <- function(models) {
  if (is.null(models)) return(NULL)
  out <- stats::aggregate(
    cbind(bias_observed = err_observed, bias_corrected = err_corrected,
          rmse_corrected = err_corrected^2, rho = rho,
          rho_significant = abs(z_rho) > 1.96, converged = converged) ~
      scenario + item,
    data = models, FUN = mean)
  out$rmse_corrected <- sqrt(out$rmse_corrected)
  out
}

#' What response-rate gap does 80% power actually require?
#'
#' Sweeps arm A's response rate while holding arm B's at the rate a single
#' invitation buys, and reports power per construct. This is the number to take
#' into the clarification calls with the agencies: it converts "we would like a
#' bigger reminder lift" into "we need this many points, and here is why".
gap_curve <- function(base = dgp_params(),
                      rr_arm_a = c(0.105, 0.12, 0.14, 0.16, 0.18, 0.20),
                      n_rep = 150) {
  do.call(rbind, lapply(rr_arm_a, function(r) {
    p <- base; p$rr_arm_a <- r
    s <- run_scenario(sprintf("rr_a=%.3f", r), p, n_rep = n_rep)
    agg <- stats::aggregate(cbind(power = reject, power_global = global_reject,
                                  gap_pp = gap_pp, est_sd = est_sd,
                                  n_resp = n_resp) ~ construct,
                            data = s$confirmatory, FUN = mean)
    agg$rr_arm_a <- r
    agg
  }))
}
