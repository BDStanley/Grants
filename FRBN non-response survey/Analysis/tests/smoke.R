# ------------------------------------------------------------------
# tests/smoke.R — run the whole pipeline once, without targets
#
# Same calls as _targets.R, in the same order, on one simulated study. Used to
# check that the chain works before `targets` is installed, and as a fast
# sanity check afterwards:
#
#   Rscript tests/smoke.R
#
# It asserts the things that must be true of the synthetic world: the
# instrument moves response, the diagnostic fires on the items built to be
# non-ignorable and not on the controls, weighting fails to repair the former,
# and the selection model gets closer to the truth than weighting does.
# ------------------------------------------------------------------

suppressMessages({library(sampleSelection); library(survey); library(ggplot2)})
for (f in list.files("R", full.names = TRUE)) source(f)

ok <- function(label, cond) {
  cat(sprintf("%-58s %s\n", label, if (isTRUE(cond)) "OK" else "FAIL"))
  if (!isTRUE(cond)) assign("failures", get0("failures", ifnotfound = 0L) + 1L,
                            envir = globalenv())
}
failures <- 0L
t0 <- Sys.time()

params   <- dgp_params()
registry <- outcome_registry()
study    <- simulate_study(params)

cat("\n-- DGP ------------------------------------------------------\n")
print(implied_rho(study), digits = 2)
ad    <- build_analysis_data(study$invitees, study$respondents, params)
rates <- response_rates_by_arm(ad$invited)
print(rates, digits = 3)
ok("response rates hit their targets",
   abs(rates$rr[rates$arm == "B"] - params$rr_wave0) < 0.015)
ok("arm A responds more than arm B", rates$gap_pp[1] > 2)

ok("primary sample is completes only", nrow(ad$respondents) > 1500)
ok("indices have acceptable reliability", all(ad$alpha > 0.7))

ok("stratified draw returns exactly the requested pool at awkward sizes",
   all(vapply(c(14000, 9999, 7001), function(ni)
     nrow(simulate_study(dgp_params(n_invited = ni, n_pop = 60000),
                         seed = 11)$invitees) == ni, logical(1))))

bal <- check_randomisation_balance(study$invitees, n_perm = 200)
ok("randomisation is balanced", bal$max_abs_std_diff < 0.1 && bal$joint_p > 0.01)

cat("\n-- Stage 1 --------------------------------------------------\n")
fs  <- fit_first_stage(ad$invited)
inc <- assess_inclusion(fs, rates, params)
cat(sprintf("arm coefficient z = %.1f, gap = %.1f pp -> %s\n", inc$z, inc$gap_pp, inc$verdict))
ok("inclusion condition holds", inc$passes)
print(reminder_wave_curves(ad$invited), digits = 3)

cat("\n-- Stage 2: confirmatory (four constructs, one-sided) --------\n")
cf <- run_confirmatory(ad$respondents, construct_registry())
print(cf[, c("construct", "hypothesis", "direction", "n", "est_sd", "se_sd",
             "p_one_sided", "q", "reject")], digits = 3, row.names = FALSE)
gl <- global_test(ad$respondents, cf)
cat(sprintf("global test: Z = %.2f, p = %.5f\n", gl$z, gl$p))
ok("every confirmatory effect has the predicted sign",
   all(sign(cf$est_sd) == cf$direction))
ok("the global test detects the family", gl$p < 0.05)

cat("\n-- Stage 2: item level (secondary) ---------------------------\n")
dg   <- apply_bh(run_diagnostics(ad$respondents, registry))
tost <- run_tost(dg)
verd <- verdict_table(cf, tost, gl)
print(verd[, c("name", "family", "hypothesis", "est_sd", "ci_lo", "ci_hi", "verdict")],
      digits = 2, row.names = FALSE)
items <- flag_items(dg, tost, cf)
print(head(items[, c("item", "role", "est_sd", "ci_lo", "ci_hi", "p")], 5),
      digits = 2, row.names = FALSE)
ok("political interest moves with the arm (manipulation check)",
   manipulation_check(ad$respondents)$p < 0.05)
ok("no control item is declared non-ignorable",
   !any(verd$verdict[verd$family != "konfirmacyjna"] == "nieignorowalny"))
# Whether TOST actually rejects in one run is a coin flip; what the revised
# bounds have to guarantee is that it *can* — that the interval is narrow
# enough to fit inside +/-0.10 SD at this sample size. Under the old +/-3 pp
# bound for binaries this was false by construction.
ok("equivalence bounds are attainable for binary controls",
   all(1.645 * tost$se_sd[tost$item %in% c("smoke", "licence", "eu")] < SESOI_SD))

cat("\n-- Stage 3 --------------------------------------------------\n")
model_items <- construct_registry()$construct
h <- fit_heckman(ad$invited, ad$respondents, model_items, full_registry())
print(h, digits = 3, row.names = FALSE)
ok("selection models converge", all(h$converged))
ok("rho is positive for turnout, negative for the stigmatised party",
   h$rho[h$item == "turnout"] > 0 && h$rho[h$item == "party_stigma"] < 0)

cat("\n-- The primary deliverable: estimates with intervals ----------\n")
est <- estimation_table(cf, h, NULL, study$truth)
print(est[, c("construct", "arm_diff_sd", "ci_lo", "ci_hi", "rho", "rho_lo", "rho_hi")],
      digits = 2, row.names = FALSE)

cat("\n-- Stage 4 --------------------------------------------------\n")
des <- rake_to_margins(ad$respondents, study$margins)
cmp <- compare_estimators(ad$respondents, des, h, NULL, study$truth,
                          external_benchmarks(), full_registry())
show <- cmp[cmp$item %in% c(model_items, "eu", "smoke"),
            c("item", "role", "truth", "observed", "raked", "corrected",
              "err_raked", "err_corrected")]
print(format(show, digits = 4), row.names = FALSE)

flagged_true <- cmp[cmp$item %in% model_items, ]
ok("weighting leaves the flagged items badly wrong",
   mean(abs(flagged_true$err_raked / flagged_true$sd_pop)) > 0.3)
ok("the selection model beats weighting on the flagged items",
   mean(abs(flagged_true$err_corrected)) < mean(abs(flagged_true$err_raked)))

cat("\n-- Simulation study (2 replications, smoke only) -------------\n")
s <- run_scenario("smoke", params, n_rep = 2, do_models = TRUE, n_rep_models = 1)
print(summarise_power(s$confirmatory)[, c("construct", "power_bh", "power_two_sided",
                                          "power_gradient", "power_global")],
      row.names = FALSE)
ok("simulation study returns results",
   nrow(s$confirmatory) > 0 && !is.null(s$models) && nrow(s$size) > 0)

cat(sprintf("\n%d failure(s); %.0f s elapsed\n", failures,
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
quit(status = if (failures > 0) 1 else 0)
