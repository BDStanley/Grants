# ------------------------------------------------------------------
# _targets.R — the analysis pipeline for FRBN 67/2026/FRBN/C
#
# Built simulation-first (§7.6): every target below runs today, on synthetic
# data whose truth we know. In November the two "delivery" targets are
# re-pointed at the agency's files and the rest of the pipeline is unchanged.
# That is the whole reason for building it this way — the analysis stage of the
# timeline is a re-run, not a development sprint.
#
#   tar_make()            run everything that is out of date
#   tar_visnetwork()      look at the dependency graph
#   tar_read(verdicts)    the per-item verdict table
#   tar_read(power_table) the power analysis for the pre-registration
#
# Stages follow §7.1-7.6 of the project design document.
# ------------------------------------------------------------------

library(targets)

tar_option_set(
  packages = c("sampleSelection", "survey", "ggplot2"),
  format   = "rds",
  seed     = 67L                      # decyzja nr 67/2026/FRBN/C
)

tar_source("R")

list(

  # =================================================================
  # Settings that get turned up for the final run and kept low while
  # developing. Each is a target so that changing one reruns exactly
  # what depends on it and nothing else.
  # =================================================================
  tar_target(n_boot,         50L),    # bootstrap draws for corrected estimates; 2000 for the report
  tar_target(n_rep_test,     200L),   # replications for size/power; 1000+ for the prereg
  tar_target(n_rep_models,   50L),    # replications that also fit selection models

  # =================================================================
  # Registries — the things the pre-registration freezes (§6)
  # =================================================================
  tar_target(params,     dgp_params()),
  tar_target(registry,   outcome_registry()),
  tar_target(constructs, construct_registry()),
  tar_target(reporting_registry, full_registry()),
  tar_target(benchmarks, external_benchmarks()),

  # =================================================================
  # The delivery.
  #
  # THIS IS THE SWAP POINT. Today `study` is a simulated world and the two
  # files are drawn from it. In November:
  #
  #   tar_target(invitee_path,    "data/invitees.csv",    format = "file"),
  #   tar_target(respondent_path, "data/respondents.csv", format = "file"),
  #   tar_target(invitee_file,    read_invitee_file(invitee_path)),
  #   tar_target(respondent_file, read_respondent_file(respondent_path)),
  #
  # The readers check the delivery against the contract annex (§4.4) and fail
  # loudly if a field is missing.
  #
  # and `population_truth_tbl` disappears, because outside a simulation nobody
  # knows the truth. Everything downstream stays as it is.
  # =================================================================
  tar_target(study,                simulate_study(params)),
  tar_target(invitee_file,         study$invitees),
  tar_target(respondent_file,      study$respondents),
  tar_target(pop_margins,          study$margins),
  tar_target(population_truth_tbl, study$truth),
  # DGP validation: the implied selection-outcome correlations, which is how
  # the synthetic world is calibrated against Bailey's empirical anchors
  tar_target(dgp_check,            implied_rho(study)),

  # =================================================================
  # Stage 0 — validation and construction (§7.1)
  # =================================================================
  tar_target(denominators,  reconcile_denominators(invitee_file)),
  tar_target(balance,       check_randomisation_balance(invitee_file)),
  tar_target(analysis_data, build_analysis_data(invitee_file, respondent_file, params)),
  tar_target(invited,       analysis_data$invited),
  tar_target(respondents,   analysis_data$respondents),
  tar_target(sensitivity,   analysis_data$sensitivity),
  tar_target(reliability,   analysis_data$alpha),
  tar_target(dk_by_arm,     item_nonresponse(respondents, registry)),

  # =================================================================
  # Stage 1 — instrument diagnostics (§7.2)
  # The inclusion condition. If this fails, §6's inference rule says the
  # Stage 2 nulls are uninformative and the contingency protocol applies.
  # =================================================================
  tar_target(response_rates, response_rates_by_arm(invited)),
  tar_target(first_stage,    fit_first_stage(invited)),
  tar_target(inclusion,      assess_inclusion(first_stage, response_rates, params)),
  tar_target(wave_curves,    reminder_wave_curves(invited)),

  # =================================================================
  # Stage 2 — the diagnostic test (§7.3)
  # The confirmatory stage. Everything here is pre-registered: the covariate
  # set, the BH family, the equivalence bounds, the placebo and the trend test.
  # =================================================================
  tar_target(mc_check,      manipulation_check(respondents)),

  # the confirmatory family: four constructs, one-sided, BH across four (§6).
  # `global` is the headline claim — is anything in the family non-ignorable —
  # and is far better powered than any single construct.
  tar_target(confirmatory,  run_confirmatory(respondents, constructs)),
  tar_target(global,        global_test(respondents, confirmatory, constructs)),
  tar_target(flagged,       confirmatory$construct[confirmatory$reject]),

  # item-level results, secondary: estimates and equivalence tests
  tar_target(diagnostics,   apply_bh(run_diagnostics(respondents, registry))),
  tar_target(tost,          run_tost(diagnostics)),
  tar_target(verdicts,      flag_items(diagnostics, tost, confirmatory)),

  # robustness and placebos
  tar_target(diag_with_interest,
             apply_bh(run_diagnostics(respondents, registry, extra = "intpol"))),
  tar_target(diag_sensitivity,
             apply_bh(run_diagnostics(sensitivity, registry))),
  tar_target(placebo,       placebo_wave0(respondents, registry)),
  tar_target(gradient,      gradient_trend(respondents, registry)),
  tar_target(quality,       quality_by_arm(respondents)),

  # =================================================================
  # Stage 3 — selection models for the flagged items only (§7.4)
  # Fitted on the invited sample. Corroborating, never primary.
  # =================================================================
  tar_target(heckman,       fit_heckman(invited, respondents, flagged,
                                        reporting_registry)),
  tar_target(heckman_boot,  bootstrap_heckman(invited, respondents, flagged,
                                              n_boot = n_boot,
                                              reg = reporting_registry)),
  tar_target(copulas,       fit_copulas(invited, respondents, flagged,
                                        reporting_registry)),

  # THE PRIMARY DELIVERABLE (§6, revised): estimates with intervals, not
  # verdicts. Survives null tests, and is what the OPUS power analysis needs.
  tar_target(estimates,     estimation_table(confirmatory, heckman, heckman_boot,
                                             population_truth_tbl)),

  # =================================================================
  # Stage 4 — comparison and external validation (§7.5)
  # =================================================================
  tar_target(raked_design,  rake_to_margins(respondents, pop_margins)),
  tar_target(comparison,    compare_estimators(respondents, raked_design, heckman,
                                               heckman_boot, population_truth_tbl,
                                               benchmarks, reporting_registry)),
  tar_target(bounds,        do.call(rbind, lapply(flagged, function(it)
                              manski_bounds(respondents, invited, it,
                                            reporting_registry$type[
                                              reporting_registry$item == it])))),
  tar_target(fig_comparison, plot_estimator_comparison(comparison, flagged)),

  # =================================================================
  # The simulation study (§7.6)
  # Size, power and estimator recovery across the registered design and the
  # things that could go wrong with it. This is what the prereg reports, and
  # it is also the regression test suite: if an edit breaks an estimator, the
  # recovery table notices before a human does.
  # =================================================================
  tar_target(scenario_names,  names(scenario_grid(params))),
  tar_target(scenario_params, scenario_grid(params), iteration = "list"),
  tar_target(
    scenario_runs,
    run_scenario(scenario_names, scenario_params,
                 n_rep = n_rep_test, do_models = TRUE, n_rep_models = n_rep_models),
    pattern  = map(scenario_names, scenario_params),
    iteration = "list"
  ),
  tar_target(power_table,
             summarise_power(do.call(rbind, lapply(scenario_runs, `[[`, "confirmatory")))),
  tar_target(size_table,
             summarise_size(do.call(rbind, lapply(scenario_runs, `[[`, "size")))),
  tar_target(recovery_table,
             summarise_recovery(do.call(rbind, lapply(scenario_runs, `[[`, "models")))),

  # What response-rate gap 80% power actually requires — the number to take
  # into the clarification calls with the agencies.
  tar_target(gap_requirement, gap_curve(params, n_rep = n_rep_test))

  # =================================================================
  # Reporting. Added once the report skeletons exist:
  #
  #   tarchetypes::tar_quarto(report_pl, "reports/raport.qmd"),
  #   tarchetypes::tar_quarto(prereg_power, "reports/power.qmd")
  # =================================================================
)
