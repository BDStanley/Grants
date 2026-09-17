# ------------------------------------------------------------------
# simulate.R — the §7.6 data-generating process
#
# Simulates the whole study, end to end, in the form the pipeline will meet
# it in November: an invitee file and a respondent file of the kind the agency
# is contractually obliged to deliver (§4.4), plus a god-view truth table that
# exists only because the data are synthetic.
#
# The design mirrors the real one:
#   population            adults the panel is meant to represent
#   invited pool          pre-stratified by sex x age x education x region (§9)
#   arm assignment        random, 42/58 by default (see dgp_params)
#   response              a queue: one chance at the day-0 invitation for
#                         everyone, two further chances at the reminders for
#                         arm A only. Propensity persists across waves, which
#                         is what makes the reminder wave a propensity gradient
#                         (§3, §7.2)
#   answers               generated for the whole population, then degraded at
#                         fieldwork by break-off, DK/refusal and satisficing
#
# Three kinds of outcome, per outcome_registry():
#   direct    shares unobserved variance with response propensity -> non-ignorable
#   indirect  depends only on covariates that also drive response -> ignorable
#             given X, but a bivariate Y ~ Z test flags it spuriously
#   clean     unrelated to response propensity
# ------------------------------------------------------------------

# ---- helpers ------------------------------------------------------

#' Draw standardised errors, normal or heavy-tailed
#'
#' The "t" option is the misspecification Bailey's fig. 11.3 warns about: the
#' Heckman likelihood assumes joint normality, and this is how we find out what
#' it costs us when that is false. Scaled to unit variance so the response rates
#' stay comparable across specifications.
rerr <- function(n, params) {
  if (params$error_dist == "normal") return(stats::rnorm(n))
  df <- params$error_df
  stats::rt(n, df) / sqrt(df / (df - 2))
}

#' Solve for the intercept that delivers a target mean response probability
calibrate_intercept <- function(eta, shock, target) {
  f <- function(a) mean(a + eta + shock > 0) - target
  stats::uniroot(f, c(-12, 12), tol = 1e-6)$root
}

#' Proportional stratified sample, as the agency draws the invited pool
#'
#' Largest-remainder allocation: each stratum gets its floor share, then the
#' remaining places go to the strata with the largest fractional parts, never
#' exceeding the number of people actually in a stratum. (An earlier version
#' absorbed the whole rounding remainder into the first cell, which could ask
#' for a negative number of draws when the first stratum was small.)
stratified_sample <- function(strata, n) {
  strata <- as.factor(strata)
  tab   <- table(strata)
  size  <- as.numeric(tab)
  stopifnot(n <= sum(size))

  exact <- n * size / sum(size)
  take  <- pmin(floor(exact), size)
  short <- n - sum(take)
  if (short > 0) {
    for (i in order(exact - floor(exact), decreasing = TRUE)) {
      if (short == 0) break
      add <- min(size[i] - take[i], short)
      take[i] <- take[i] + add
      short <- short - add
    }
  }

  idx <- unlist(mapply(function(s, k) {
    if (k <= 0) return(integer(0))
    sample(which(strata == s), k)
  }, names(tab), take, SIMPLIFY = FALSE), use.names = FALSE)
  sample(idx)                                    # shuffle so order carries no information
}

#' Solve for the intercept that puts a binary item's population value on target
#'
#' Used for the items validated against official statistics: if PKW says 74.38%
#' voted, the synthetic population votes at 74.38% too, so Stage 4's benchmark
#' comparison tests something.
calib_binary <- function(lin, target) {
  if (is.null(target) || is.na(target)) return(0)
  stats::uniroot(function(a) mean(stats::pnorm(a + lin)) - target, c(-8, 8),
                 tol = 1e-7)$root
}

#' Ordinal battery items around a latent score, 1-5
battery <- function(latent, k, loading, params) {
  vapply(seq_len(k), function(j) {
    z <- loading * latent + sqrt(1 - loading^2) * rerr(length(latent), params)
    as.numeric(cut(z, breaks = c(-Inf, -1.25, -0.5, 0.2, 0.95, Inf), labels = FALSE))
  }, numeric(length(latent)))
}

# ---- 1. population ------------------------------------------------

#' The adult population the panel is meant to represent
#'
#' Demographic margins are plausible placeholders. TODO before the prereg
#' freeze: replace with GUS margins for the adult population, which is also
#' what Stage 4 rakes to.
simulate_population <- function(params) {
  n <- params$n_pop

  female <- stats::rbinom(n, 1, 0.52)
  age    <- 18 + round(67 * stats::rbeta(n, 1.9, 2.1))
  agegrp <- cut(age, c(17, 29, 44, 59, Inf), labels = c("18-29", "30-44", "45-59", "60+"))

  p_hi <- stats::plogis(-0.90 - 0.025 * (age - 50))
  p_lo <- stats::plogis(-1.90 + 0.030 * (age - 50))
  u    <- stats::runif(n)
  edu  <- factor(ifelse(u < p_lo, "podstawowe",
                 ifelse(u < p_lo + (1 - p_lo - p_hi), "srednie", "wyzsze")),
                 levels = c("podstawowe", "srednie", "wyzsze"))
  eduh <- as.numeric(edu == "wyzsze")

  town <- factor(sample(c("wies", "miasto", "duze miasto"), n, TRUE, c(.40, .35, .25)),
                 levels = c("wies", "miasto", "duze miasto"))
  region <- factor(sample(c("centralny", "poludniowy", "wschodni",
                            "polnocno-zachodni", "poludniowo-zachodni", "polnocny"),
                          n, TRUE, c(.19, .21, .15, .17, .10, .18)))

  # panel-profile variables the agency holds for respondents and non-respondents alike
  tenure <- round(stats::rgamma(n, 2, 0.6), 1)
  active <- stats::rbinom(n, 1, stats::plogis(-0.2 + 0.08 * tenure))

  # --- the two unobserved traits, which drive both response and answers ---
  P <- as.numeric(scale(-0.30 * scale(age) + 0.25 * eduh + rerr(n, params)))  # engagement
  T <- as.numeric(scale(0.15 * P + rerr(n, params)))                          # institutional trust

  pop <- data.frame(id = seq_len(n), female, age, agegrp, edu, eduh, town, region,
                    tenure, active, P, T)

  # --- partisan camp: a panel-profile variable, as in Bailey's Ipsos design ---
  p_none <- stats::plogis(-0.60 - 0.80 * P)
  is_part <- stats::rbinom(n, 1, 1 - p_none)
  p_gov  <- stats::plogis(0.10 + 0.45 * T + 0.25 * eduh - 0.008 * (age - 50))
  pop$camp <- factor(ifelse(is_part == 0, "brak",
                     ifelse(stats::rbinom(n, 1, p_gov) == 1, "rzadzacy", "opozycja")),
                     levels = c("brak", "rzadzacy", "opozycja"))

  simulate_outcomes(pop, params)
}

#' The answers every member of the population would give if asked
#'
#' Each outcome's dependence on P and T (direct), on demographics alone
#' (indirect), or on neither (clean) is what outcome_registry() records and what
#' the pipeline is tested against.
simulate_outcomes <- function(pop, params) {
  n <- nrow(pop); e <- function() rerr(n, params)
  P <- pop$P; T <- pop$T; eduh <- pop$eduh; a50 <- pop$age - 50
  female <- pop$female; rural <- as.numeric(pop$town == "wies")

  # --- manipulation check: political interest (4-pt, higher = more interested)
  pop$intpol <- as.numeric(cut(0.85 * P + 0.25 * eduh + 0.45 * e(),
                               c(-Inf, -0.8, 0, 0.8, Inf), labels = FALSE))

  # --- H1 turnout (direct: engagement) ---------------------------------
  pop$sklonfrek <- pmin(10, pmax(0, round(5.6 + 1.9 * P + 0.45 * eduh +
                                          0.020 * a50 + 1.6 * e())))
  lat_turn <- 0.76 * P + 0.18 * eduh + 0.007 * a50 + e()
  pop$lat_turn <- lat_turn
  pop$frekniedz <- as.numeric(lat_turn - 0.05 > 0)
  lin23 <- 0.70 * P + 0.20 * eduh + 0.006 * a50
  lin25 <- 0.72 * P + 0.18 * eduh + 0.006 * a50
  pop$rec_turn23 <- stats::rbinom(n, 1, stats::pnorm(
    calib_binary(lin23, params$truth_targets[["rec_turn23"]]) + lin23))
  pop$rec_turn25 <- stats::rbinom(n, 1, stats::pnorm(
    calib_binary(lin25, params$truth_targets[["rec_turn25"]]) + lin25))

  # --- H2 party sympathy, 0-10 (direct: distrust raises support for the
  #     anti-establishment parties and lowers response propensity) ----------
  therm <- function(base, bP, bT, bX) {
    pmin(10, pmax(0, round(base + bP * P + bT * T + bX + 2.1 * e(), 1)))
  }
  pop$lat_konf <- -0.15 * P - 1.45 * T + 0.35 * (1 - female) - 0.020 * a50 + 2.1 * e()
  pop$therm_konf <- pmin(10, pmax(0, round(3.6 + pop$lat_konf, 1)))
  pop$therm_kor  <- therm(2.4, -0.10, -1.20,  0.30 * (1 - female) - 0.018 * a50)
  pop$therm_pis  <- therm(4.1,  0.05, -0.55,  0.35 * rural + 0.020 * a50 - 0.40 * eduh)
  # indirect: demographics only, but demographics that also drive response
  pop$therm_ko   <- therm(4.6,  0.00,  0.00,  0.75 * eduh - 0.015 * a50 + 0.25 * female)
  pop$therm_lew  <- therm(3.9,  0.00,  0.00,  0.55 * eduh - 0.030 * a50 + 0.30 * female)

  # --- H3 / H4 index batteries (direct: engagement and trust) -------------
  lat_minor <- 0.45 * eduh + 0.35 * P + 0.25 * T - 0.012 * a50 + 0.95 * e()
  lat_norms <- 0.30 * eduh + 0.35 * P + 0.30 * T - 0.005 * a50 + 0.95 * e()
  pop$lat_minor <- lat_minor; pop$lat_norms <- lat_norms
  minor_items <- battery(as.numeric(scale(lat_minor)), 8, 0.72, params)
  norms_items <- battery(as.numeric(scale(lat_norms)), 5, 0.70, params)
  colnames(minor_items) <- sprintf("minor_%d", seq_len(8))
  colnames(norms_items) <- sprintf("norms_%d", seq_len(5))
  pop <- cbind(pop, minor_items, norms_items)

  # --- H5 ignorable items --------------------------------------------
  pop$lr <- pmin(10, pmax(0, round(5.2 + 0.020 * a50 - 0.25 * eduh + 2.3 * e(), 1)))
  pop$eu <- stats::rbinom(n, 1, stats::plogis(1.00 + 0.45 * eduh - 0.010 * a50 +
                                              0.15 * female))
  # indirect: strongly demographic, so a bivariate test would flag it
  pop$valence1 <- pmin(10, pmax(0, round(6.9 + 0.95 * eduh - 0.030 * a50 + 1.8 * e(), 1)))
  pop$valence2 <- pmin(10, pmax(0, round(6.1 + 0.10 * eduh + 0.004 * a50 + 2.0 * e(), 1)))
  pop$valence3 <- pmin(10, pmax(0, round(5.4 - 0.05 * eduh + 0.002 * a50 + 2.2 * e(), 1)))

  # --- benchmarked behaviours (indirect: demographics only) ---------------
  lin_sm <- -0.45 * eduh - 0.008 * a50
  lin_lc <-  0.60 * eduh - 0.020 * a50 - 0.55 * female
  pop$smoke <- stats::rbinom(n, 1, stats::pnorm(
    calib_binary(lin_sm, params$truth_targets[["smoke"]]) - 0.72 + lin_sm))
  pop$licence <- stats::rbinom(n, 1, stats::pnorm(
    calib_binary(lin_lc, params$truth_targets[["licence"]]) + 0.78 + lin_lc))

  # --- exploratory: the mirror image (Bailey ch. 12.3) --------------------
  # Engaged partisans of both camps are more extreme, in opposite directions, so
  # the aggregate can look unbiased while both subgroups are badly biased.
  dir <- ifelse(pop$camp == "rzadzacy", 1, ifelse(pop$camp == "opozycja", -1, 0))
  pop$approval <- pmin(10, pmax(0, round(5.0 + 2.1 * dir +
                                         params$mirror_strength * dir * P +
                                         0.30 * T + 1.7 * e(), 1)))
  pop$polar <- ifelse(pop$camp == "brak", NA,
                      35 + 16 * P + 15 * e())

  pop
}

# ---- 2. fieldwork -------------------------------------------------

#' Draw the invited pool, randomise the arms, and run the contact protocol
#'
#' Returns the population with fieldwork columns attached: arm, the wave each
#' person responded on (NA if never), and disposition.
field_study <- function(pop, params) {
  strata <- interaction(pop$female, pop$agegrp, pop$edu, pop$region, drop = TRUE)
  inv_id <- stratified_sample(strata, params$n_invited)
  inv <- pop[inv_id, ]
  n <- nrow(inv)

  # Arm assignment. Two arms by default; a middle arm (one reminder) is
  # available so the three-level design can be priced against the two-level one
  # before it is asked for.
  u <- stats::runif(n)
  inv$arm <- ifelse(u < params$p_arm_a, "A",
             ifelse(u < params$p_arm_a + params$p_arm_mid, "M", "B"))
  inv$arm <- factor(inv$arm, levels = c("A", "M", "B"))
  inv$armB <- as.numeric(inv$arm == "B")          # Z = 1 for light contact (§3)
  inv$batch <- 1L                                  # top-up batches would appear here

  # latent response propensity, minus the intercept
  eta <- with(inv, params$resp_engagement * P +
                   params$resp_trust      * T +
                   params$resp_edu_higher * eduh +
                   params$resp_age        * (age - 50) +
                   params$resp_tenure     * (tenure - mean(tenure)) +
                   params$resp_active     * active)

  # persistent individual component + a fresh shock at each contact, so response
  # propensity is correlated across waves without being deterministic
  k  <- params$n_reminders
  w  <- params$wave_noise_sd
  xi <- sqrt(max(0, 1 - w^2)) * rerr(n, params)
  shock <- xi + vapply(seq_len(k + 1), function(j) w * rerr(n, params), numeric(n))

  # wave 0: the day-0 invitation, identical in every arm. Calibrated to the
  # response rate a single invitation buys, which is arm B's whole story.
  a0 <- calibrate_intercept(eta, shock[, 1], params$rr_wave0)
  wave <- rep(NA_integer_, n)
  wave[a0 + eta + shock[, 1] > 0] <- 0L

  # Reminders go to the non-responders in the arms that get them: all of them in
  # arm A, the first one only in the middle arm. One free scalar sets reminder
  # strength and is solved for so arm A lands on its target response rate; each
  # successive reminder is weaker by `reminder_decay`.
  n_rem <- ifelse(inv$arm == "A", k, ifelse(inv$arm == "M", 1L, 0L))

  run_waves <- function(r1) {
    wv <- wave
    for (j in seq_len(k)) {
      elig <- is.na(wv) & n_rem >= j
      hit  <- elig & (a0 + r1 - (j - 1) * params$reminder_decay + eta +
                        shock[, j + 1] > 0)
      wv[hit] <- j
    }
    wv
  }
  isA <- inv$arm == "A"
  r1 <- stats::uniroot(
    function(x) mean(!is.na(run_waves(x))[isA]) - params$rr_arm_a,
    c(-8, 8), tol = 1e-6)$root

  inv$wave <- run_waves(r1)
  inv$responded <- as.numeric(!is.na(inv$wave))
  inv$resp_propensity <- a0 + eta                 # god view only, for diagnostics
  inv$resp_latent <- a0 + eta + shock[, 1]        # R* at the day-0 invitation

  # break-off: some starters abandon partway (§7.1 treats partials as non-response)
  p_break <- ifelse(inv$arm == "B", params$p_breakoff_b, params$p_breakoff_a)
  inv$breakoff <- inv$responded * stats::rbinom(n, 1, p_break)
  inv$disposition <- ifelse(inv$responded == 0, "brak odpowiedzi",
                     ifelse(inv$breakoff == 1, "przerwany", "zrealizowany"))

  # completion timing: arm A's completes arrive later, because reminders take
  # time. Field day is a control in the exclusion-restriction robustness spec
  # (§4.3) and the reason the gradient test needs field-day fixed effects.
  sched <- params$reminder_days
  inv$day <- sched[inv$wave + 1] + stats::rbinom(n, 2, 0.4)
  inv$day[is.na(inv$wave)] <- NA

  inv
}

#' Degrade true answers into observed ones: DK/refusal, satisficing, break-off
#'
#' Item nonresponse is informative — it rises with distrust and with the
#' satisficing that being reminded induces — so it is a secondary outcome by
#' arm, not a nuisance to be imputed away (§4.3, §5 M10).
observe_answers <- function(inv, params, reg = outcome_registry()) {
  resp <- inv[inv$disposition == "zrealizowany" | inv$disposition == "przerwany", ]
  n <- nrow(resp)

  # who satisfices: more likely in arm A, where respondents were chased
  p_sat <- stats::plogis(-1.6 + params$satisfice_a * (resp$arm != "B") - 0.25 * resp$P)
  resp$satisficing <- stats::rbinom(n, 1, p_sat)

  attitude_items <- c("intpol", "sklonfrek", "therm_konf", "therm_kor", "therm_pis",
                      "therm_ko", "therm_lew", "lr", "valence1", "valence2", "valence3",
                      "approval", "polar", "frekniedz", "eu",
                      sprintf("minor_%d", 1:8), sprintf("norms_%d", 1:5))

  p_dk <- stats::plogis(stats::qlogis(params$dk_base) + params$dk_trust * resp$T +
                        0.35 * resp$satisficing)
  for (v in attitude_items) {
    miss <- stats::rbinom(n, 1, p_dk) == 1
    resp[[v]][miss] <- NA                          # DK/refusal, kept as missing (codes 97/98)
  }

  # straightlining: satisficers flatten the batteries
  flat <- resp$satisficing == 1 & stats::rbinom(n, 1, 0.45) == 1
  for (b in list(sprintf("minor_%d", 1:8), sprintf("norms_%d", 1:5))) {
    if (any(flat)) {
      anchor <- resp[[b[1]]][flat]
      for (v in b) resp[[v]][flat] <- anchor
    }
  }

  # break-offs lose everything after the halfway point of the questionnaire
  after_break <- c(sprintf("minor_%d", 1:8), sprintf("norms_%d", 1:5),
                   "lr", "eu", "valence1", "valence2", "valence3",
                   "smoke", "licence", "approval", "polar")
  brk <- resp$disposition == "przerwany"
  for (v in after_break) resp[[v]][brk] <- NA

  # exclusion-restriction violation, for the stress-test scenario. Zero by
  # default: randomisation guarantees Z has no direct effect in expectation.
  # Implemented on the continuous items only; binary items are left alone,
  # which is a limitation of the scenario, not of the design.
  if (params$beta_z_outcome != 0) {
    cont <- c("sklonfrek", "therm_konf", "therm_kor", "therm_pis", "therm_ko",
              "therm_lew", "lr", "valence1", "valence2", "valence3", "approval",
              "polar", "intpol", sprintf("minor_%d", 1:8), sprintf("norms_%d", 1:5))
    for (v in cont) {
      resp[[v]] <- resp[[v]] + params$beta_z_outcome *
        stats::sd(resp[[v]], na.rm = TRUE) * resp$armB
    }
  }

  # paradata
  base_dur <- 11.5 - 2.6 * resp$satisficing
  resp$duration <- round(pmax(2, base_dur * exp(0.28 * rerr(nrow(resp), params))), 1)
  resp$attention_pass <- stats::rbinom(nrow(resp), 1,
                                       stats::plogis(2.2 - 1.5 * resp$satisficing))
  resp$straightline <- as.numeric(flat)
  resp
}

# ---- 3. the two files the agency delivers, plus the god view -------

#' Run the whole simulated study
#'
#' @return invitees   one row per invited person: arm, disposition, wave,
#'                    profile covariates. No answers, no latent traits — this
#'                    is the §4.4 invitee file.
#'         respondents one row per complete or partial: the observed answers.
#'         truth      population values of every outcome, known only here.
#'         margins    population margins for raking in Stage 4.
#'         godview    latents and true answers for the invited pool, used by
#'                    the simulation study and by teaching figures. Never by
#'                    the estimation pipeline.
simulate_study <- function(params = dgp_params(), seed = NULL) {
  set.seed(seed %||% params$seed)

  pop <- simulate_population(params)
  inv <- field_study(pop, params)
  obs <- observe_answers(inv, params)

  reg <- outcome_registry()
  answer_cols <- c(setdiff(reg$item, c("minor", "norms")),
                   sprintf("minor_%d", 1:8), sprintf("norms_%d", 1:5))

  invitees <- inv[, c("id", "arm", "armB", "batch", "disposition", "responded",
                      "wave", "day", profile_covariates(), "camp")]

  respondents <- obs[, c("id", "arm", "armB", "wave", "day", "disposition",
                         answer_cols,
                         "duration", "attention_pass", "straightline")]

  list(
    invitees    = invitees,
    respondents = respondents,
    truth       = population_truth(pop, reg),
    margins     = population_margins(pop),
    godview     = inv[, c("id", "arm", "armB", "responded", "wave", "P", "T",
                          "resp_propensity", "resp_latent", "camp",
                          "lat_turn", "lat_konf", "lat_minor", "lat_norms",
                          answer_cols)],
    params      = params
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Population values of every registered outcome — the answer key
population_truth <- function(pop, reg = outcome_registry()) {
  pop <- build_constructs(pop)
  types <- stats::setNames(reg$type, reg$item)
  items <- c(reg$item, construct_registry()$construct)
  items <- items[!duplicated(items)]
  out <- lapply(items, function(it) {
    v <- pop[[it]]
    binary <- identical(unname(types[it]), "binary")
    data.frame(item = it,
               truth  = if (binary) 100 * mean(v, na.rm = TRUE) else mean(v, na.rm = TRUE),
               sd_pop = stats::sd(v, na.rm = TRUE))
  })
  do.call(rbind, out)
}

#' Implied selection-outcome correlation, item by item
#'
#' This is the quantity the Heckman model estimates as rho, computed here from
#' the god view. It is how the DGP is calibrated: Bailey's turnout estimate
#' (ch. 12.2, rho-hat = .49) is the empirical anchor, and the parameters in
#' dgp_params() are set so the synthetic world sits in that neighbourhood
#' rather than wherever arbitrary coefficients happen to land.
implied_rho <- function(sim) {
  g <- sim$godview
  items <- c(frekniedz = "lat_turn", therm_konf = "lat_konf",
             minor = "lat_minor", norms = "lat_norms")
  data.frame(
    item = names(items),
    rho  = vapply(items, function(v) stats::cor(g$resp_latent, g[[v]]), numeric(1)),
    row.names = NULL
  )
}

#' Population margins for conventional raking (§7.5)
population_margins <- function(pop) {
  list(
    female = as.data.frame(table(female = pop$female)),
    agegrp = as.data.frame(table(agegrp = pop$agegrp)),
    edu    = as.data.frame(table(edu    = pop$edu)),
    town   = as.data.frame(table(town   = pop$town)),
    region = as.data.frame(table(region = pop$region))
  )
}
