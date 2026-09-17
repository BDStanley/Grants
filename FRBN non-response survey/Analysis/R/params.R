# ------------------------------------------------------------------
# params.R — everything the pre-registration freezes
#
# Three registries live here and nowhere else:
#   dgp_params()        parameters of the simulated data-generating process (§7.6)
#   outcome_registry()  the outcomes, their roles and their hypotheses (§5, §6)
#   external_benchmarks()  official figures used for validation (§7.5)
#
# Changing a number here changes the pipeline everywhere downstream, which
# is the point: the prereg cites this file.
# ------------------------------------------------------------------

#' Parameters of the data-generating process
#'
#' Response behaviour is modelled as a queue: everyone gets one chance to
#' respond at the day-0 invitation, and arm A's non-responders get two further
#' chances at the reminders. Individual response propensity persists across
#' waves (that is what makes the reminder wave a propensity gradient), with a
#' modest fresh shock each wave.
#'
#' Intercepts are not set directly. `rr_wave0` and `rr_arm_a` state the response
#' rates we want; the simulation solves for the intercepts that deliver them,
#' so the parameters that appear in the prereg are quantities an agency can
#' recognise and quote.
dgp_params <- function(
    # --- scale -----------------------------------------------------
    n_pop          = 100000,   # adult population the panel represents
    # Pool size and allocation are set by the budget, not by convention. Three
    # reminders raise arm A's response rate, so 20,000 invitations would now
    # yield ~2,400 completes. Completes drive the agency's own cost even though
    # the tender is priced as a fixed total, so that puts a compliant bid under
    # the 21,000 zl cap in doubt. 18,500 invitations split 42/58 towards arm B
    # lands on ~2,000 completes, in line with the original costing — and the
    # unequal split is worth having on its own: equalising *completes* across
    # the arms minimises the standard error of the difference being tested.
    n_invited      = 18500,    # pre-drawn invited pool (§4.2)
    p_arm_a        = 0.42,     # share of the invited pool in the intensive arm
    p_arm_mid      = 0,        # optional middle arm (one reminder); 0 = two-arm design

    # --- response model (§4.1, §4.2) -------------------------------
    rr_wave0       = 0.08,     # response to a single invitation = arm B's RR
    rr_arm_a       = 0.16,     # arm A's RR after its reminders
    n_reminders    = 3L,       # reminders in arm A (§4.1, revised: was 2)
    reminder_days  = c(0, 3, 7, 11),   # contact schedule; both arms close on day 14
    # How much weaker each reminder is than the last, on the probit scale.
    # Calibrated to a plausible declining wave profile (roughly 50/34/12/4 of
    # completes across the day-0 invitation and the three reminders). Replace
    # with the wave profile the winning agency reports.
    reminder_decay = 0.15,
    wave_noise_sd  = 0.45,     # fresh shock per wave; 0 = pure deterministic queue

    # how the latent traits and demographics drive response propensity
    # Calibrated so the implied selection-outcome correlations sit near
    # Bailey's empirical anchors (ch. 12.2: rho-hat = .49 for turnout).
    # See implied_rho() for the check.
    resp_engagement = 1.10,
    resp_trust      = 0.50,
    resp_edu_higher = 0.15,
    resp_age        = 0.006,   # per year, centred at 50
    resp_tenure     = 0.03,    # per year of panel membership
    resp_active     = 0.35,    # recently active panellist

    # --- outcome model ---------------------------------------------
    error_dist     = c("normal", "t"),  # "t" = misspecified errors (Bailey fig. 11.3)
    error_df       = 5,
    beta_z_outcome = 0,        # exclusion-restriction violation; 0 = valid instrument

    # --- questionnaire behaviour (§4.3, §5 M10) ---------------------
    p_breakoff_a   = 0.045,    # break-off among starters, standard contact
    p_breakoff_b   = 0.035,    # break-off among starters, light contact
    dk_base        = 0.04,     # baseline DK/refusal rate on attitude items
    dk_trust       = -0.25,    # distrust raises item nonresponse
    satisfice_a    = 0.20,     # extra satisficing in arm A (reminded respondents)

    # --- pinning the benchmarked items to their official values ----
    # Where an official figure exists, the simulation calibrates that item's
    # intercept so the population truth equals it. That makes the Stage 4
    # benchmark comparison a real test rather than a coincidence. NA = leave
    # the item wherever the model puts it.
    truth_targets = c(rec_turn23 = 0.7438, rec_turn25 = 0.7163,
                      smoke = NA, licence = NA),

    # --- exploratory structure -------------------------------------
    mirror_strength = 0.55,    # within-camp mirror-image bias (Bailey ch. 12.3)

    seed = 67L
) {
  error_dist <- match.arg(error_dist)
  stopifnot(rr_arm_a > rr_wave0, n_invited < n_pop, beta_z_outcome >= 0)
  as.list(environment())
}

#' The outcomes, their design role and the hypothesis each one serves
#'
#' `role` is the ground truth of the simulation and the thing the pipeline is
#' tested against:
#'   direct    the outcome shares unobserved variance with response propensity;
#'             non-ignorable, and no amount of weighting repairs it
#'   indirect  the outcome depends only on covariates that also drive response;
#'             ignorable *conditional on X*, so weighting works — but a
#'             bivariate Y ~ Z test would flag it spuriously (Bailey fig. 11.4)
#'   clean     the outcome is unrelated to response propensity
outcome_registry <- function() {
  r <- function(item, label, type, role, hypothesis, unit = NA_character_) {
    data.frame(item, label, type, role, hypothesis, unit, stringsAsFactors = FALSE)
  }
  rbind(
    # manipulation check (§6 M-check)
    r("intpol",      "Zainteresowanie polityką",              "scale",  "direct",   "MC",  "1-4"),
    # H1 turnout
    r("sklonfrek",   "Skłonność do głosowania (0-10)",        "scale",  "direct",   "H1",  "0-10"),
    r("frekniedz",   "Pewny udziału w wyborach",              "binary", "direct",   "H1",  "%"),
    # H2 stigmatised parties
    r("therm_konf",  "Sympatia: Konfederacja",                "scale",  "direct",   "H2",  "0-10"),
    r("therm_kor",   "Sympatia: Korona",                      "scale",  "direct",   "H2",  "0-10"),
    r("therm_pis",   "Sympatia: PiS (drugorzędne)",           "scale",  "direct",   "H2s", "0-10"),
    r("therm_ko",    "Sympatia: KO",                          "scale",  "indirect", "EX",  "0-10"),
    r("therm_lew",   "Sympatia: Lewica",                      "scale",  "indirect", "EX",  "0-10"),
    # H3 / H4 indices
    r("minor",       "Indeks postaw wobec mniejszości",       "index",  "direct",   "H3",  "SD"),
    r("norms",       "Indeks norm demokratycznych",           "index",  "direct",   "H4",  "SD"),
    # H5 ignorable items
    r("lr",          "Autoidentyfikacja lewica-prawica",      "scale",  "clean",    "H5",  "0-10"),
    r("eu",          "Poparcie dla członkostwa w UE",         "binary", "clean",    "H5",  "%"),
    r("valence1",    "Wydatki na ochronę zdrowia",            "scale",  "indirect", "H5",  "0-10"),
    r("valence2",    "Wydatki na obronność",                  "scale",  "clean",    "H5",  "0-10"),
    r("valence3",    "Podatki a usługi publiczne",            "scale",  "clean",    "H5",  "0-10"),
    # benchmarked behaviours (§5 M9)
    r("smoke",       "Pali papierosy",                        "binary", "indirect", "H5",  "%"),
    r("licence",     "Posiada prawo jazdy",                   "binary", "indirect", "H5",  "%"),
    # recall items, benchmarked against PKW (§5 M4)
    r("rec_turn23",  "Głosował w wyborach do Sejmu 2023",     "binary", "direct",   "BM",  "%"),
    r("rec_turn25",  "Głosował w II turze prezydenckich 2025","binary", "direct",   "BM",  "%"),
    # exploratory: mirror-image structure (Bailey ch. 12.3)
    r("approval",    "Ocena urzędującego prezydenta",         "scale",  "direct",   "EX",  "0-10"),
    r("polar",       "Polaryzacja afektywna (zwolennicy)",    "scale",  "direct",   "EX",  "0-100")
  )
}

#' The four constructs that carry the confirmatory claims (§6, revised)
#'
#' The family was thirteen items and is now four constructs, one test each.
#' Multiplicity was costing about half the power and most of the items were
#' measuring the same four things. Individual items remain in the registry and
#' are reported as secondary.
#'
#' `direction` is the signed prediction from the §6 hypothesis table. Every
#' hypothesis has one, so the confirmatory tests are one-sided: two-sided
#' testing was giving away power for nothing.
construct_registry <- function() {
  data.frame(
    construct  = c("turnout", "party_stigma", "minor", "norms"),
    label      = c("Skłonność do głosowania (0-10)",
                   "Sympatia do partii stygmatyzowanych (indeks)",
                   "Indeks postaw wobec mniejszości",
                   "Indeks norm demokratycznych"),
    hypothesis = c("H1", "H2", "H3", "H4"),
    direction  = c(1, -1, 1, 1),
    components = I(list("sklonfrek", c("therm_konf", "therm_kor"),
                        "minor", "norms")),
    stringsAsFactors = FALSE
  )
}

#' Present the construct registry in the shape the item-level helpers expect
creg_as_registry <- function(creg = construct_registry()) {
  data.frame(item = creg$construct, label = creg$label, type = "scale",
             role = "direct", hypothesis = creg$hypothesis, unit = "SD",
             stringsAsFactors = FALSE)
}

#' Registry of items and constructs together, for the reporting stages
#'
#' Two constructs (`minor`, `norms`) are also registry items; the construct row
#' wins, so every name appears exactly once.
full_registry <- function() {
  cr <- creg_as_registry()
  rbind(cr, outcome_registry()[!outcome_registry()$item %in% cr$item, ])
}

#' Which outcomes belong to the confirmatory family (§6: BH-FDR is applied here)
confirmatory_items <- function(reg = construct_registry()) {
  reg$construct
}

#' Items whose ignorability is claimed affirmatively by equivalence testing
equivalence_items <- function(reg = outcome_registry()) {
  reg$item[reg$hypothesis == "H5"]
}

#' Smallest effect size of interest for the TOST bounds (§6 H5, revised)
#'
#' Stated in standard deviations for every item, binary ones included. The old
#' bound of +/-3 percentage points for binaries required a standard error near
#' 1.5 pp; at n = 2,000 we will have about 1.9, so no binary control could ever
#' have cleared it. In SD units the same bound is about 5 pp at p = 0.5 and 4 pp
#' at p = 0.2 — demanding but attainable, and consistent across item types.
SESOI_SD <- 0.10
sesoi <- function(type) rep(SESOI_SD, length(type))

#' Covariate set for the Stage 2 diagnostic, fixed ex ante (§7.3)
#'
#' Political interest is deliberately excluded: it is outcome-adjacent and is
#' itself a test variable. A with-interest robustness column is reported.
diagnostic_covariates <- function() {
  c("female", "agegrp", "edu", "town", "region")
}

#' Covariates available for non-respondents, i.e. usable in the first stage (§4.4)
profile_covariates <- function() {
  c("female", "agegrp", "edu", "town", "region", "tenure", "active")
}

#' Official figures for external validation (§7.5)
#'
#' TODO before the prereg freeze: verify every figure against the source and
#' set `verified = TRUE`. Nothing here may be quoted in the report while it is
#' still FALSE.
external_benchmarks <- function() {
  data.frame(
    item     = c("rec_turn23", "rec_turn25", "smoke", "licence"),
    benchmark = c(74.38, 71.63, NA, NA),
    source   = c("PKW, Sejm 2023", "PKW, wybory prezydenckie 2025, II tura",
                 "GUS/EHIS - do uzupełnienia", "GUS - do uzupełnienia"),
    verified = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}
