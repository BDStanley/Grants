# ------------------------------------------------------------------
# prepare.R — Stage 0: validation and construction (§7.1)
#
# Reconcile the delivered files, verify the randomisation, build the outcome
# variables and indices, and define the primary and sensitivity samples. This
# is the stage that has to be right before anything else is worth looking at.
# ------------------------------------------------------------------

#' Build the index variables from their item batteries
#'
#' Indices are means of the constituent items, computed only where at least
#' `min_items` were answered — a rule fixed ex ante, because "how much item
#' nonresponse is too much" is exactly the kind of decision that should not be
#' made after seeing results.
build_indices <- function(df, min_items = list(minor = 5, norms = 3)) {
  mk <- function(prefix, k, need) {
    cols <- sprintf("%s_%d", prefix, seq_len(k))
    cols <- cols[cols %in% names(df)]
    if (!length(cols)) return(rep(NA_real_, nrow(df)))
    m <- as.matrix(df[, cols, drop = FALSE])
    ok <- rowSums(!is.na(m)) >= need
    out <- rowMeans(m, na.rm = TRUE)
    out[!ok] <- NA_real_
    out
  }
  df$minor <- mk("minor", 8, min_items$minor)
  df$norms <- mk("norms", 5, min_items$norms)
  df
}

#' Build the four confirmatory constructs (§6, revised)
#'
#' The confirmatory family tests four constructs, not twenty items. Two are the
#' existing indices; one is the turnout propensity scale, which is better
#' powered than the binary certainty item and so becomes the primary H1 outcome
#' (the binary stays as a secondary); one pools the two stigmatised-party
#' thermometers, which are on the same 0-10 scale and measure the same thing.
#'
#' Pre-specified rule: a construct is computed where at least one component is
#' answered, in line with the index rule above.
build_constructs <- function(df) {
  df <- build_indices(df)
  df$turnout <- df$sklonfrek
  comp <- c("therm_konf", "therm_kor")
  comp <- comp[comp %in% names(df)]
  if (length(comp)) {
    m <- as.matrix(df[, comp, drop = FALSE])
    out <- rowMeans(m, na.rm = TRUE)
    out[rowSums(!is.na(m)) == 0] <- NA_real_
    df$party_stigma <- out
  }
  df
}

#' Cronbach's alpha, reported for every index (§7.1)
index_alpha <- function(df, prefix, k) {
  cols <- sprintf("%s_%d", prefix, seq_len(k))
  cols <- cols[cols %in% names(df)]
  m <- stats::na.omit(as.matrix(df[, cols, drop = FALSE]))
  if (nrow(m) < 50) return(NA_real_)
  p <- ncol(m)
  (p / (p - 1)) * (1 - sum(apply(m, 2, stats::var)) / stats::var(rowSums(m)))
}

#' Reconcile the invitee file against the field report (§7.1)
#'
#' Denominators per arm per batch. If these do not match what the agency
#' reported, nothing downstream means anything.
reconcile_denominators <- function(invitees) {
  agg <- stats::aggregate(
    cbind(invited = rep(1, nrow(invitees)),
          completes = invitees$disposition == "zrealizowany",
          partials  = invitees$disposition == "przerwany") ~ arm + batch,
    data = invitees, FUN = sum)
  agg$response_rate <- agg$completes / agg$invited
  agg
}

#' Verify the randomisation on profile covariates (§7.1)
#'
#' Standardised differences plus a joint permutation test. A randomisation that
#' fails here is a randomisation that did not happen the way it was described.
#' The design allocates 42/58, not 50/50: what is checked is covariate balance,
#' which does not require equal arm sizes.
check_randomisation_balance <- function(invitees, n_perm = 2000, seed = 1L) {
  vars <- profile_covariates()
  X <- stats::model.matrix(~ . - 1, data = invitees[, vars, drop = FALSE])
  z <- invitees$armB

  std_diff <- vapply(seq_len(ncol(X)), function(j) {
    x <- X[, j]
    (mean(x[z == 1]) - mean(x[z == 0])) /
      sqrt((stats::var(x[z == 1]) + stats::var(x[z == 0])) / 2)
  }, numeric(1))

  fit  <- stats::glm(z ~ X, family = stats::binomial())
  null <- stats::glm(z ~ 1, family = stats::binomial())
  obs  <- stats::deviance(null) - stats::deviance(fit)

  set.seed(seed)
  perm <- replicate(n_perm, {
    zp <- sample(z)
    stats::deviance(stats::glm(zp ~ 1, family = stats::binomial())) -
      stats::deviance(stats::glm(zp ~ X, family = stats::binomial()))
  })

  list(
    std_diff = data.frame(term = colnames(X), std_diff = round(std_diff, 4)),
    max_abs_std_diff = max(abs(std_diff)),
    joint_p = mean(perm >= obs)
  )
}

#' Assemble the analysis frames (§7.1)
#'
#' @return invited      one row per invitee: profile covariates, arm, response
#'                      indicator. This is what the first stage and the
#'                      selection models are estimated on.
#'         respondents  primary sample: completes only, with indices built and
#'                      profile covariates merged on.
#'         sensitivity  quality-filtered sample, used for sensitivity only.
#'                      Filtering rules interact with response propensity, so
#'                      they never touch the primary analysis (§4.3).
build_analysis_data <- function(invitees, respondents, params) {
  resp <- merge(respondents,
                invitees[, c("id", profile_covariates(), "camp", "batch")],
                by = "id", all.x = TRUE)
  resp <- build_constructs(resp)

  primary <- resp[resp$disposition == "zrealizowany", ]

  # pre-specified quality filter: attention check failed, or completed
  # implausibly fast, or straightlined a battery
  drop <- primary$attention_pass == 0 |
          primary$duration < 0.4 * stats::median(primary$duration, na.rm = TRUE) |
          primary$straightline == 1
  sensitivity <- primary[!drop, ]

  invited <- invitees
  invited$responded <- as.numeric(invited$disposition == "zrealizowany")

  list(
    invited     = invited,
    respondents = primary,
    sensitivity = sensitivity,
    dropped_n   = sum(drop, na.rm = TRUE),
    alpha       = c(minor = index_alpha(primary, "minor", 8),
                    norms = index_alpha(primary, "norms", 5))
  )
}

#' Item nonresponse rate per outcome, by arm — an informative secondary outcome
item_nonresponse <- function(respondents, reg = outcome_registry()) {
  items <- intersect(reg$item, names(respondents))
  do.call(rbind, lapply(items, function(it) {
    data.frame(item = it,
               dk_A = mean(is.na(respondents[[it]][respondents$arm == "A"])),
               dk_B = mean(is.na(respondents[[it]][respondents$arm == "B"])))
  }))
}

# ---- reading the real delivery ------------------------------------
# In November these replace the simulated `invitee_file` / `respondent_file`
# targets. They also check the delivery against the contract annex (§4.4),
# which is the moment to find out that something is missing — not three weeks
# later, in the middle of the analysis.

#' Required fields in the invitee file (§4.4)
invitee_contract <- function() {
  c("id", "arm", "batch", "disposition", "wave", "day", profile_covariates())
}

#' Required fields in the respondent file (§4.4)
respondent_contract <- function(reg = outcome_registry()) {
  c("id", "arm", "wave", "duration", "attention_pass")
}

#' Check a delivered file against the contract, loudly
check_delivery <- function(df, required, what = "plik") {
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop(sprintf("%s: brak pol wymaganych umowa (§4.4): %s",
                 what, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Read the agency's invitee file
read_invitee_file <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  check_delivery(df, invitee_contract(), basename(path))
  df$arm  <- factor(df$arm, levels = c("A", "B"))
  df$armB <- as.numeric(df$arm == "B")
  for (v in c("agegrp", "edu", "town", "region", "camp")) {
    if (v %in% names(df)) df[[v]] <- factor(df[[v]])
  }
  df
}

#' Read the agency's respondent file
read_respondent_file <- function(path, reg = outcome_registry()) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  check_delivery(df, respondent_contract(reg), basename(path))
  df$arm  <- factor(df$arm, levels = c("A", "B"))
  df$armB <- as.numeric(df$arm == "B")
  # DK and refusal arrive as 97/98 and must become missing before anything else
  # touches them; item nonresponse is analysed separately, never imputed (§5)
  att <- intersect(c(reg$item, sprintf("minor_%d", 1:8), sprintf("norms_%d", 1:5)),
                   names(df))
  for (v in att) df[[v]][df[[v]] %in% c(97, 98)] <- NA
  df
}
