# ------------------------------------------------------------------
# 01_simulate.R — synthetic data for the non-ignorable nonresponse demo
#
# Simulates a population in which two UNOBSERVED traits drive both
# survey response and (some) survey answers:
#   P = political engagement  (drives response UP, turnout/attitudes UP)
#   T = institutional trust   (drives response UP, stigmatized-party support DOWN)
# Then fields a two-arm survey experiment on a random invitee pool:
#   Arm A (standard contact: invitation + reminders + bonus points) -> high RR
#   Arm B (light contact: single bare invitation)                   -> low RR
#
# Because the data are synthetic, we KNOW the population truth, so we
# can show exactly which estimators recover it and which do not.
# ------------------------------------------------------------------

set.seed(67)                    # decyzja nr 67/2026/FRBN/C

# All paths resolve via here() from the repo root, so the script can be run
# from any working directory inside the Grants repo.
suppressPackageStartupMessages(library(here))
demo <- function(...) here("FRBN non-response survey", "demo", ...)

dir.create(demo("data"), showWarnings = FALSE)

N_POP <- 100000                 # population
N_INV <- 30000                  # invited to the survey (random contact)

# --- 1. Population: demographics -----------------------------------
female <- rbinom(N_POP, 1, 0.52)
age    <- 18 + round(67 * rbeta(N_POP, 1.9, 2.1))
agegrp <- cut(age, c(17, 29, 44, 59, Inf), labels = c("18-29", "30-44", "45-59", "60+"))

# education: three levels, mildly age-dependent
p_hi  <- plogis(-0.90 - 0.025 * (age - 50))
p_lo  <- plogis(-1.90 + 0.030 * (age - 50))
u     <- runif(N_POP)
edu   <- ifelse(u < p_lo, "basic", ifelse(u < p_lo + (1 - p_lo - p_hi), "secondary", "higher"))
edu   <- factor(edu, levels = c("basic", "secondary", "higher"))
eduh  <- as.numeric(edu == "higher")

town  <- factor(sample(c("rural", "town", "city"), N_POP, TRUE, prob = c(.40, .35, .25)),
                levels = c("rural", "town", "city"))

# --- 2. Population: unobserved traits ------------------------------
P <- as.numeric(scale(-0.30 * scale(age) + rnorm(N_POP)))   # engagement
T <- as.numeric(scale(0.15 * P + rnorm(N_POP)))             # trust

# --- 3. Population: survey outcomes --------------------------------
# flagged items (depend on P and/or T -> non-ignorable):
turnout <- rbinom(N_POP, 1, pnorm(-0.05 + 0.76 * P + 0.18 * eduh + 0.007 * (age - 50)))
partyS  <- rbinom(N_POP, 1, pnorm(-1.15 - 0.65 * T + 0.15 * (edu == "basic")
                                  + 0.21 * (1 - female) - 0.007 * (age - 50)))
minor   <- 0.45 * eduh + 0.35 * P + 0.25 * T - 0.012 * (age - 50) + rnorm(N_POP, 0, 0.95)
norms   <- 0.30 * eduh + 0.35 * P + 0.30 * T - 0.005 * (age - 50) + rnorm(N_POP, 0, 0.95)

# control items (depend on observed demographics only -> ignorable):
eu      <- rbinom(N_POP, 1, plogis( 1.00 + 0.45 * eduh - 0.010 * (age - 50) + 0.15 * female))
smoke   <- rbinom(N_POP, 1, plogis(-1.15 - 0.45 * eduh - 0.008 * (age - 50)))

# partisan camp (a *panel profile* variable, known for all invitees, as
# in Bailey's Ipsos design) + affective polarization among partisans
p_none  <- plogis(-0.60 - 0.80 * P)
is_part <- rbinom(N_POP, 1, 1 - p_none)
p_gov   <- plogis(0.10 + 0.45 * T + 0.25 * eduh - 0.008 * (age - 50))
camp    <- ifelse(is_part == 0, "none", ifelse(rbinom(N_POP, 1, p_gov) == 1, "gov", "opp"))
camp    <- factor(camp, levels = c("none", "gov", "opp"))
polar   <- ifelse(camp == "none", NA, 35 + 16 * P + rnorm(N_POP, 0, 15))  # in-party minus out-party sympathy, 0-100 scale points

pop <- data.frame(female, age, agegrp, edu, eduh, town, P, T, camp,
                  turnout, partyS, minor, norms, eu, smoke, polar)

# --- 4. Fieldwork: random contact + randomized contact intensity ----
inv <- pop[sample.int(N_POP, N_INV), ]
inv$id   <- seq_len(N_INV)
inv$armA <- rbinom(N_INV, 1, 0.5)          # 1 = standard contact, 0 = light contact
inv$armB <- 1 - inv$armA

# response: probit-style threshold on engagement, trust, demographics, arm
eta  <- -2.245 + 1.05 * inv$armA + 0.70 * inv$P + 0.60 * inv$T +
         0.15 * inv$eduh + 0.005 * (inv$age - 50)
inv$responded <- as.numeric(eta + rnorm(N_INV) > 0)

# --- 5. Files the survey agency would deliver ----------------------
# (a) invitee file: arm, disposition, profile variables — NO latents, NO answers
invitees <- inv[, c("id", "armA", "armB", "responded",
                    "female", "age", "agegrp", "edu", "town", "camp")]
write.csv(invitees, demo("data", "invitees.csv"), row.names = FALSE)

# (b) respondent file: survey answers for completes only
respondents <- inv[inv$responded == 1,
                   c("id", "armA", "armB", "turnout", "partyS", "minor",
                     "norms", "eu", "smoke", "polar")]
write.csv(respondents, demo("data", "respondents.csv"), row.names = FALSE)

# --- 6. God view (possible only because data are synthetic) ---------
truth <- data.frame(
  item  = c("turnout", "partyS", "minor", "norms", "eu", "smoke"),
  truth = c(mean(pop$turnout), mean(pop$partyS), mean(pop$minor),
            mean(pop$norms),   mean(pop$eu),     mean(pop$smoke))
)
truth$sd_pop <- c(NA, NA, sd(pop$minor), sd(pop$norms), NA, NA)
write.csv(truth, demo("data", "truth.csv"), row.names = FALSE)

polar_truth <- mean(pop$polar[pop$camp != "none"], na.rm = TRUE)
saveRDS(list(polar_truth = polar_truth,
             margins = list(
               female = as.data.frame(table(female = pop$female)),
               agegrp = as.data.frame(table(agegrp = pop$agegrp)),
               edu    = as.data.frame(table(edu    = pop$edu)),
               town   = as.data.frame(table(town   = pop$town)))),
        demo("data", "pop_aux.rds"))

# invitee latents, for the "tilted fish" figure only
god <- inv[, c("id", "armA", "responded", "P", "T", "eduh", "age", "minor", "turnout")]
god$queue <- 0.70 * god$P + 0.60 * god$T + 0.15 * god$eduh + 0.005 * (god$age - 50)
saveRDS(god, demo("data", "godview.rds"))

# --- 7. Console summary ---------------------------------------------
cat(sprintf("Population: %d | Invited: %d\n", N_POP, N_INV))
cat(sprintf("Response rate  Arm A (standard): %.1f%%  (n = %d)\n",
            100 * mean(inv$responded[inv$armA == 1]), sum(inv$responded * inv$armA)))
cat(sprintf("Response rate  Arm B (light):    %.1f%%  (n = %d)\n",
            100 * mean(inv$responded[inv$armA == 0]), sum(inv$responded * (1 - inv$armA))))
cat("\nPopulation truth:\n"); print(truth, digits = 3)
cat(sprintf("\nTrue polarization gap among partisans: %.1f points\n", polar_truth))
