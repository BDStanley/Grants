# ------------------------------------------------------------------
# 02_analyze.R — the Bailey (2024) workflow on the synthetic survey
#
#   Stage 1  First stage: does the randomized contact protocol move
#            response rates? (inclusion condition)
#   Stage 2  Diagnostic test: Y ~ arm + demographics among respondents
#            (Bailey eq. 10.6 — arm differences reveal non-ignorable
#            nonresponse; controls X are essential, fig. 11.4)
#   Stage 3  Estimator comparison per item:
#            truth | observed | conventional raking | Heckman selection
#   Stage 4  Affective polarization: observed vs corrected
# ------------------------------------------------------------------

suppressMessages({
  library(ggplot2); library(dplyr); library(tidyr)
  library(survey);  library(sampleSelection)
  library(sandwich); library(lmtest);  library(here)
})

# All paths resolve via here() from the repo root, so the script can be run
# from any working directory inside the Grants repo.
demo <- function(...) here("FRBN non-response survey", "demo", ...)

dir.create(demo("figs"),   showWarnings = FALSE)
dir.create(demo("output"), showWarnings = FALSE)

inv   <- read.csv(demo("data", "invitees.csv"), stringsAsFactors = TRUE)
resp  <- read.csv(demo("data", "respondents.csv"))
truth <- read.csv(demo("data", "truth.csv"))
aux   <- readRDS(demo("data", "pop_aux.rds"))
god   <- readRDS(demo("data", "godview.rds"))

inv$edu  <- factor(inv$edu,  levels = c("basic", "secondary", "higher"))
inv$town <- factor(inv$town, levels = c("rural", "town", "city"))
resp <- resp |> left_join(inv |> select(id, female, agegrp, edu, town, camp), by = "id")

items <- c(turnout = "Certain to vote",
           partyS  = "Supports stigmatised party",
           minor   = "Pro-minority attitudes (index)",
           norms   = "Democratic norms (index)",
           eu      = "Supports EU membership",
           smoke   = "Smokes (benchmarked control)")
binary  <- c(turnout = TRUE, partyS = TRUE, minor = FALSE,
             norms = FALSE,  eu = TRUE,     smoke = TRUE)
flagged <- c("turnout", "partyS", "minor", "norms")   # non-ignorable by construction

# Shared Jost house theme (theme_plots from the Writing repo). NB the Rscript
# Jost gotcha: assign every plot to an object and pass it explicitly to
# ggsave() with device = ragg::agg_png, never a fallback device.
theme_plots <- function(base_size = 11, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      title = element_text(size = rel(1), family = "Jost", face = "bold"),
      plot.subtitle = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      plot.caption = element_text(
        margin = margin(t = 10),
        size = rel(0.6),
        family = "Jost",
        face = "plain"
      ),
      panel.border = element_rect(
        color = "grey50",
        fill = NA,
        linewidth = 0.15
      ),
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(
        size = rel(0.8),
        vjust = 0.5,
        family = "Jost",
        face = "bold"
      ),
      legend.key.size = unit(0.7, "line"),
      legend.key = element_blank(),
      legend.spacing = unit(0.1, "lines"),
      legend.justification = "left",
      legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
      strip.text = element_text(
        size = rel(0.9),
        hjust = 0,
        family = "Jost",
        face = "plain"
      ),
      strip.background = element_rect(fill = "white", colour = NA),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}
theme_set(theme_plots(base_size = 13))
col2 <- c(`Arm A (standard contact)` = "grey35", `Arm B (light contact)` = "#9B0000")

# ================= Stage 1: first stage ============================
rr <- inv |> group_by(arm = ifelse(armA == 1, "Arm A (standard contact)",
                                   "Arm B (light contact)")) |>
  summarise(invited = n(), completes = sum(responded), rr = mean(responded))

fs <- glm(responded ~ armA + female + agegrp + edu + town,
          family = binomial(link = "probit"), data = inv)
fs_z <- coeftest(fs)["armA", "z value"]

p1 <- ggplot(rr, aes(arm, rr, fill = arm)) +
  geom_col(width = .55, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%\n(n = %s)", 100 * rr,
                                format(completes, big.mark = " "))),
            vjust = -0.25, size = 4.2, lineheight = .95, family = "Jost") +
  scale_fill_manual(values = unname(col2)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .28)) +
  labs(title = "The instrument works: contact intensity moves response",
       subtitle = sprintf("15,000 invited per arm, randomly assigned • first-stage z on arm = %.0f", fs_z),
       x = NULL, y = "Completion rate among invited")
ggsave(demo("figs", "fig1_response_rates.png"), p1, width = 7.5, height = 5, dpi = 200, device = ragg::agg_png)

# ================= Tilted fish (god view) ==========================
set.seed(1); gs <- god[sample.int(nrow(god), 4000), ]
gs$who <- ifelse(gs$responded == 1, "Responded", "Did not respond")
m_all  <- mean(god$minor); m_resp <- mean(god$minor[god$responded == 1])

p2 <- ggplot(gs, aes(queue, minor, colour = who, alpha = who)) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = m_all,  linetype = "dashed") +
  geom_hline(yintercept = m_resp, linetype = "dotted", colour = "#9B0000") +
  annotate("text", x = -3.4, y = m_all - .28, hjust = 0, size = 3.8, family = "Jost",
           label = sprintf("True population mean = %.2f", m_all)) +
  annotate("text", x = -3.4, y = m_resp + .28, hjust = 0, size = 3.8, family = "Jost",
           colour = "#9B0000", label = sprintf("Respondent mean = %.2f", m_resp)) +
  scale_colour_manual(values = c(Responded = "#9B0000", `Did not respond` = "grey75")) +
  scale_alpha_manual(values = c(Responded = .9, `Did not respond` = .35)) +
  labs(title = "Respondents are a biased slice of the invited",
       subtitle = "Each dot is an invitee • answers rise with eagerness to respond • we only ever see the red dots",
       x = "Eagerness to respond (latent — visible only because data are synthetic)",
       y = "Pro-minority attitudes (index)", colour = NULL, alpha = NULL)
ggsave(demo("figs", "fig2_tilted_fish.png"), p2, width = 8, height = 5.4, dpi = 200, device = ragg::agg_png)

# ================= Stage 2: diagnostic test ========================
diag_one <- function(item) {
  f  <- reformulate(c("armB", "female", "agegrp", "edu", "town"), response = item)
  m  <- lm(f, data = resp)
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC2"))["armB", ]
  data.frame(item, est = ct["Estimate"], se = ct["Std. Error"], p = ct["Pr(>|t|)"])
}
dg <- bind_rows(lapply(names(items), diag_one))
dg <- dg |>
  mutate(sd_pop = truth$sd_pop[match(item, truth$item)],
         unit   = ifelse(binary[item], "Percentage points", "SD units"),
         est_u  = ifelse(binary[item], 100 * est, est / sd_pop),
         se_u   = ifelse(binary[item], 100 * se,  se  / sd_pop),
         label  = items[item],
         role   = ifelse(item %in% flagged, "Expected non-ignorable", "Control (expected null)"))
write.csv(dg, demo("output", "diagnostic_test.csv"), row.names = FALSE)

p3 <- ggplot(dg, aes(est_u, reorder(label, est_u), colour = role)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = est_u - 1.96 * se_u, xmax = est_u + 1.96 * se_u), size = .55) +
  facet_wrap(~unit, scales = "free") +
  scale_colour_manual(values = c(`Expected non-ignorable` = "#9B0000",
                                 `Control (expected null)` = "grey40")) +
  labs(title = "The diagnostic: do light-contact respondents answer differently?",
       subtitle = "Effect of Arm B (light contact) on each answer, controlling for demographics • 95% CIs",
       x = "Arm B effect among respondents", y = NULL, colour = NULL)
ggsave(demo("figs", "fig3_diagnostic.png"), p3, width = 9, height = 5.2, dpi = 200, device = ragg::agg_png)

# ================= Stage 3: estimator comparison ===================
# conventional raking to population margins (sex, age group, education, town)
margins <- aux$margins
des <- svydesign(ids = ~1, data = resp |> mutate(female = factor(female)))
des <- rake(des, sample.margins = list(~female, ~agegrp, ~edu, ~town),
            population.margins = list(margins$female |> setNames(c("female", "Freq")),
                                      margins$agegrp, margins$edu, margins$town))

heck_one <- function(item) {
  d <- inv |> left_join(resp |> select(id, all_of(item)), by = "id")
  d$y <- d[[item]]; if (binary[item]) d$y <- factor(d$y)
  m  <- selection(responded ~ armB + female + agegrp + edu + town,
                  y ~ female + agegrp + edu + town, data = d, method = "ml")
  s  <- summary(m)$estimate
  bo <- coef(m, part = "outcome")
  Xo <- model.matrix(~ female + agegrp + edu + town, data = d)
  bo <- bo[colnames(Xo)]
  fit <- as.numeric(Xo %*% bo)
  est <- if (binary[item]) mean(pnorm(fit)) else mean(fit)
  list(est = est, rho = s["rho", 1], rho_se = s["rho", 2])
}

res <- lapply(names(items), function(it) {
  ob <- mean(resp[[it]]); ob_se <- sd(resp[[it]]) / sqrt(nrow(resp))
  w  <- svymean(reformulate(it), des); h <- heck_one(it)
  data.frame(item = it,
             estimator = c("Truth", "Observed", "Raked (conventional)", "Heckman (instrument)"),
             est  = c(truth$truth[truth$item == it], ob, coef(w)[1], h$est),
             se   = c(NA, ob_se, SE(w)[1], NA),
             rho  = h$rho, rho_se = h$rho_se)
}) |> bind_rows()
write.csv(res, demo("output", "estimators.csv"), row.names = FALSE)

res_p <- res |>
  mutate(label = factor(items[item], levels = unname(items)),
         est_u = ifelse(binary[item], 100 * est, est),
         se_u  = ifelse(binary[item], 100 * se,  se),
         estimator = factor(estimator, c("Observed", "Raked (conventional)",
                                         "Heckman (instrument)", "Truth")))
p4 <- ggplot(res_p |> filter(estimator != "Truth"),
             aes(estimator, est_u, colour = estimator)) +
  geom_hline(data = res_p |> filter(estimator == "Truth"),
             aes(yintercept = est_u), linetype = "dashed") +
  geom_pointrange(aes(ymin = est_u - 1.96 * se_u, ymax = est_u + 1.96 * se_u),
                  size = .5, show.legend = FALSE, na.rm = TRUE) +
  facet_wrap(~label, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("grey25", "#0479A8", "#9B0000")) +
  scale_x_discrete(labels = c("Observed", "Raked", "Heckman")) +
  labs(title = "Weighting cannot fix what it cannot see — the selection model can",
       subtitle = "Dashed line = true population value (known because data are synthetic) • binary items in %, indices in points",
       x = NULL, y = "Estimate") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(demo("figs", "fig4_estimators.png"), p4, width = 10, height = 6.2, dpi = 200, device = ragg::agg_png)

# ================= Stage 4: polarization ===========================
d  <- inv |> filter(camp != "none") |> droplevels() |>
  left_join(resp |> select(id, polar), by = "id")
mp <- selection(responded ~ armB + female + agegrp + edu + town + camp,
                polar ~ female + agegrp + edu + town + camp, data = d, method = "ml")
bo <- coef(mp, part = "outcome")
Xo <- model.matrix(~ female + agegrp + edu + town + camp, data = d)
pol_heck <- mean(as.numeric(Xo %*% bo[colnames(Xo)]))
pol_obs  <- mean(resp$polar[resp$camp != "none"], na.rm = TRUE)
pol_w    <- coef(svymean(~polar, subset(des, camp != "none"), na.rm = TRUE))[1]

pol <- data.frame(
  estimator = factor(c("Truth", "Observed poll", "Raked poll", "Heckman (instrument)"),
                     c("Observed poll", "Raked poll", "Heckman (instrument)", "Truth")),
  gap = c(aux$polar_truth, pol_obs, pol_w, pol_heck))
write.csv(pol, demo("output", "polarization.csv"), row.names = FALSE)

p5 <- ggplot(pol, aes(estimator, gap, fill = estimator)) +
  geom_col(width = .6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", gap)), vjust = -0.35, size = 4.4, family = "Jost") +
  scale_fill_manual(values = c("grey25", "#0479A8", "#9B0000", "grey70")) +
  scale_y_continuous(limits = c(0, max(pol$gap) * 1.15)) +
  labs(title = "Polls exaggerate polarisation",
       subtitle = "Mean in-party vs out-party sympathy gap among partisans (0–100 scale points)",
       x = NULL, y = "Affective polarisation gap")
ggsave(demo("figs", "fig5_polarization.png"), p5, width = 7.5, height = 5, dpi = 200, device = ragg::agg_png)

# ================= key numbers for the slides ======================
key <- list(
  rr_A = rr$rr[rr$arm == "Arm A (standard contact)"],
  rr_B = rr$rr[rr$arm == "Arm B (light contact)"],
  n_A  = rr$completes[rr$arm == "Arm A (standard contact)"],
  n_B  = rr$completes[rr$arm == "Arm B (light contact)"],
  fs_z = fs_z, diag = dg, est = res_p, pol = pol,
  rho  = res |> filter(estimator == "Truth") |> select(item, rho, rho_se) |>
           mutate(label = items[item], z = rho / rho_se)
)
saveRDS(key, demo("output", "key_numbers.rds"))

cat("\n--- Diagnostic test (arm B effect, adjusted) ---\n")
print(dg |> select(label, unit, est_u, se_u, p) |> mutate(across(where(is.numeric), ~round(., 3))))
cat("\n--- Estimators vs truth ---\n")
print(res_p |> select(label, estimator, est_u) |>
        pivot_wider(names_from = estimator, values_from = est_u) |>
        mutate(across(where(is.numeric), ~round(., 2))))
cat("\n--- Polarization ---\n"); print(pol)
cat("\nFigures in figs/, tables in output/.\n")
