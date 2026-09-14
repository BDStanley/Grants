# ------------------------------------------------------------------
# 03_figs_pl.R — polskie wersje wykresów (figs/pl/)
# Korzysta z zapisanych wyników 02_analyze.R — nie przelicza modeli
# (poza szybkim probitem pierwszego równania dla statystyki z).
# ------------------------------------------------------------------

suppressMessages({
       library(ggplot2)
       library(dplyr)
       library(tidyr)
       library(lmtest)
       library(here)
})

# Wszystkie ścieżki przez here() od korzenia repozytorium, więc skrypt można
# uruchomić z dowolnego katalogu roboczego wewnątrz repo Grants.
demo <- function(...) here("FRBN non-response survey", "demo", ...)

source(demo("R", "theme_plots.R"))
theme_set(theme_plots(base_size = 13))
dir.create(demo("figs", "pl"), recursive = TRUE, showWarnings = FALSE)

fmt <- function(x, d = 1) {
       formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = " ")
}

inv <- read.csv(demo("data", "invitees.csv"), stringsAsFactors = TRUE)
god <- readRDS(demo("data", "godview.rds"))
dg <- read.csv(demo("output", "diagnostic_test.csv"))
res <- read.csv(demo("output", "estimators.csv"))
pol <- read.csv(demo("output", "polarization.csv"))

items_pl <- c(
       turnout = "Pewny udział w wyborach",
       partyS = "Popiera stygmatyzowaną partię",
       minor = "Postawy wobec mniejszości (indeks)",
       norms = "Normy demokratyczne (indeks)",
       eu = "Popiera członkostwo w UE",
       smoke = "Pali papierosy (kontrolne, benchmark)"
)
binary <- c(
       turnout = TRUE,
       partyS = TRUE,
       minor = FALSE,
       norms = FALSE,
       eu = TRUE,
       smoke = TRUE
)
flagged <- c("turnout", "partyS", "minor", "norms")
col2 <- c(
       `Ramię A (kontakt standardowy)` = "grey35",
       `Ramię B (lekki kontakt)` = "#9B0000"
)

# ================= Rys. 1: pierwsze równanie ========================
rr <- inv |>
       group_by(
              arm = ifelse(
                     armA == 1,
                     "Ramię A (kontakt standardowy)",
                     "Ramię B (lekki kontakt)"
              )
       ) |>
       summarise(
              invited = n(),
              completes = sum(responded),
              rr = mean(responded)
       )

fs <- glm(
       responded ~ armA + female + agegrp + edu + town,
       family = binomial(link = "probit"),
       data = inv
)
fs_z <- coeftest(fs)["armA", "z value"]

p1 <- ggplot(rr, aes(arm, rr, fill = arm)) +
       geom_col(width = .55, show.legend = FALSE) +
       geom_text(
              aes(
                     label = paste0(
                            fmt(100 * rr),
                            "%\n(n = ",
                            formatC(completes, big.mark = " "),
                            ")"
                     )
              ),
              vjust = -0.25,
              size = 4.2,
              lineheight = .95,
              family = "Jost"
       ) +
       scale_fill_manual(values = unname(col2)) +
       scale_y_continuous(labels = scales::percent, limits = c(0, .28)) +
       labs(
              title = "Intensywność kontaktu zmienia odsetek odpowiedzi",
              subtitle = sprintf(
                     "Po 15 000 zaproszonych w każdym ramieniu, przydział losowy • statystyka z dla ramienia = %.0f",
                     fs_z
              ),
              x = NULL,
              y = "Odsetek zrealizowanych ankiet wśród zaproszonych"
       )
ggsave(
       demo("figs", "pl", "fig1_response_rates.png"),
       p1,
       width = 7.5,
       height = 5,
       dpi = 200,
       device = ragg::agg_png
)

# ================= Rys. 2: przekrzywiona ryba =======================
set.seed(1)
gs <- god[sample.int(nrow(god), 4000), ]
gs$who <- ifelse(gs$responded == 1, "Odpowiedzieli", "Nie odpowiedzieli")
m_all <- mean(god$minor)
m_resp <- mean(god$minor[god$responded == 1])

p2 <- ggplot(gs, aes(queue, minor, colour = who, alpha = who)) +
       geom_point(size = 1.1) +
       geom_hline(yintercept = m_all, linetype = "dashed") +
       geom_hline(
              yintercept = m_resp,
              linetype = "dotted",
              colour = "#9B0000"
       ) +
       annotate(
              "text",
              x = -3.4,
              y = m_all - .28,
              hjust = 0,
              size = 3.8,
              family = "Jost",
              label = paste0("Prawdziwa średnia w populacji = ", fmt(m_all, 2))
       ) +
       annotate(
              "text",
              x = -3.4,
              y = m_resp + .28,
              hjust = 0,
              size = 3.8,
              family = "Jost",
              colour = "#9B0000",
              label = paste0("Średnia wśród respondentów = ", fmt(m_resp, 2))
       ) +
       scale_colour_manual(
              values = c(
                     Odpowiedzieli = "#9B0000",
                     `Nie odpowiedzieli` = "grey75"
              )
       ) +
       scale_alpha_manual(
              values = c(Odpowiedzieli = .9, `Nie odpowiedzieli` = .35)
       ) +
       labs(
              title = "Respondenci to obciążony wycinek zaproszonych",
              subtitle = "Każdy punkt to zaproszona osoba • odpowiedzi rosną wraz z chęcią udziału • widzimy tylko czerwone punkty",
              x = "Skłonność do odpowiedzi (ukryta — widoczna tylko dzięki danym syntetycznym)",
              y = "Przychylność wobec mniejszości (indeks)",
              colour = NULL,
              alpha = NULL
       )
ggsave(
       demo("figs", "pl", "fig2_tilted_fish.png"),
       p2,
       width = 8,
       height = 5.4,
       dpi = 200,
       device = ragg::agg_png
)

# ================= Rys. 3: test diagnostyczny =======================
dg <- dg |>
       mutate(
              unit_pl = ifelse(
                     binary[item],
                     "Punkty procentowe",
                     "Odchylenia standardowe"
              ),
              label = items_pl[item],
              role_pl = ifelse(
                     item %in% flagged,
                     "Oczekiwane: nieignorowalny",
                     "Kontrolne (oczekiwane zero)"
              )
       )

p3 <- ggplot(dg, aes(est_u, reorder(label, est_u), colour = role_pl)) +
       geom_vline(xintercept = 0, linetype = "dashed") +
       geom_pointrange(
              aes(xmin = est_u - 1.96 * se_u, xmax = est_u + 1.96 * se_u),
              size = .55
       ) +
       facet_wrap(~unit_pl, scales = "free") +
       scale_colour_manual(
              values = c(
                     `Oczekiwane: nieignorowalny` = "#9B0000",
                     `Kontrolne (oczekiwane zero)` = "grey40"
              )
       ) +
       labs(
              title = "Czy respondenci lekkiego kontaktu odpowiadają inaczej?",
              subtitle = "Efekt ramienia B (lekki kontakt) dla każdego pytania, z kontrolą cech demograficznych • 95-proc. przedziały ufności",
              x = "Efekt ramienia B wśród respondentów",
              y = NULL,
              colour = NULL
       )
ggsave(
       demo("figs", "pl", "fig3_diagnostic.png"),
       p3,
       width = 9,
       height = 5.2,
       dpi = 200,
       device = ragg::agg_png
)

# ================= Rys. 4: porównanie estymatorów ===================
res_p <- res |>
       mutate(
              label = factor(items_pl[item], levels = unname(items_pl)),
              est_u = ifelse(binary[item], 100 * est, est),
              se_u = ifelse(binary[item], 100 * se, se),
              estym = recode(
                     estimator,
                     "Observed" = "Obserwowane",
                     "Raked (conventional)" = "Po ważeniu",
                     "Heckman (instrument)" = "Heckman",
                     "Truth" = "Prawda"
              ),
              estym = factor(
                     estym,
                     c("Obserwowane", "Po ważeniu", "Heckman", "Prawda")
              )
       )

p4 <- ggplot(
       res_p |> filter(estym != "Prawda"),
       aes(estym, est_u, colour = estym)
) +
       geom_hline(
              data = res_p |> filter(estym == "Prawda"),
              aes(yintercept = est_u),
              linetype = "dashed"
       ) +
       geom_pointrange(
              aes(ymin = est_u - 1.96 * se_u, ymax = est_u + 1.96 * se_u),
              size = .5,
              show.legend = FALSE,
              na.rm = TRUE
       ) +
       facet_wrap(~label, scales = "free_y", ncol = 3) +
       scale_colour_manual(values = c("grey25", "#0479A8", "#9B0000")) +
       labs(
              title = "Ważenie nie naprawi tego, czego nie widzi — model selekcji tak",
              subtitle = "Linia przerywana = prawdziwa wartość w populacji (znana, bo dane są syntetyczne) • pytania binarne w %, indeksy w punktach",
              x = NULL,
              y = "Oszacowanie"
       ) +
       theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(
       demo("figs", "pl", "fig4_estimators.png"),
       p4,
       width = 10,
       height = 6.2,
       dpi = 200,
       device = ragg::agg_png
)

# ================= Rys. 5: polaryzacja ==============================
pol <- pol |>
       mutate(
              estym = recode(
                     estimator,
                     "Truth" = "Prawda",
                     "Observed poll" = "Sondaż (surowy)",
                     "Raked poll" = "Po ważeniu",
                     "Heckman (instrument)" = "Heckman (instrument)"
              ),
              estym = factor(
                     estym,
                     c(
                            "Sondaż (surowy)",
                            "Po ważeniu",
                            "Heckman (instrument)",
                            "Prawda"
                     )
              )
       )

p5 <- ggplot(pol, aes(estym, gap, fill = estym)) +
       geom_col(width = .6, show.legend = FALSE) +
       geom_text(
              aes(label = fmt(gap)),
              vjust = -0.35,
              size = 4.4,
              family = "Jost"
       ) +
       scale_fill_manual(values = c("grey25", "#0479A8", "#9B0000", "grey70")) +
       scale_y_continuous(limits = c(0, max(pol$gap) * 1.15)) +
       labs(
              title = "Sondaże wyolbrzymiają polaryzację",
              subtitle = "Średnia różnica sympatii do własnej i przeciwnej partii wśród zwolenników partii (punkty na skali 0–100)",
              x = NULL,
              y = "Polaryzacja afektywna"
       )
ggsave(
       demo("figs", "pl", "fig5_polarization.png"),
       p5,
       width = 7.5,
       height = 5,
       dpi = 200,
       device = ragg::agg_png
)

cat("Polskie wykresy zapisane w figs/pl/.\n")
