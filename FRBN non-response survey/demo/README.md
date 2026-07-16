# Non-ignorable nonresponse demo (FRBN pilot 67/2026/FRBN/C)

A self-contained demonstration of the problem the FRBN pilot addresses and of
Bailey's (2024) randomized-response-instrument solution, using **synthetic data
where the population truth is known**.

## Contents

```
demo/
├── R/01_simulate.R       simulate population + two-arm survey experiment
├── R/02_analyze.R        diagnostics, raking, Heckman selection models, figures
├── R/03_figs_pl.R        Polish figure versions (from saved outputs; no refits)
├── R/theme_plots.R       shared Jost house plot theme (from the Writing repo)
├── data/                 generated: invitees.csv, respondents.csv, truth.csv, …
├── figs/                 generated: fig1–fig5 (PNG, 200 dpi); figs/pl/ = Polish
├── output/               generated: tables (CSV) + key numbers for the slides
├── presentation.qmd      reveal.js slide deck, English (reads output/, embeds figs/)
├── presentation.html     rendered, fully self-contained — just open/share it
├── presentation_pl.qmd   Polish version of the deck (uses figs/pl/)
└── presentation_pl.html  rendered Polish deck, self-contained
```

## The synthetic world (fixed seed = 67)

100,000 people; two unobserved traits — political engagement and institutional
trust — drive both survey response and some answers. 30,000 are invited;
random 50/50 split into **Arm A** (standard contact: reminders, ~21% RR) and
**Arm B** (light contact: one bare invitation, ~6% RR). Items:

| Item | Depends on latent traits? | Designed verdict |
|---|---|---|
| Certain to vote | engagement ↑ | non-ignorable (overstated in polls) |
| Stigmatized-party support | trust ↓ | non-ignorable (understated) |
| Pro-minority attitudes | engagement + trust ↑ | non-ignorable (overstated) |
| Democratic norms | engagement + trust ↑ | non-ignorable (overstated) |
| EU support | demographics only | ignorable → weighting works |
| Smoking | demographics only | ignorable → weighting works |
| Polarization gap (partisans) | engagement ↑ | exaggerated by polls |

## Regenerate everything

All R scripts resolve paths with `here()` from the Grants repo root, so they can
be run (Rscript or `source()` in Positron) from any working directory inside
the repo:

```sh
Rscript "FRBN non-response survey/demo/R/01_simulate.R"   # ~5 s
Rscript "FRBN non-response survey/demo/R/02_analyze.R"    # ~2 min (six ML fits)
Rscript "FRBN non-response survey/demo/R/03_figs_pl.R"    # ~10 s (Polish figures)
cd "FRBN non-response survey/demo"
quarto render presentation.qmd
quarto render presentation_pl.qmd
```

Requires R (≥ 4.4) with: ggplot2, dplyr, tidyr, survey, sampleSelection,
sandwich, lmtest, scales — and Quarto for the slides.

## Key references

- Bailey, M.A. (2024). *Polling at a Crossroads*. Cambridge UP. (chs. 5–12)
- Heckman, J.J. (1979). Sample selection bias as a specification error. *Econometrica* 47(1).
- DiNardo, McCrary & Sanbonmatsu (2006). Constructive proposals for dealing with attrition.
- Meng, X.-L. (2018). Statistical paradises and paradoxes in big data. *Annals of Applied Statistics* 12(2).
- Sun, Liu, Miao, Wirth, Robins & Tchetgen Tchetgen (2018). *Statistica Sinica* 28.
