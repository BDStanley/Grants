# Analysis pipeline — FRBN 67/2026/FRBN/C

The `targets` pipeline and the simulated data-generating process for the pilot
*Pilotażowe wdrożenie randomizowanego instrumentu odpowiedzi w polskim sondażu
społeczno-politycznym*.

Built **simulation-first** (§7.6 of the project design document): every stage of
the analysis runs today, on synthetic data whose population truth is known. In
November the two delivery targets are re-pointed at the agency's files and
nothing else changes. The analysis stage of the timeline is then a re-run rather
than a development sprint.

## Layout

```
Analysis/
├── _targets.R                 the pipeline: stages 0-4 plus the simulation study
├── R/
│   ├── params.R               everything the pre-registration freezes
│   ├── simulate.R             the §7.6 data-generating process
│   ├── prepare.R              Stage 0 — validation and construction
│   ├── stage1_instrument.R    Stage 1 — inclusion condition
│   ├── stage2_diagnostic.R    Stage 2 — the diagnostic test (confirmatory)
│   ├── stage3_selection.R     Stage 3 — Heckman ML, bootstrap, copula slot
│   ├── stage4_compare.R       Stage 4 — raking, comparison, benchmarks, bounds
│   └── simstudy.R             size, power and recovery across scenarios
└── tests/smoke.R              the whole chain in one script, with assertions
```

## Running it

```sh
Rscript tests/smoke.R      # ~20 s, no targets needed: runs and checks the chain
```

```r
# install.packages(c("targets", "tarchetypes"))   # not yet installed
targets::tar_make()
targets::tar_visnetwork()

targets::tar_read(verdicts)     # the per-item verdict — the project's deliverable
targets::tar_read(comparison)   # observed / raked / corrected, against truth
targets::tar_read(power_table)  # size and power, for the pre-registration
targets::tar_read(dgp_check)    # implied selection-outcome correlations
```

Settings that cost time are targets of their own (`n_boot`, `n_rep_test`,
`n_rep_models`), set low for development. Raise them for the report and only
the affected branches rerun.

## The simulated world

`simulate_study()` returns exactly what the agency is contractually obliged to
deliver (§4.4) — an invitee file and a respondent file — plus a god-view truth
table that exists only because the data are synthetic.

- **Population** of adults with plausible demographics (placeholders; replace
  with GUS margins before the prereg freeze — they are also what Stage 4 rakes to).
- **Two unobserved traits**, political engagement and institutional trust, drive
  both response and some answers. This is where non-ignorability comes from.
- **Invited pool** of 18,500, pre-stratified by sex × age × education × region,
  randomised 42/58 into arms A and B — unequal because arm B needs roughly twice
  the invitations to yield the same number of interviews, and the estimand is a
  difference between arms.
- **Response as a queue**: everyone gets one chance at the day-0 invitation;
  arm A's non-responders get two more at the reminders. Propensity persists
  across waves, which is what makes the reminder wave a propensity gradient and
  what makes arm B vs arm A wave 0 a clean placebo.
- **Answers degraded at fieldwork**: break-off, DK/refusal that rises with
  distrust, satisficing that rises with being reminded, straightlining,
  attention-check failures, completion times.

Each outcome has one of three roles, and the pipeline is tested against them:

| role | meaning | expected verdict |
|---|---|---|
| `direct` | shares unobserved variance with response propensity | non-ignorable; weighting fails |
| `indirect` | depends only on covariates that also drive response | ignorable **given X** — but a bivariate `Y ~ Z` test flags it spuriously (Bailey fig. 11.4) |
| `clean` | unrelated to response propensity | ignorable |

The `indirect` items matter as much as the `direct` ones: they are how we check
that the covariate-adjusted test keeps its size. Bailey's fig. 11.4 result — that
the bivariate test false-positives catastrophically — is derived at a much
stronger instrument than ours; at the registered parameters the simulation puts
the bivariate false-positive rate only a little above nominal. The covariate
adjustment is cheap insurance here rather than a dramatic repair, and that is
worth knowing in advance rather than asserting either way.

### Calibration

Response coefficients are not arbitrary. They are set so the implied
selection–outcome correlations sit near Bailey's empirical anchors (ch. 12.2:
ρ̂ ≈ .49 for turnout); `implied_rho()` reports them and `dgp_check` puts them in
the pipeline. The benchmarked recall items are calibrated so the synthetic
population's turnout equals the PKW figure, which makes the Stage 4 benchmark
comparison a real test rather than a coincidence.

Response rates are stated as targets (`rr_wave0`, `rr_arm_a`) and the intercepts
are solved for, so the parameters in the prereg are quantities an agency can
recognise: 8% for a single invitation, 16% after three reminders.

## Scenarios

`scenario_grid()` runs the registered design and the things that could go wrong
with it (§12): a weak instrument, the third-reminder contingency, heavy-tailed
errors (the misspecification of Bailey fig. 11.3), a violated exclusion
restriction, and a smaller realised sample.

## What the simulation already says

The first run changed the design. Numbers below are from 200 replications per
scenario, recorded in `output/` and reproduced by `tar_read(power_table)` and
`tar_read(gap_requirement)`.

**The design as originally specified could not have answered its question.**
Two reminders, a thirteen-item confirmatory family and two-sided tests gave 34%
power on turnout and 8–10% on the stigmatised-party and index hypotheses. The
cause is arithmetic rather than modelling: the implied selection–outcome
correlations are close to Bailey's (ρ ≈ .44 for turnout), but a 5-point
difference in response rate barely changes the composition of the two arms, so
the arm contrast is roughly a third of the cross-propensity-group gaps he
reports. Sample size is not the remedy — power grows with √n and the budget is
fixed, while the gap is something the protocol controls.

**Three changes, compounding.** Four constructs instead of thirteen items and
one-sided tests in the pre-registered direction roughly tripled power; a third
reminder in arm A doubled it again:

| Design | Gap | Turnout | Other three constructs | Global test |
|---|---:|---:|---:|---:|
| Original: 13 items, two-sided, 2 reminders | 4.7 pp | 0.34 | 0.08–0.10 | — |
| Revised analysis, 2 reminders | 4.7 pp | 0.71 | 0.30–0.34 | 0.94 |
| **Revised analysis, 3 reminders** | 7.6 pp | 1.00 | 0.65–0.69 | **1.00** |

The per-construct tests are still short of a conventional 80%, which is why the
primary reported quantity is the estimate and its interval rather than a
verdict, and why the global test carries the headline claim.

**What the gap has to be.** `gap_curve()` sweeps arm A's response rate: 80%
power on the three harder constructs needs a gap around 9–10 points; at 5.6 pp
they sit at 0.42–0.47; at 2.3 pp even the global test is a coin flip (0.43).
This is the number for the clarification calls with the agencies, and the
reason demonstrated reminder lift became a pass/fail tender requirement.

**Three further results worth knowing:**

- *Size is nominal* (0.005–0.06 on the control items), and the revised ±0.10 SD
  equivalence bounds let ignorability be affirmed for the binary controls
  39–55% of the time — against approximately never under the old ±3 pp bound.
- *A three-arm design is not worth asking for.* A 40/20/40 split with a
  one-reminder middle arm costs power on the primary contrast (0.56 against
  0.67–0.69) without a compensating gain.
- *A violated exclusion restriction does asymmetric damage.* A simulated direct
  arm effect of 0.10 SD inflated the three same-signed constructs to power 1.00
  and masked the opposite-signed one entirely (power 0.01; estimated +0.03 SD
  against a true −0.09). It does not merely add noise — it can turn a real
  effect into a null in exactly the direction H2 predicts, which is why the
  wave-0 placebo is reported as a first-class result.

**The gradient test remains much better powered** than the arm contrast
(0.96–1.00 against 0.65–1.00 at the target gap, and 0.47–0.94 where the arm
contrast collapses). It buys that with weaker identification — the reminder wave
a person completed on is not randomly assigned — so it is pre-registered as a
secondary test with field-day controls and reported alongside, never instead.

## Still to do

- `fit_copulas()` is a stub: GJRM models are slotted into the pipeline so the
  output shape is fixed, but not yet implemented (§7.4).
- NINR weights (Sun et al. 2018) — stretch goal, not started.
- Pseudo-non-respondent fallback for the case where the agency cannot deliver
  invitee-level covariates (§4.4 point 4).
- Population margins are placeholders pending GUS figures.
- `external_benchmarks()` carries `verified = FALSE` on every row. Nothing there
  may be quoted until each figure has been checked against its source.
- Hartman–Huang sensitivity (§7.7).

## Reference

Project design document: `../Administration/Project design.md`, §§4–7 and 12.
Method: Bailey, M. A. (2024) *Polling at a Crossroads*, CUP, chs. 8–12.
