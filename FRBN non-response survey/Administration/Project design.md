# Project design: Pilot implementation of a randomized response instrument in a Polish socio-political survey

**FRBN decision 67/2026/FRBN/C** · PI: Ben Stanley · Team: Mikołaj Cześnik, Marta Żerkowska-Balas
**Budget:** 25,830 zł gross (21,000 zł net + 23% VAT), fieldwork only · **Deadline:** all activities complete by **16 December 2026**
**Methodological basis:** Bailey, M.A. (2024). *Polling at a Crossroads: Rethinking Modern Survey Research*. CUP. (esp. chs. 5–12)

---

## 1. What the project must deliver (constraints from the funded application)

The application (wniosek #34, 13 May 2026) commits us to:

1. A **two-arm CAWI experiment** on a Polish online panel: random split of the *invited* sample into a **standard-contact arm (A)** and a **limited-contact arm (B)**, with the agency tagging every completed interview with its arm indicator. Target ≈ 2,000 completes (≈ 1,200 in A, ≈ 800 in B; contractual floor 1,800 with ≥ 650 in B). *(The application specified a 50/50 split; §4.2 revises this to 42/58 on statistical grounds, at the same cost and the same total completes. What the application commits to — a randomised two-arm contact experiment at this scale — is unaffected.)*
2. **Pre-registration** (hypotheses, tests, significance criteria, multiple-testing strategy) *before fieldwork*.
3. SWPS **Research Ethics Committee approval**.
4. A questionnaire mixing items where non-ignorable nonresponse is expected (turnout, support for stigmatized parties, minority attitudes, democratic norms) with **control items** where it is not.
5. **Diagnostic analysis**: regression of each socio-political item on the arm indicator plus demographics; identification of items where the arm indicator is significant.
6. **Selection models** to correct estimates for flagged items; comparison of corrected estimates with conventional weighting.
7. Deliverables: methodological report; article manuscript for POQ / Survey Research Methods / IJPOR; public repository with full R code, replication materials, and an implementation guide; workshop for the PGSW team and CSD staff; a validated protocol ready for a future PGSW round and for NCN OPUS / Horizon Europe scale-up.

Tasks and months in the application map onto the project window as follows (decision signed 13.07.2026; project activities begin **7 September 2026**, so months are calendar months from early September):

| Application task | Content | Calendar |
|---|---|---|
| Zadanie 1 (m. 1) | Questionnaire + pre-registration | early Sep – early Oct |
| Zadanie 2 (m. 1) | Agency procurement, contract, ethics approval | early Sep – mid-Oct |
| Zadanie 3 (m. 2) | Fieldwork, ~2,000 completes; invoice | mid-Nov – early Dec |
| Zadanie 4 (m. 2–3) | Validation + diagnostic regressions | early–mid Dec |
| Zadanie 5 (m. 3) | Selection models, corrected vs weighted estimates | mid-Dec |
| Zadanie 6 (m. 3–4) | Report + manuscript | mid-Dec (manuscript continues past project close) |
| Zadanie 7 (m. 4) | Public repo + PGSW/CSD workshop | early–mid Dec, done by 16.12 |

---

## 2. Conceptual foundation (what the pilot operationalizes)

The design translates Bailey's framework into a Polish CAWI setting. The load-bearing ideas:

- **Non-ignorable nonresponse** exists when the latent propensity to respond, R\*, is correlated with the survey outcome Y (ρ ≠ 0). Weighting on demographics assumes this away; it cannot detect or fix it (chs. 3, 5).
- **Meng decomposition** (eq. 5.1/6.2): error = ρ × √((1−p_r)/p_r) × σ_Y under random contact. With CAWI response rates in single digits, even small ρ produces large bias, so the question "is ρ ≠ 0 for this item?" is empirical and item-specific.
- **Non-ignorable nonresponse leaves observable traces** (ch. 8, fig. 8.3): if two randomly assigned contact protocols produce different response rates, then *differences in observed Y across protocols* indicate ρ ≠ 0 ("tilted fish"); no difference indicates ignorability. This is the entire logic of the pilot.
- **A randomized response instrument** Z must satisfy (ch. 10, eqs. 10.3–10.4):
  - *Inclusion*: Z shifts the probability of response, strongly (weak instruments make selection models useless — ch. 8.5, 10.1);
  - *Exclusion*: Z does not affect answers given response (guaranteed in expectation by randomization, protected in practice by design — §4.3 below).
- **The simple diagnostic test** (eq. 10.6): among respondents, regress Y on Z **plus every covariate X that plausibly affects both response and outcome**. The bivariate version (Y on Z alone) has a catastrophic false-positive rate (fig. 11.4) because Z correlates with X *among respondents* even when randomized; the covariate-adjusted version has correct size (~5%).
- **Selection models** are used when the test flags an item (ch. 11 decision tree, fig. 11.5): Heckman ML as the fixed, interpretable reference (as Bailey does in ch. 12); copula models (GJRM) as parametric-form robustness; NINR weights (Sun et al. 2018) as a stretch goal. All need the first stage estimated on the **full invited sample** (respondents + nonrespondents).
- **If nonrespondent covariates are unavailable**, randomization of Z lets us build **pseudo-nonrespondents** from aggregate margins (ch. 10.5): Pr(X|R=0) = (Pr(X)·N − Pr(X|R=1)·n)/(N−n), and Z ⊥ X among nonrespondents by randomization.
- **Known failure modes to design against** (ch. 11.4): weak instrument; Z affecting Y; non-monotonic R\*–Y relationships (mitigated by covariates spanning response interest and by a graded propensity measure); heterogeneous ρ across subgroups (partisan mirror-images that cancel in the full sample — ch. 12 found exactly this for Trump approval); researcher degrees of freedom (mitigated by pre-registration and full code/data release).
- **Expected empirical patterns** (ch. 12, the closest analogue to ours): turnout intention strongly non-ignorable (ρ̂ ≈ 0.49; observed 78% → Heckman 60% vs. actual 67%); presidential approval ignorable in the full sample but strongly non-ignorable with opposite signs within parties; sensitive attitudes (racial conservatism) non-ignorable and concentrated in specific subgroups; policy items mostly ignorable overall, partisan underneath. Observed polls exaggerate partisan polarization.

**The pilot's contribution:** the first systematic implementation of a randomized contact-intensity response instrument in a Polish (and, to our knowledge, any CEE) commercial CAWI panel, with benchmark validation against known election quantities, producing a reusable protocol for PGSW.

---

## 3. Design overview

```
Panel sample drawn by agency (pre-stratified: sex × age × education × region)
                    │  pre-randomized 42/58 (by us or verified seed)
      ┌─────────────┴──────────────┐
   ARM A (intensive contact)    ARM B (light contact)
   invitation + 3 reminders     single invitation, no reminders
   (days 0, 3, 7, 11)           (day 0)
   same incentive, same questionnaire, same field window (14 days)
      │                            │
   ~1,200 completes            ~800 completes
   (higher RR → includes        (lower RR → only eager,
    reluctant respondents)       high-R* respondents)
      └─────────────┬──────────────┘
                    │
   Respondent-level test: Y ~ Z + X  (Z = arm; X = full covariate set)
   Invited-level first stage: R ~ Z + X_profile  (N ≈ all invitees)
   Flagged items → Heckman ML (reference) + GJRM copulas (robustness)
   Corrected estimates vs. conventional raking vs. external benchmarks
```

Direction conventions used throughout: **Z = 1 for arm B (light contact)**. Arm B respondents are the high-propensity types (Bailey fig. 8.3, *top* panels); arm A adds reluctant types (*bottom* panels). Under ρ > 0 for an item, arm B shows a **higher** observed mean.

Within arm A, the reminder wave on which each respondent completed (0, 1, 2, 3) provides a **graded, protocol-driven response-propensity measure** — a continuum-of-resistance variable in the spirit of Peress (2010) / Bailey §9.4 — at zero extra cost. It supports a second, partially independent diagnostic and guards against non-monotonicity (ch. 11.4 recommends ≥3 propensity levels; we get five: B, A-wave-0, A-wave-1, A-wave-2, A-wave-3, ordered by decreasing eagerness... strictly: B and A-wave-0 have identical stimulus at completion time; their comparison is also a clean no-difference placebo check).

---

## 4. Fieldwork design

### 4.1 The instrument (contact-intensity manipulation)

- **Arm A (intensive):** e-mail/app invitation on day 0; reminders on ~day 3, ~day 7 and ~day 11 to non-completers. **Revised 17 September 2026: three reminders, not two.** The simulation-based power analysis (§7.6) showed that the two-reminder protocol produced a response-rate gap of ~5 pp, at which the confirmatory tests detect the effects we expect 8–10% of the time. The gap, not the number of completes, is the binding constraint on power, and arm A is the only side of it we can move. This takes arm A above business-as-usual, so arm A is no longer a pure benchmark for the panel's normal protocol; the wave-level response curve still recovers what a two-reminder protocol would have yielded, which is what the PGSW recommendation needs.
- **Arm B (light):** the same invitation on day 0 only. Nothing else. Less invasive than standard practice (relevant for ethics: the treatment *reduces* contact burden).
- **Identical in both arms:** incentive (standard panel points), questionnaire, field window (close both arms on day 14), invitation text, sender, subject line, device availability.
- **What must NOT happen:** router-based recruitment ("survey offered on login"), quota-managed fielding, top-ups to one arm only, differential incentives, early closure of one arm. Any of these destroys either the instrument or the denominator. These are contract clauses, not preferences (§9).

### 4.2 Sample sizing and power

Assumptions to be replaced by the agency's empirical rates at RFQ stage. Bidders are now required (§9 W6, mandatory since Modyfikacja nr 1) to supply invitation→complete rates **wave by wave** on comparable 10–12 min political surveys, not just with and without reminders: the shape of the wave profile determines what a third reminder is worth.

- Plausible Polish access-panel rates: RR_B ≈ 7–9% (single invitation), RR_A ≈ 14–17% (three reminders; two reminders typically lift completes by 40–60% over invitation-only, a third adds less).
- **Invite ≈ 18,500 in total, split 42/58 towards arm B** (≈ 7,800 in A, ≈ 10,700 in B). Expected completes ≈ 1,200 (A) and ≈ 800 (B).

  Both numbers changed for reasons worth recording. **The pool shrank because three reminders cost money.** The mechanism is indirect and worth stating precisely: §9 asks for a fixed price for the whole commission, not a price per interview, and bids above 21,000 zł net are rejected outright. Completes nonetheless drive the agency's own cost — incentive points are paid per interview — so a protocol expected to yield ~2,400 completes will be bid higher than one yielding ~2,000. At 20,000 invitations the revised protocol yields ~2,400 completes, which at the ~10.50 zł per interview implied by the original costing is about 25,000 zł of variable cost alone, and puts a compliant bid in doubt. 18,500 invitations land on ~2,000 completes, in line with the costing the budget was built on. **The split became unequal because equal completes beat equal invitations**: the quantity being tested is a difference between arms, its standard error is minimised when the two arms contribute equally, and arm B needs roughly twice the invitations to produce the same number of completes. The unequal split is free — same cost, same field effort — and it buys 0.05–0.15 of power per construct:

  | Configuration | Invited | Completes A / B | Cost at 10.50 zł | Turnout | Other three | Global |
  |---|---:|---:|---:|---:|---:|---:|
  | 17,000, split 50/50 | 17,000 | 1,307 / 675 | 20,811 zł | 0.97 | 0.49–0.55 | 1.00 |
  | **18,500, split 42/58** | 18,500 | 1,193 / 807 | 21,000 zł | 0.97 | **0.58–0.69** | 1.00 |
  | (20,000, split 50/50 — over budget) | 20,000 | ~1,600 / ~800 | ~25,200 zł | 1.00 | 0.65–0.69 | 1.00 |

  It also lifts arm B clear of the 650-complete floor in the tender. Source: `Analysis/output/power_affordable_2026-09-17.csv`.
- **Top-up rule (if completes lag):** release additional *pre-randomized* invitation batches to both arms in the same 42/58 ratio, never selectively; keep the day-0/3/7/11 cadence within each batch. Document batch IDs in the data.

**Power (simulation, 200 replications per scenario; `Analysis/output/power_2026-09-17.csv`).**

| Design | Gap | n | Turnout | Stigmatised party | Minority index | Norms index | Global test |
|---|---:|---:|---:|---:|---:|---:|---:|
| **Design of record** (3 reminders, 4 constructs, one-sided, 18,500 invited split 42/58) | 7.6 pp | 2,000 | 0.97 | 0.69 | 0.60 | 0.58 | **1.00** |
| The same, at 20,000 invited 50/50 (over budget) | 7.6 pp | 2,300 | 1.00 | 0.67 | 0.69 | 0.65 | 1.00 |
| Revised analysis, 2 reminders | 4.7 pp | 2,010 | 0.71 | 0.30 | 0.34 | 0.32 | 0.94 |
| As originally specified (13 items, two-sided, 2 reminders) | 4.7 pp | 2,010 | 0.34 | 0.08 | 0.10 | 0.08 | — |
| Poor lift despite three reminders | 3.3 pp | 1,870 | 0.34 | 0.17 | 0.15 | 0.14 | 0.70 |
| Revised, but only 1,600 completes | 7.5 pp | 1,610 | 0.90 | 0.46 | 0.49 | 0.48 | 1.00 |
| Revised, heavy-tailed errors | 7.5 pp | 2,300 | 0.99 | 0.61 | 0.81 | 0.81 | 1.00 |

Read the third, fourth and fifth rows together: the analysis changes in §6 roughly tripled power on H2–H4, and the protocol change in §4.1 doubled it again. The per-construct tests still fall short of a conventional 80% — which is the honest reason §6 makes the estimate rather than the verdict the primary reported quantity, and why the **global test carries the headline claim**: it is at or near certainty in every scenario except a genuinely failed instrument.

Size is nominal (false-positive rates 0.005–0.06 on the control items) and the revised equivalence bounds let ignorability be affirmed for the binary controls 39–55% of the time, against approximately never under the old ±3 pp bound.

Two further findings from the same run:

- **A three-arm design is not worth asking for.** Splitting the pool 40/20/40 to add a one-reminder middle arm costs power on the primary contrast (0.56 against 0.67–0.69) without a compensating gain, because the middle arm thins the two arms that identify the effect. Not pursued.
- **A violated exclusion restriction is not symmetric in its damage.** Simulating a direct arm effect of 0.10 SD inflated the three same-signed constructs to power 1.00 and *masked* the opposite-signed one entirely (power 0.01, estimated effect +0.03 SD against a true −0.09). A violation therefore does not just add noise: it can turn a real effect into a null in exactly the direction H2 predicts. This is the argument for the §4.3 protections and for reporting the wave-0 placebo as a first-class result.

The remainder of this section is superseded by simulation (§7.6), which supplies the numbers the pre-registration reports. The MDE table that stood here asked the wrong question. It compared a detectable effect against *Bailey's observed effects across propensity groups* — but his contrasts span a far wider range of response propensity than a two-arm contrast at a 5 pp gap does. The right question is what effect **this** design produces, and the simulation answers it directly: the arm difference is a function of how much the two arms differ in composition, and a 5-point difference in response rate moves composition very little.

Two consequences, both of which drove the September revisions:

- **Sample size is not the lever.** Power scales with √n, and the budget is fixed. The response-rate gap is the lever, which is why §4.1 now specifies three reminders and why §9 makes demonstrated reminder lift a mandatory requirement rather than a scoring criterion.
- **The confirmatory design had to change** (§6): four constructs instead of thirteen items, one-sided tests, a global test across the family, and equivalence bounds stated in SD units. Together these recover a large multiple of the power the original specification had.

**Within-party mirror-image tests remain pre-registered as exploratory/directional** and become a headline power justification for the OPUS-scale follow-up.

**First-stage strength, and the go/no-go threshold (revised).** With 10,000 invited per arm even a 3 pp RR difference gives z ≈ 7–10 on the arm coefficient, so the *inclusion condition* is easy to satisfy. That is not the same as being powered, and the previous threshold conflated them: a 4 pp gap passes the inclusion test comfortably and still leaves the confirmatory tests at 8–10% power.

The soft launch (§4.5) therefore checks the gap against a **power** threshold, not a validity one:

The thresholds come from the `gap_curve()` target, which sweeps arm A's response rate and reports power per construct (`Analysis/output/gap_curve_2026-09-17.csv`):

| RR in arm A | Gap | Turnout | Other three constructs | Global test | Action at soft launch |
|---:|---:|---:|---:|---:|---|
| 20% | 11.4 pp | 1.00 | 0.93 | 1.00 | proceed |
| 18% | 9.5 pp | 1.00 | 0.82–0.83 | 1.00 | proceed |
| 16% | 7.6 pp | 1.00 | 0.62–0.68 | 0.99 | **proceed as planned** |
| 14% | 5.6 pp | 0.91 | 0.42–0.47 | 0.97 | **proceed, stating the reduced power; emphasis on the global test and the estimation table** |
| 12% | 3.7 pp | 0.49 | 0.19–0.22 | 0.81 | **invoke the escalation clause: fourth reminder in arm A, hold the launch** |
| 10% | 2.3 pp | 0.16 | 0.08–0.09 | 0.43 | escalate; if the lift cannot be produced, the confirmatory aims are not achievable and the study is reported as an estimation exercise |

In short: **≥ 7 pp proceed, 5–7 pp proceed with caveats, < 5 pp escalate.** Note what the bottom row means — at a 2–3 pp gap even the global test is a coin flip, which is why §9 now makes demonstrated reminder lift a pass/fail tender requirement.

### 4.3 Protecting the exclusion restriction

Randomization guarantees Z ⊥ Y in expectation, but two practical leak paths need design attention:

1. **Timing composition:** arm A completes arrive later on average (post-reminder). If the information environment shifts mid-field, "arm" partly encodes "date". Mitigations: short window (14 days); avoid fielding across major scheduled political events; record completion timestamps; robustness spec with field-day fixed effects; report the A-wave-0 vs B comparison (identical stimulus and near-identical timing) as a placebo.
2. **Response-quality effects:** pestered respondents may satisfice (Fricker & Tourangeau 2010, cited by Bailey ch. 10). We treat data-quality indicators (median completion time, straightlining, item nonresponse, attention-check failure) as **secondary outcomes by arm** — substantively interesting for the protocol recommendation and a check that arm differences in Y are not artifacts of effort. Do **not** filter low-effort respondents asymmetrically: exclusion rules interact with response propensity, so the primary analysis uses all completes, with quality-filtered estimates as sensitivity only (rules fixed ex ante in the pre-registration).

### 4.4 Data contract (deliverables from the agency — contract annex, non-negotiable)

Per **invitee** (pseudonymous ID, all ≈ 18,500):
1. Arm indicator; invitation batch ID; timestamps of invitation and each reminder actually sent.
2. Disposition: completed / partial (with break-off point) / clicked-but-not-started / no click; completion timestamp; device type.
3. Panel-profile covariates: sex, age (or band), education, region (NUTS2), settlement size; plus any panel-held political profile variables (past-vote or party-preference profile fields, if the panel maintains them — Ipsos panel data made Bailey's first stage possible); panel tenure and recent-activity indicator.
4. If invitee-level profiles for nonrespondents are refused (GDPR grounds): **aggregate cross-tabs of the invited pool per arm** (sex × age × education × region × settlement size), which enable the pseudo-nonrespondent method (Bailey §10.5). Get this in writing at RFQ stage; it is the fallback identification strategy.

Per **respondent**: full questionnaire responses with item-level DK/refusal codes preserved (no agency-side imputation), reminder wave at completion (0/1/2), completion duration, and the agency's standard weighting variables if any (we will not use agency weights, but they document practice).

Plus: field report with daily completes per arm; the randomization seed/procedure description (we supply the randomization or independently verify balance); confirmation of no quota management and fixed field window.

### 4.5 Soft launch

Before main launch: ~100 invitations per arm, full protocol compression (reminders on days 1–2 for the pilot batch or excluded from reminder analysis), to verify: arm tagging arrives in the data; paradata fields populated; questionnaire timing ≤ 12 min median; no routing errors. Soft-launch cases kept only if protocol-identical, else discarded (decide ex ante; state in prereg).

---

## 5. Questionnaire blueprint (target: median ≤ 12 minutes, ~60 items)

Modules, with the design role of each:

| # | Module | Content | Role |
|---|---|---|---|
| M1 | Screener & consent | Info sheet, consent, age 18+ | Ethics |
| M2 | Demographics | Sex, age, education, settlement size, region, labour-market status, household income band, religiosity/church attendance | X for test + weighting; church attendance also benchmarkable (CBOS/ISKK) |
| M3 | Political engagement | Political interest (4-pt PGSW wording), campaign attention, political knowledge quiz (3 items), media use | **Positive control** (political interest is the canonical driver of ρ for political items — expect strong arm effect); knowledge quiz mirrors Bailey fig. 1.2 |
| M4 | Turnout & vote | General turnout propensity (0–10), "election next Sunday" turnout (5-pt certainty), vote intention (Sejm), **recalled vote and turnout: 2023 Sejm and 2025 presidential (both rounds)** | Core H1; recall items benchmark against PKW official results |
| M5 | Party evaluations | 0–10 sympathy thermometers: PiS, KO, Polska 2050, PSL, Lewica, Razem, Konfederacja, Korona (Braun); leader thermometers (subset) | Core H2 (stigmatized-party support); enables within-party mirror-image analysis (Bailey fig. 12.11) |
| M6 | Minority attitudes | Short validated batteries: attitudes to LGBT rights (2–3 items), Ukrainian refugees (2–3), Jews/Roma (1–2, EVS/PGSW wording) | Core H3 (social-desirability-loaded) |
| M7 | Democratic norms | 4–6 items: strong leader unbound by parliament/elections (EVS/CSES), court independence, media criticism tolerance, losing-side acceptance, army rule | Core H4 |
| M8 | Ideology & policy controls | Left–right self-placement (0–10), EU membership support, 2–3 low-stigma valence policy items (e.g., healthcare spending) | **Negative controls** (H5: expect no arm effect) |
| M9 | Benchmarked behaviors | 2 items with known population values (e.g., current smoking [GUS/EHIS], driving licence possession) | Negative controls with external truth |
| M10 | Data quality | 1 instructed-response attention check mid-survey; grid-design consistency | Secondary outcomes by arm |

Item sources: PGSW/CSES module wordings wherever available (comparability with PGSW is itself a deliverable), EVS for democratic norms, CBOS wordings for church attendance. Full Polish wording fixed in Task 1; cognitive check within team + 2–3 naive testers.

Design rules: DK/refusal explicitly offered on attitude items (item nonresponse by arm is informative); no forced response except consent and core demographics; randomize item order within M5–M7 batteries; single mid-survey attention check (two would inflate burden asymmetries).

---

## 6. Hypotheses and pre-registration

Pre-register on **OSF** (registration frozen before main launch; soft launch allowed before freeze only for technical verification, no outcome analysis). Contents: design protocol, full questionnaire, hypotheses below, SAP (§7) including estimators/SEs/multiplicity/exclusions, power analysis, contingency rules (third reminder; top-up batches; pseudo-nonrespondent fallback), and the simulated-data pipeline (§7.6).

**The primary reported quantity is an estimate, not a verdict (revised 17 September 2026).** For each construct we report the arm difference in SD units with a 95% confidence interval, and ρ̂ with its interval from the selection model. Hypothesis tests are secondary to that table. The reason is the power analysis in §7.6: a pilot that reports "ρ̂ = 0.31, 95% CI [0.05, 0.55]" has produced something the methodological report, the manuscript and the OPUS power analysis can all use, whether or not a test clears its threshold, whereas a pilot that reports six rows of "indeterminate" has not. This also matches what the funded application promised — a *pilotażowe wdrożenie*, not a definitive census of Polish survey items.

**Confirmatory tests: four constructs, one-sided** (coefficient on Z [= arm B] in Y ~ Z + X among respondents; BH-FDR 5% across the four). The family was thirteen items and is now four constructs. Multiplicity was costing roughly half the power, most of the items measured the same four things, and every hypothesis carries a signed prediction, so two-sided testing was giving away more power for nothing. A result significant in the *wrong* direction is reported as a failure of the hypothesis, not as a discovery.

| H | Construct | Composition | Expectation | Rationale |
|---|---|---|---|---|
| H1 | Turnout propensity | `sklonfrek`, 0–10 (the binary "next Sunday" certainty becomes secondary: the scale is better powered) | β_Z > 0 | Civic engagement ↔ response propensity; the most robust finding in the literature (Bailey ch. 12.2: ρ̂ = .49) |
| H2 | Stigmatised-party sympathy | mean of Konfederacja and Korona thermometers, 0–10 (PiS secondary) | β_Z < 0 | Anti-establishment distrust lowers response propensity among these electorates ("shy" pattern via nonresponse, not lying) |
| H3 | Minority attitudes (progressive-scored index) | 8 items | β_Z > 0 | Socially conservative views concentrated among low-R\* types (Bailey ch. 12.4) |
| H4 | Democratic norms (liberal-scored index) | 5 items | β_Z > 0 | Norm-committed citizens overrepresented at high R\* |

**Global test (new, and the headline confirmatory claim).** The four directional test statistics are combined by Brown's method, using the correlation of the outcome residuals among respondents to account for the four constructs being measured on the same people. This answers "is *anything* in this family non-ignorable" and is far better powered than any single construct; the per-construct tests then say where.

**H5 (ignorable items).** Left–right self-placement, EU support, three low-stigma valence items and two benchmarked behavioural items: β_Z ≈ 0, tested by **TOST equivalence with SESOI ±0.10 SD for every item, binary ones included.** The old bound of ±3 pp for binaries needed a standard error near 1.5 pp; at n ≈ 2,000 we will have about 1.9, so no binary control could ever have cleared it and the reassuring finding — certifying an item as clean — would have been unobtainable by construction. In SD units the same bound is about 5 pp at p = 0.5 and 4 pp at p = 0.2. Ignorability claims remain affirmative, never absence-of-significance.

**M-check (political interest).** β_Z > 0, sizable. Positive control; if null, the instrument is too weak and H1–H4 nulls are uninformative (pre-specified inference rule). Note the cliff this creates: the simulation puts the failure rate of this check at roughly one run in five at the two-reminder protocol, which is a further argument for the three-reminder revision in §4.1.

**Secondary confirmatory test: the four-level propensity gradient (pre-registered now, §3).** Rejection rates of 0.64–1.00 against 0.20–0.72 for the arm contrast on the same items — it is the best-powered test available. It is registered *now*, before any data exist, precisely because reaching for it after a null arm contrast would be indefensible. It buys its power with weaker identification: the reminder wave a person completed on is not randomly assigned, so it is confounded with time in field. It is reported alongside the arm contrast, never instead of it, always with field-day fixed effects, and the arm-B vs arm-A-wave-0 placebo carries the defence.

**Exploratory (pre-registered as such, no confirmatory claims):** within-party mirror-image effects (PiS vs KO vs Konfederacja identifiers; Bailey ch. 12.3 logic); heterogeneity by education and political interest strata (non-monotonicity guard); the four-level propensity gradient (B / A-wave-0 / A-wave-1 / A-wave-2) with test for monotonic trend; item-nonresponse rates by arm; data-quality outcomes by arm; A-wave-0 vs B placebo (expect null).

**Interpretation rules fixed ex ante** (avoids post-hoc spin): (a) confirmatory claims only from the H1–H5 family with BH correction; (b) an item "shows non-ignorable nonresponse" if the covariate-adjusted arm test rejects at BH-5% — selection-model ρ̂ significance is corroborating, not primary (Heckman rejects too often under misspecification; fig. 11.4 favors the adjusted nonparametric test); (c) "ignorable" is claimed only where TOST rejects the ±0.10 SD bound; in-between results are reported as indeterminate; (d) corrected point estimates are reported with the reminder that identification leans on functional form (ch. 12's practice: Heckman as reference, copulas as robustness range).

---

## 7. Statistical analysis plan

All analysis in R (≥ 4.4), pipeline built with `targets`, environment pinned with `renv`, report in Quarto. Everything written and tested against **simulated data before fieldwork ends** (§7.6).

### 7.1 Stage 0 — validation & construction
- Reconcile invitee file with field report (denominators per arm per batch); verify that the realised allocation matches the 42/58 design and check randomization balance on profile covariates (standardized differences; joint permutation test).
- Construct outcome variables and indices (pre-specified codings; indices as means of standardized items, reliability reported).
- Primary sample: all completes. Sensitivity sample: quality-filtered (rules from prereg). Partials treated as nonrespondents (sensitivity: as respondents for items answered before break-off).

### 7.2 Stage 1 — instrument diagnostics (invited-level, N ≈ 20k)
- RR by arm; response probit R ~ Z + X_profile (+ batch fixed effects). Report the Z coefficient, implied pp effect, and z-statistic — the **inclusion condition** (target: |z| ≫ 10, per Bailey tab. 12.1 where z = −23.8).
- Wave-level response curves in arm A (lift per reminder) — feeds the PGSW protocol recommendation and the four-level gradient.

### 7.3 Stage 2 — the diagnostic test (respondent-level)
- For each pre-registered outcome: OLS (linear probability for binaries; ordered-logit sensitivity) of Y on Z + X, HC2 SEs. **X set (fixed ex ante):** sex, age, age², education, settlement size, region, income band, religiosity, labour-market status. (Political interest is *excluded* from X here — it is outcome-adjacent and itself a test variable; noted as a limitation with a with/without-interest robustness column.)
- One-sided tests in the pre-registered direction for the four constructs; BH-FDR 5% across those four; Brown-combined global test across the family; TOST (±0.10 SD) for H5 items; standardized effect sizes with 95% CIs for everything (the CI, not the star, is the product — and after the September revision, the CI *is* the primary result).
- Placebo: A-wave-0 vs B. Trend: four-level propensity gradient.

### 7.4 Stage 3 — selection models for flagged items
- **Heckman ML** (reference; `sampleSelection::selection`): first stage R ~ Z + X_profile on invited sample; outcome equation Y ~ X. Report ρ̂ with CI, corrected population estimates (fitted values over the full invited sample, post-stratified to population margins), and the **R²_M multicollinearity diagnostic** (regress inverse Mills ratio on outcome-equation covariates; report as a matter of course, per ch. 8.6).
- **Copula robustness** (`GJRM`): Gaussian, Frank, Gumbel, Joe (+ rotations); AIC-selected model reported alongside Heckman, presented as a **range** of corrected estimates (ch. 12 warns copulas over-detect; they bound, not adjudicate).
- **NINR weights** (Sun et al. 2018; stretch goal): method-of-moments implementation adapting Bailey's replication code (Bailey 2023); report if stable, else document non-convergence honestly.
- **Fallback if invitee-level nonrespondent X unavailable:** pseudo-nonrespondents from aggregate margins (ch. 10.5), with multiple imputation over pseudo-pool draws (footnote 9 of ch. 10).
- Uncertainty: nonparametric bootstrap over the invited sample (stratified by arm) for corrected estimates.

### 7.5 Stage 4 — comparison & external validation
- Three estimate columns per item: (1) observed unweighted; (2) **conventional raking** (sex × age, education, settlement size, region — standard Polish practice; `survey::rake`); (3) selection-corrected (Heckman reference + copula range).
- **Benchmarks:** recalled 2023 Sejm vote and turnout vs PKW official results (turnout 74.38%); recalled 2025 presidential vote (both rounds; runoff turnout 71.63%, first round 67.31%; verify exact PKW figures during implementation); smoking and driving-licence items vs GUS/EHIS; church attendance vs CBOS/ISKK. Metric: absolute error of each estimator vs benchmark, with recall-error caveats stated (benchmarks validate direction and rough magnitude, not decimal precision — Peress's validated-turnout logic).
- Headline exhibit (mirrors Bailey figs. 12.4/12.6): estimator comparison plot per flagged item.

### 7.6 Simulation-first development
Before data arrive: simulate the full design (invited pool with profile X; latent R\* = γ_Z Z + γ_X X + τ; outcomes with direct and indirect non-ignorability pathways; ch. 11.1 style) at registered parameter values; run the entire `targets` pipeline end-to-end on simulated data; verify size/power of the test and recovery by Heckman/GJRM under correct and misspecified errors (t-distributed, per fig. 11.3). The simulation doubles as (a) the power analysis in the prereg, (b) the regression test suite for the pipeline, (c) teaching material for the workshop.

**Implemented September 2026** in `Analysis/` (targets pipeline, `R/simulate.R` for the data-generating process, `tests/smoke.R` for the end-to-end check). It did its job before fieldwork rather than after: the first run showed the design as originally specified had 8–10% power on H2–H4, which is what prompted the revisions to §4.1, §4.2, §6 and §9 recorded above. Parameters are calibrated so the implied selection–outcome correlations sit near Bailey's empirical anchors (ρ ≈ .44 for turnout against his .49), and the benchmarked recall items are pinned to the PKW figures so the Stage 4 comparison tests something. The `gap_curve()` target reports power per construct across candidate response rates and is the source of the §4.2 threshold.

### 7.7 Sensitivity & bounds (supplementary)
- Manski bounds for headline items at observed response rates (ch. 7.1) — honesty exhibit for the report.
- Hartman–Huang-style sensitivity for one or two items (posit population distributions of a partially observed variable, e.g., political interest; re-rake; show how conclusions move) — connects the pilot to the no-instrument world PGSW currently inhabits.

---

## 8. Timeline (7 September → 16 December 2026)

| Weeks (2026) | Workstream | Milestones |
|---|---|---|
| **22 Sep – 30 Oct** | Questionnaire cognitive testing then final version; ethics application to SWPS KEB (light-contact arm framed as burden-*reducing*); English translation of the instrument | Ethics submitted 24 Sep; questionnaire final 9 Oct; ethics decision by 30 Oct |
| **29 Sep – 23 Oct** | RFQ with contract annex (Załącznik 2) to ≥3 agencies (Ariadna, Opinia24, Pollster, + Norstat/IQS as backups); bidder questions answered; bids evaluated (criteria §9); clarification calls; contract signed | RFQ out 29 Sep; bids 15 Oct; contract 23 Oct |
| **26 Oct – 4 Nov** | Questionnaire handed to agency; agency scripting; our script testing; randomization verified | Script signed off 4 Nov |
| **5 – 13 Nov** | **Soft launch** (~100/arm); reminder-lift verified; go/no-go decision; prereg **frozen on OSF before main launch** | Go/no-go 12 Nov; prereg frozen 13 Nov |
| **16 – 30 Nov** | **Main fieldwork** (14-day window, both arms; opens after 11 Nov Independence Day so no public holiday falls inside it); reminders day 3/7/11 (19, 23, 27 Nov); daily monitoring of per-arm completes; top-up batches in the same 42/58 ratio if needed | ~2,000 completes; floor 1,800 with ≥ 650 in arm B |
| **30 Nov – 14 Dec** | Data delivery (≤ 3 working days after close, i.e. by 3 Dec); acceptance protocol; **agency invoice** (coordinate FRBN invoice timing with Dział Badań); Stage 0–5 per SAP | Data 3 Dec; acceptance 9 Dec; diagnostics 9 Dec; Stage 5 by 14 Dec |
| **9 – 16 Dec** | Methodological report (PL, incl. journalist-accessible summary per application); PGSW implementation memo; public repo release (code, anonymized data per §10, replication guide); **workshop** for PGSW team + CSD; final FRBN report to Dział Badań | Workshop 15 Dec; everything closed by **16 Dec** |

Fieldwork sits in the second half of November. A Monday day 0 puts all three reminders on weekdays (Thu 19, Mon 23, Fri 27) and opens the window after Independence Day, so no public holiday falls inside it — unlike a late-October window, in which All Saints' Day would have landed mid-field. The cost is the tail: data return 3 December against a 16 December close, leaving **9 working days** for analysis, report, repository and workshop with no slack. That is only feasible because the pipeline is built simulation-first, so analysis is a re-run on real data rather than new development. Any slippage upstream eats the report directly; the mitigations, in order of preference, are moving the workshop to January, requesting a no-cost extension from FRBN, or delivering the report as a draft at project close. The manuscript draft (EN) is scheduled after project close and is not an FRBN commitment — the report, repo and workshop are.

---

## 9. Procurement: RFQ specification and selection criteria

Send identical RFQ to Ariadna, Opinia24, Pollster (+ backups). Budget disclosed as "up to 21,000 zł net"; ~2,000 completes; 10–12 min CAWI; nationwide adult sample.

**Mandatory technical requirements (bid must confirm each in writing):**
1. **Invitation-based fielding to a fixed, pre-drawn sample** (e-mail/app push). No router assignment, no open links. This is the make-or-break requirement — router recruitment has no invited denominator and no controllable contact protocol.
2. Random 42/58 arm assignment of the invited pool (arm A / arm B), stratified by sex × age × education × region; assignment executed with our seed or verifiably documented.
3. Arm protocols exactly as §4.1 (A: day 0 + reminders day 3, 7, 11; B: day 0 only); identical incentives; fixed 14-day window; **no quota management during fielding**; completes targets are expectations, not quotas.
4. Per-invitee data deliverables per §4.4 (or, minimum, aggregate invited-pool cross-tabs per arm) — GDPR-compatible pseudonymized delivery.
5. Reminder-wave tagging of every complete; batch-tagged top-ups to both arms only.
6. **Demonstrated reminder lift (mandatory, revised 17 September 2026).** The panel's invitation→complete rate and its wave-by-wave reminder lift on comparable 10–12 minute political surveys, in writing. This moved from scoring criterion to pass/fail because the power analysis (§7.6) shows a 2–3 pp gap makes the study uninformative, and the range this document itself called plausible (RR_B 7–9%, RR_A 11–14% under two reminders) admits exactly that. A bid that cannot evidence the lift cannot be evaluated for the only parameter that determines whether the study can answer its question.
7. **Escalation clause.** If the soft launch shows a gap below the §4.2 threshold, the agency adds a further reminder wave to arm A at no additional cost, and the main launch is held until it does.

**Selection criteria (weighted):** compliance with 1–5 (pass/fail); panel size and invitation-RR evidence (30%); price per complete (30%); data-delivery quality incl. paradata (25%); timeline reliability (15%). Volume discount for N > 1,500 was already assumed in the application's costing; arm B is cheaper to field (no reminders), which the application flags as the argument for holding the blended rate at standard levels.

**Known agency-side risks to probe in clarification calls:** whether "reminders" are e-mail only or include app push (must be symmetric across arms — B gets neither); whether the panel tops up automatically when a survey underperforms (must be disabled); whether profile variables can be delivered for nonrespondents (drives §4.4 fallback).

---

## 10. Ethics and data protection

- **KEB application** (submit immediately; content): standard CAWI with adult panel volunteers; consent screen; no deception — the manipulation varies only the *number of reminders*, i.e., a routine fieldwork parameter; arm B receives *fewer* contacts than industry standard, so the experimental treatment reduces burden; sensitive-topic items (minority attitudes, vote) use established PGSW/EVS wordings with explicit refusal options; no incentives manipulation.
- **GDPR:** agency is controller for panel data; we receive pseudonymized records under a data-processing/sharing agreement (standard agency paperwork). No direct identifiers; region at NUTS2; age banded in the published file.
- **Public data release:** anonymized respondent-level file (k-anonymity check on demographic cross-classifications; suppress region × settlement × age cells < 5), full code, and the invitee-level file reduced to arm/disposition/wave/coarsened demographics. Repository: OSF + GitHub mirror; CC-BY for materials, CC0 for data if agency contract permits (negotiate publication rights in the contract — add to annex).

---

## 11. Outputs

1. **Methodological report (PL)** for SWPS/FRBN + accessible summary for journalists (application commitment): what is non-ignorable in Polish CAWI, what weighting can and cannot fix, protocol recommendations.
2. **Article manuscript (EN)** — working title: *"Diagnosing Non-Ignorable Nonresponse with a Randomized Response Instrument: Evidence from a Polish Online Panel."* Target order: **Public Opinion Quarterly** (fits ch.-12-style applied-methods papers), then **Survey Research Methods**, then **IJPOR**. Selling points: first systematic randomized response instrument in a CEE panel; benchmark validation; partisan mirror-image evidence outside the US two-party context; open protocol.
3. **Public repository:** `renv`-pinned R project, `targets` pipeline, simulation suite, anonymized data, questionnaire (PL/EN), agency contract annex template, and a step-by-step implementation guide ("how to add a light-contact arm to your CAWI study").
4. **Workshop** (PGSW team + CSD, early–mid Dec): method, results, and a concrete proposal for a light-contact arm in the next PGSW CAWI component; cost model from this pilot.
5. **Grant leverage:** the pilot's effect-size estimates and demonstrated protocol feed directly into an NCN OPUS proposal (full-scale implementation: multi-wave, 3+ contact levels, within-party power) and position CSD for Horizon Europe survey-infrastructure calls. (Note: the professorship case and OPUS 2020/39/B/HS6/00853 history make a clean, delivered, pre-registered pilot the right kind of track-record item.)

---

## 12. Risk register

| Risk | Likelihood | Impact | Mitigation / fallback |
|---|---|---|---|
| Reminder lift too small → weak instrument | Med | **Fatal to the confirmatory aims** | Demonstrated lift is now a pass/fail tender requirement with an escalation clause (§9); three reminders from the start (§4.1); soft-launch go/no-go raised to a power threshold, not a validity one (§4.2); gradient pre-registered as a secondary test (§6) |
| Confirmatory tests return null across the board | Low for the global test, **Med for individual constructs** (power 0.65–0.69 at the target gap) | Med | Primary reported quantity is an estimate with an interval, not a verdict (§6); global test across the family is the headline claim; the report and the manuscript are framed around effect-size estimation and the OPUS power case, which survive nulls |
| Ignorability cannot be certified for any item | Med | Med | Equivalence bounds restated in SD units so binary controls are testable at all (§6); where TOST is indeterminate, report the interval and say so |
| Agency cannot do invitation-based fielding or arm tagging | Med | Fatal per agency | Pass/fail RFQ criterion; ≥3 bidders + 2 backups |
| No nonrespondent-level covariates (GDPR) | Med | Med | Pseudo-nonrespondents (ch. 10.5) pre-specified as fallback; aggregate cross-tabs contractually required |
| Completes shortfall (esp. arm B) | Med | Med | Top-up batches to both arms in the same 42/58 ratio; contractual floor is 1,800 total with ≥ 650 in B; power holds |
| Price above 10.50 zł net/complete | Med | Med | Trim questionnaire to 10 min; reduce target to ~1,800 (power at 1,100/700: MDE 6.8 pp / 0.14 SD — still adequate); negotiate arm-B discount |
| Fieldwork collides with unscheduled political shock | Low | Med | 14-day window, timestamps, field-day FE robustness, A-wave-0 vs B placebo |
| Heckman/GJRM instability on flagged items | Med | Low–Med | Primary claim rests on the nonparametric test; report R²_M; copula range; document honestly (ch. 12 practice) |
| Ethics decision delayed | Low–Med | Med | Submit in first project week (early Sep, post-vacation — KEB back to normal schedule); KEB chair contacted about turnaround; fieldwork not before 26 Oct anyway |
| Timeline compression at analysis stage | Low | Med | Simulation-first pipeline: analysis is a re-run on real data |
| Multiple-testing / researcher-df criticism at review | Med | Med | Frozen OSF prereg, BH within family, TOST for nulls, full code+data release (ch. 11.4 defenses, verbatim) |

---

## 13. Immediate next actions (first project week, 7–11 September)

1. **Submit KEB ethics application** (PI; template + consent text drafted from this document).
2. **Send RFQ** with the §9 annex to Ariadna, Opinia24, Pollster (CC backups); ask for bids + RR evidence within 10 working days.
3. **Confirm with Dział Badań** (A. Nahorna): invoice timing vs "month 2" plan, FRBN reporting template, and whether contract-signing needs university legal review lead time.
4. **Assemble questionnaire v0.9** from PGSW 2023/CSES/EVS item banks (team task: Żerkowska-Balas — minority attitudes & norms batteries; Cześnik — turnout/vote/PGSW comparability; Stanley — party evaluations, controls, integration).
5. **Initialize the repository** (`targets` skeleton + simulation of the §7.6 data-generating process) so the power analysis in the prereg comes from the same code that will analyze the real data.
6. **Draft OSF pre-registration** shell in parallel with 4–5.

---

*Design document prepared 16 July 2026; revised 20 July 2026 (timeline shifted to 7 September – 16 December 2026); revised 17 September 2026 (§§3, 4.1, 4.2, 6, 7.3, 7.6, 9, 12 — contact protocol, confirmatory design and procurement requirements changed in response to the simulation-based power analysis). Funding: Uniwersytet SWPS, FRBN, decyzja nr 67/2026/FRBN/C. All outputs to acknowledge: „Projekt finansowany ze środków Funduszu Rozwoju Badań Naukowych Uniwersytetu SWPS (decyzja nr 67/2026/FRBN/C)."*
