# FRBN non-response survey — actions

Decision 67/2026/FRBN/C · project runs **2026-09-07 → 2026-12-16** (internal target 2026-12-14).
Dates from [[Project design]] §8 timeline and §13 immediate actions.

## Immediate (7–11 September)

- [ ] Submit KEB ethics application (light-contact arm framed as burden-reducing; consent text from design doc) #FRBN ⏫ 🛫 2026-09-07 📅 2026-09-18
- [ ] Send RFQ with §9 contract annex to Ariadna, Opinia24, Pollster (CC Norstat/IQS as backups); request bids + invitation→complete RR and reminder-lift evidence within 10 working days #FRBN ⏫ 📅 2026-09-11
- [ ] Confirm with Dział Badań (A. Nahorna): invoice timing vs "month 2" plan, FRBN reporting template, legal-review lead time for contract signing #FRBN 🔼 📅 2026-09-11
- [ ] Assemble questionnaire v0.9 from PGSW 2023/CSES/EVS item banks (Żerkowska-Balas: minority attitudes & norms; Cześnik: turnout/vote/PGSW comparability; Stanley: party evaluations, controls, integration) #FRBN ⏫ 📅 2026-09-18
- [ ] Initialize repository: `targets` skeleton + `renv` + simulation of the §7.6 data-generating process #FRBN 🔼 📅 2026-09-18
- [ ] Draft OSF pre-registration shell (design protocol, hypotheses H1–H5 + M-check, SAP outline, contingency rules) #FRBN 🔼 📅 2026-09-18
- [ ] Contact KEB chair about review turnaround #FRBN 🔽 📅 2026-09-11

## Contracting & instrument preparation (21 September – 9 October)

- [ ] Hold clarification calls with bidders: reminder channels symmetric across arms, auto top-up disabled, nonrespondent profile-variable availability #FRBN 🔼 🛫 2026-09-21 📅 2026-10-02
- [ ] Evaluate bids against §9 criteria (pass/fail on mandatory requirements 1–5; then panel size/RR 30%, price 30%, data quality 25%, timeline 15%) #FRBN ⏫ 📅 2026-10-02
- [ ] Sign agency contract incl. data-delivery annex (§4.4), no-quota-management and fixed-window clauses, publication-rights clause for CC0 data release #FRBN ⏫ 📅 2026-10-09
- [ ] Finalize questionnaire (median ≤ 12 min, ~60 items) + cognitive check with team and 2–3 naive testers #FRBN ⏫ 📅 2026-10-09
- [ ] Complete simulation-based power analysis and lock parameters for the prereg #FRBN 🔼 📅 2026-10-09
- [ ] Draft full pre-registration: hypotheses, SAP (estimators, SEs, BH-FDR, TOST SESOI, exclusion rules), contingencies (third reminder, top-up batches, pseudo-nonrespondent fallback) #FRBN ⏫ 📅 2026-10-09
- [ ] Receive KEB ethics decision (follow up if not received) #FRBN ⏫ 📅 2026-10-09

## Scripting & soft launch (12–23 October)

- [ ] Test agency scripting: routing, DK/refusal options, item-order randomization in M5–M7, attention check, arm tagging #FRBN ⏫ 🛫 2026-10-12 📅 2026-10-20
- [ ] Run soft launch (~100 invitations per arm): verify arm tagging in data, paradata fields populated, median timing ≤ 12 min, no routing errors #FRBN ⏫ 📅 2026-10-23
- [ ] Go/no-go check on reminder lift; if < 2 pp, invoke pre-registered contingency (third reminder in arm A) #FRBN ⏫ 📅 2026-10-23
- [ ] Freeze pre-registration on OSF before main launch #FRBN 🔺 📅 2026-10-23
- [ ] Verify 50/50 randomization of invited pool (our seed or documented procedure; balance on profile covariates) #FRBN 🔼 📅 2026-10-23

## Main fieldwork (26 October – 9 November)

- [ ] Launch main fieldwork: 14-day window, both arms, closing before 11 Nov Independence Day #FRBN 🔺 📅 2026-10-26
- [ ] Monitor daily completes per arm; release symmetric pre-randomized top-up batches if completes lag (never selectively) #FRBN ⏫ 🛫 2026-10-26 📅 2026-11-09
- [ ] Close fieldwork with ≥ 1,800 completes (~1,200 arm A / ~700–800 arm B) #FRBN ⏫ 📅 2026-11-09
- [ ] Coordinate agency invoice with Dział Badań on fieldwork completion #FRBN 🔼 📅 2026-11-13

## Data & analysis (10–27 November)

- [ ] Receive full data delivery (contractual ≤ 5 working days after close): invitee file, respondent file, field report, randomization documentation #FRBN ⏫ 📅 2026-11-16
- [ ] Stage 0: reconcile invitee file with field report, verify randomization balance, construct outcomes and indices #FRBN 🔼 📅 2026-11-19
- [ ] Stage 1: response rates by arm, response probit (inclusion condition), reminder-wave response curves #FRBN 🔼 📅 2026-11-23
- [ ] Stage 2: diagnostic regressions Y ~ Z + X for all pre-registered outcomes; BH-FDR; TOST for H5; placebo and gradient checks; flag items #FRBN ⏫ 📅 2026-11-23
- [ ] Stage 3: Heckman ML + GJRM copula models for flagged items (NINR weights as stretch goal; pseudo-nonrespondent fallback if needed); bootstrap uncertainty #FRBN ⏫ 📅 2026-11-27
- [ ] Stage 4: three-column comparison (observed / raked / corrected) with external benchmarks (PKW 2023 + 2025, GUS/EHIS, CBOS/ISKK); headline estimator-comparison plot #FRBN ⏫ 📅 2026-11-27
- [ ] Verify exact PKW benchmark figures (2023 Sejm; 2025 presidential both rounds) #FRBN 🔽 📅 2026-11-23

## Reporting (23 November – 9 December)

- [ ] Write methodological report v1 (PL) incl. journalist-accessible summary #FRBN ⏫ 🛫 2026-11-23 📅 2026-12-04
- [ ] Draft article manuscript (EN) — "Diagnosing Non-Ignorable Nonresponse with a Randomized Response Instrument" — targeting POQ #FRBN 🔼 📅 2026-12-09
- [ ] Write PGSW implementation memo: arm design for next wave, cost model, protocol lessons #FRBN 🔼 📅 2026-12-09
- [ ] Run Manski bounds and Hartman–Huang sensitivity analyses for supplementary section #FRBN 🔽 📅 2026-12-09

## Closeout (7–14 December)

- [ ] Prepare and release public repository: `renv`-pinned code, `targets` pipeline, simulation suite, anonymized data (k-anonymity check; suppress region × settlement × age cells < 5), questionnaire PL/EN, contract annex template, implementation guide #FRBN ⏫ 📅 2026-12-14
- [ ] Hold workshop for PGSW team + CSD: method, results, light-contact-arm proposal for next PGSW CAWI, cost model #FRBN ⏫ 📅 2026-12-14
- [ ] Submit final FRBN report to Dział Badań #FRBN 🔺 📅 2026-12-14
- [ ] Check all outputs carry the FRBN acknowledgement („Projekt finansowany ze środków Funduszu Rozwoju Badań Naukowych Uniwersytetu SWPS, decyzja nr 67/2026/FRBN/C") #FRBN 🔽 📅 2026-12-14
