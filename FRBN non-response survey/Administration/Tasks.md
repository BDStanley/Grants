# FRBN non-response survey — actions

Decision 67/2026/FRBN/C · project runs **2026-09-07 → 2026-12-16**.
Dates from [[harmonogram]] (Gantt) and [[Project design]] §8 timeline.

## Immediate (21–29 September)

- [ ] Fill in tender dates [T1]–[T15] in `Tender/zapytanie-ofertowe.qmd` and re-render before sending #FRBN 📅 2026-09-25
- [ ] Complete the two `[DO UZUPEŁNIENIA]` placeholders in the KEB form: IOD consultation date, team experience descriptions #FRBN 📅 2026-09-23
- [ ] Submit KEB ethics application #FRBN 📅 2026-09-24
- [ ] Contact KEB chair about review turnaround #FRBN 📅 2026-09-24
- [ ] Contact Marta and Mikołaj regarding the non-response survey project #FRBN 📅 2026-09-22
- [ ] Confirm with Dział Badań (A. Nahorna): invoice timing against a 10 December acceptance protocol, FRBN reporting template, legal-review lead time for contract signing #FRBN 📅 2026-09-25
- [ ] Decide whether the workshop moves to January or the project requests a no-cost extension; data return 3 December and the project closes 16 December #FRBN 📅 2026-09-29

## Questionnaire and ethics (22 September – 30 October)

- [ ] Cognitive testing of questionnaire with team and 2–3 naive testers; focus on own-wording items M6.2, M6.4–M6.6, M9.1–M9.3 and the bipolar format in M8.3–M8.5 #FRBN 🛫 2026-09-22 📅 2026-10-06
- [ ] Verify exact PKW benchmark figures (2023 Sejm turnout; 2025 presidential both rounds) against the PKW site #FRBN 📅 2026-10-06
- [ ] Finalise questionnaire: 58 substantive items plus attention check, median ≤ 12 minutes #FRBN 📅 2026-10-09
- [ ] Receive KEB ethics decision (follow up if not received) #FRBN 📅 2026-10-30
- [ ] Translate questionnaire into English for OSF deposit and repository #FRBN 🛫 2026-10-12 📅 2026-10-30

## Tender and contracting (29 September – 23 October)

- [ ] Send RFQ to Ariadna, Opinia24, Pollster (CC Norstat/IQS as backups); requirement W6 makes wave-by-wave reminder-lift evidence a rejection criterion #FRBN 📅 2026-09-29
- [ ] Answer bidder questions and circulate replies to all addressees #FRBN 🛫 2026-09-29 📅 2026-10-08
- [ ] Receive bids #FRBN 📅 2026-10-15
- [ ] Hold clarification calls: reminder channel symmetry across arms, auto top-up disabled, nonrespondent profile-variable availability, whether three reminders in 14 days trigger panel contact-frequency caps #FRBN 🛫 2026-10-16 📅 2026-10-20
- [ ] Evaluate bids: pass/fail on W1–W7, then K1 price 30, K2 panel and invitation effectiveness 30, K3 data quality 25, K4 timeline 15 #FRBN 📅 2026-10-21
- [ ] Sign agency contract including Załącznik 2 data annex, the §8 pt 9 escalation clause, no-quota-management and fixed-window clauses, and the open-data publication right #FRBN 📅 2026-10-23

## Scripting and testing (26 October – 4 November)

- [ ] Hand questionnaire to agency for programming #FRBN 📅 2026-10-26
- [ ] Test agency script: routing, code 97/98 options visible on every attitudinal item, item-order randomisation in M5–M7, attention check inside M6, arm and wave tagging #FRBN 🛫 2026-11-02 📅 2026-11-04
- [ ] Verify 42/58 randomisation of the invited pool (our seed or documented procedure; balance on profile covariates) #FRBN 📅 2026-11-04

## Soft launch and pre-registration (5–13 November)

- [ ] Run soft launch, ~100 invitations per arm: arm and wave tags present in data, paradata fields populated, median timing ≤ 12 minutes, no routing errors #FRBN 🛫 2026-11-05 📅 2026-11-09
- [ ] Verify reminder lift between arms from soft-launch data #FRBN 🛫 2026-11-09 📅 2026-11-11
- [ ] Go/no-go decision: ≥ 7 pp proceed as planned; 5–7 pp proceed and record reduced power; < 5 pp invoke the escalation clause and add a fourth reminder wave in arm A #FRBN 📅 2026-11-12
- [ ] Record whether soft-launch cases enter the main dataset (only if the protocol was identical) #FRBN 📅 2026-11-12
- [ ] Freeze pre-registration on OSF: hypotheses H1–H5, M-check, H-global, SAP, decision rules, contingencies, questionnaire, contact protocol, simulation code #FRBN 📅 2026-11-13

## Main fieldwork (16–30 November)

- [ ] Launch main fieldwork, day 0; 14-day window, both arms closing the same day #FRBN 📅 2026-11-16
- [ ] Confirm reminder 1 sent to arm A non-completers only (day 3) #FRBN 📅 2026-11-19
- [ ] Confirm reminder 2 sent to arm A non-completers only (day 7) #FRBN 📅 2026-11-23
- [ ] Confirm reminder 3 sent to arm A non-completers only (day 11) #FRBN 📅 2026-11-27
- [ ] Monitor daily completes per arm; release top-up batches to both arms simultaneously in the same 42/58 ratio if completes lag at the midpoint #FRBN 🛫 2026-11-16 📅 2026-11-30
- [ ] Close fieldwork: target ~2,000 completes (~1,200 arm A, ~800 arm B), minimum 1,800 with no fewer than 650 in arm B #FRBN 📅 2026-11-30

## Data and analysis (30 November – 14 December)

- [ ] Receive full data delivery: invitee file, respondent file, field report, randomisation documentation #FRBN 🛫 2026-11-30 📅 2026-12-03
- [ ] Verify delivery and sign acceptance protocol #FRBN 🛫 2026-12-03 📅 2026-12-09
- [ ] Coordinate agency invoice with Dział Badań after acceptance #FRBN 📅 2026-12-10
- [ ] Stage 0: reconcile invitee file against field report, verify randomisation balance, construct outcomes and indices #FRBN 📅 2026-12-04
- [ ] Stage 1: response rates by arm, response probit on the invited pool, reminder-wave response curve #FRBN 📅 2026-12-07
- [ ] Stage 2: diagnostic regressions Y ~ Z + X with HC2 errors for all pre-registered outcomes; one-sided tests with BH-FDR across the four constructs; Brown's combined test #FRBN 📅 2026-12-09
- [ ] Run placebo test (arm B vs arm A wave 0) and the five-level propensity gradient with field-day fixed effects #FRBN 📅 2026-12-09
- [ ] Stage 3: TOST equivalence tests for H5 against ±0.10 SD, binaries included #FRBN 📅 2026-12-10
- [ ] Stage 4: Heckman ML plus GJRM copula range for flagged items; bootstrap over the invited pool stratified by arm; NINR weights as a stretch goal; pseudo-nonrespondent fallback if individual-level nonrespondent data are unavailable #FRBN 📅 2026-12-12
- [ ] Stage 5: three-column comparison (observed / raked / corrected) against PKW 2023 and 2025, GUS/EHIS, CEPiK, CBOS/ISKK; estimator-comparison plot #FRBN 📅 2026-12-14
- [ ] Run Manski bounds and Hartman–Huang sensitivity analyses for the supplement #FRBN 📅 2026-12-14

## Reporting and closeout (9–16 December)

- [ ] Write methodological report v1 (PL) including the journalist-accessible summary #FRBN 🛫 2026-12-09 📅 2026-12-16
- [ ] Write PGSW implementation memo: light-contact arm for the next wave, cost model, protocol lessons #FRBN 📅 2026-12-14
- [ ] Release public repository: `renv`-pinned code, `targets` pipeline, simulation suite, anonymised data (k-anonymity check, suppress region × settlement × age cells below 5), questionnaire PL/EN, contract annex template, implementation guide #FRBN 🛫 2026-12-11 📅 2026-12-16
- [ ] Hold workshop for PGSW team and CSD #FRBN 📅 2026-12-15
- [ ] Check all outputs carry the FRBN acknowledgement („Projekt finansowany ze środków Funduszu Rozwoju Badań Naukowych Uniwersytetu SWPS, decyzja nr 67/2026/FRBN/C") #FRBN 📅 2026-12-16
- [ ] Submit final FRBN report to Dział Badań #FRBN 📅 2026-12-16

## After project close

- [ ] Draft article manuscript (EN) — "Diagnosing Non-Ignorable Nonresponse with a Randomized Response Instrument" — targeting POQ #FRBN 📅 2027-01-29
