# Recorded simulation runs

Snapshots of the simulation study, kept in the repository because the
pre-registration cites them and because `_targets/` is not version-controlled.
Regenerate with `targets::tar_make()` (`power_table`, `size_table`,
`gap_requirement`).

## 2026-09-17 — the run that changed the design

200 replications per scenario; `gap_curve()` at 150. Parameters as in
`dgp_params()` at that date: RR_B target 8%, three reminders in arm A,
implied selection–outcome correlations near Bailey's anchors.

- `power_2026-09-17.csv` — power by construct and scenario
- `size_2026-09-17.csv` — false-positive and equivalence rates on the controls
- `gap_curve_2026-09-17.csv` — power against arm A's response rate

Headline: the design as originally specified (two reminders, thirteen-item
confirmatory family, two-sided) had 8–10% power on H2–H4. The revised design
(three reminders, four constructs, one-sided) has 65–69%, with the global test
at ~1.00. The remaining shortfall against a conventional 80% is the honest
reason the primary reported quantity is now an estimate with an interval.

## 2026-09-17 (second run) — reconciling the protocol with the budget

Three reminders raise arm A's response rate and therefore the number of
completes. The tender asks for a fixed price for the whole commission rather
than a price per interview, but completes drive the agency's own cost, so a
protocol yielding ~2,400 completes will be bid higher than one yielding ~2,000
— and bids above the 21,000 zł net cap are rejected. Two configurations that
stay within the costing the budget was built on, 200 replications each:

- `cost_2026-09-17.csv` — completes and cost per configuration
- `power_affordable_2026-09-17.csv` — power for each

| Configuration | Invited | Completes A / B | Variable cost at 10.50 zł | Turnout | Other three | Global |
|---|---:|---:|---:|---:|---:|---:|
| 17,000 invited, 50/50 | 17,000 | 1,307 / 675 | 20,811 zł | 0.97 | 0.49–0.55 | 1.00 |
| **18,500 invited, 42/58** | 18,500 | 1,193 / 807 | 21,000 zł | 0.97 | **0.58–0.69** | 1.00 |

The unequal split is strictly better at the same cost, because equalising
*completes* across the arms minimises the standard error of the difference
being tested. It also lifts arm B's completes clear of the tender's 650
minimum. This is the design of record; `dgp_params()` defaults to it.
