# rota-writer

Write contract-compliant, fair on-call rotas for resident doctors in **Anaesthesia and Intensive Care**,
using constrained optimisation (mixed-integer linear programming) in R.

> **Status:** early development. The input format, test data and compliance checker are in place,
> and the solver writes rotas that meet every hard rule. Soft rules (fairness, requests, skill mix)
> are not optimised yet (see [Roadmap](#roadmap)).

## Aims

1. **Compliant by construction.** Every rota produced satisfies the 2016 Terms and Conditions of
   Service for resident doctors in England (Schedule 3, as amended in 2019). These rules are hard
   constraints, not post-hoc checks. See [`docs/contract-rules.md`](docs/contract-rules.md).
2. **Fair.** On-calls, nights and weekends are shared in proportion to each doctor's whole-time
   equivalent (WTE). Leave and preference requests are honoured wherever the hard rules allow.
3. **Training-aware.** The rota respects placements (e.g. a 3- or 6-month ICU or obstetric block),
   competencies (e.g. obstetric sign-off), and a sensible skill mix across the on-call tiers.
4. **Transparent.** All rules and their weights live in a spreadsheet that the rota coordinator can
   read and edit. Every rota, whether solved or edited by hand, can be re-checked by an independent
   compliance checker.
5. **Usable by non-programmers.** The eventual interface is a point-and-click app: upload the
   workbook, press solve, download the rota. R is only the engine.

## Scope

| Role | Code | Shifts |
|---|---|---|
| ICU 1st on-call (senior resident) | `ICU1` | long day `ICU1_LD`, night `ICU1_N` |
| ICU 2nd on-call | `ICU2` | `ICU2_LD` (weekends), `ICU2_N` |
| Anaesthesia 1st on-call | `ANA1` | `ANA1_LD`, `ANA1_N` |
| Anaesthesia 2nd on-call | `ANA2` | `ANA2_LD` (weekends), `ANA2_N` |
| Obstetric anaesthesia | `OBS` | `OBS_LD`, `OBS_N` |
| Normal working days | `ICU_DAY`, `ANA_DAY` | weekdays 08:00–17:00 |

All on-call shifts are **resident**: long day 08:00–20:30 (12.5 h) and night 20:00–08:30 (12.5 h).
Non-resident on-call is listed in the rules reference for future use.

## Constraint catalogue

### Hard: contractual
These come from TCS Schedule 3. Full details, parameters and sources are in
[`docs/contract-rules.md`](docs/contract-rules.md).

- Average ≤ 48 h/week (pro rata for LTFT), and ≤ 72 h in any 168 h
- Shifts ≤ 13 h, and ≥ 11 h rest between shifts
- ≤ 7 consecutive shifts, ≤ 4 consecutive long shifts, ≤ 4 consecutive long evenings, each followed by 48 h rest
- ≤ 4 consecutive nights, with 46 h rest after any run of nights
- Weekends no more than 1 in 2
- LTFT doctors do not work on their fixed non-working day

### Hard: local
- Every on-call slot is filled, and normal-day cover meets its minimum
- Doctors only fill roles they are signed off for (`can_*` columns in `staff`)
- Doctors only work while in post (`start_date`/`end_date`)
- Placements: during a placement block, a doctor only works the roles listed for that placement
  (`placement_roles`). An ICU block means ICU1/ICU2/ICU_DAY only, and an OBS block means OBS on-calls
  and ANA_DAY only.
- **Approved** leave is never overwritten. Nights-exempt doctors (e.g. on occupational health
  advice) are never given nights.

### Soft: weighted in the objective
- **Skill mix** (`skill_mix` sheet). For example, at least one ST5+ across the 1st on-call tiers
  (ICU1/ANA1/OBS) every night, at least one ST4+ across ICU1/ANA1 every long day, and at least one
  ST3+ in theatre/labour ward at night. These have heavy weights,
  so a shortfall is reported rather than making the model infeasible.
- **Fairness:** on-calls, nights and weekends deviate as little as possible from each doctor's
  WTE-weighted share
- **Requested** (not yet approved) leave, and preference requests (`requests` sheet, with
  per-request weights)
- **Discouraged placement roles** (`penalty` column in `placement_roles`). For example, doctors on
  the general rota can cover OBS on-calls, but at a significant penalty per shift, so obstetric
  on-call goes to OBS-block doctors first.
- Weekends no more than 1 in 3 where possible (this avoids the weekend allowance)
- Avoid isolated single nights and fragmented patterns

The penalty weights are in the `rules` and `skill_mix` sheets, so users can tune them without code.

## Modelling approach

- A binary decision variable `x[d, t, s]` is 1 if doctor *d* works shift *s* on day *t*.
- **Cover:** for each on-call shift on each day, `Σ_d x[d,t,s] = n_required`.
- **One shift per day:** for each doctor and day, `Σ_s x[d,t,s] ≤ 1`, and it equals 0 on leave days,
  non-working days and days outside the doctor's post.
- **Rest rules** become linear constraints over day indices:
  - *Sliding windows* handle maxima, e.g. any 5 consecutive days have ≤ 4 nights, and any 7 days
    have ≤ 72 h.
  - *Incompatible pairs* handle minimum rest. A pre-computed table lists the forbidden
    `(shift on t, shift on t+1)` pairs where the gap is < 11 h, e.g. night → day shift.
  - *Run-end rules* handle rest after runs. For nights, `x[t, night] − night[t+1] + x[t+k, s'] ≤ 1`
    for every shift `s'` starting less than 46 h after the night ends. For a run of the maximum
    length (e.g. 7 shifts), the run's days plus any shift within 48 h of its end sum to at most
    the maximum.
  - All gaps are computed in clock time from the `shifts` sheet, so the model matches the
    checker without hard-coding shift patterns.
- **Weekends:** an indicator `w[d, k]` is 1 if doctor *d* works any shift touching weekend *k*
  (this includes Friday night). Then `Σ_k w[d,k] ≤ weekends in post / 2` enforces 1 in 2.
- **Objective:** minimise the weighted sum of soft-constraint violations (slack variables) and the
  fairness deviations.
- **Solver:** [HiGHS](https://highs.dev) via `highs::highs_solve()`. The model is built as tidy
  tables with dplyr (one row per variable, one per constraint, one per non-zero) and turned into a
  sparse matrix only at solve time. This avoids ompr's slow model building at this size. Variables
  a doctor may never use (wrong competency or placement, leave, LTFT day, not in post) are left
  out, so the test data has about 10k binaries rather than 37k. The full 13-week test rota
  solves in under 10 s.
- **Independent checker:** a separate, deliberately simple function re-checks any rota against
  every rule. It does not trust the solver, and it also checks rotas that have been edited by hand.

## Input data: `rota_inputs.xlsx`

One workbook with one sheet per table. Dates are real Excel dates and times are `"HH:MM"` text.
Yes/No columns use `Y`/`N` drop-downs.

| Sheet | Contents |
|---|---|
| `settings` | Rota start and end dates, reference period, solver time limit |
| `rules` | Every rule: `rule_id`, description, parameter value, `hard`/`soft`, weight, enabled. Contract parameters are editable here. |
| `grades` | Grade → seniority `rank`, used for skill-mix rules. Clinical fellows (LEDs) are mapped to an equivalent rank. |
| `shifts` | Shift definitions: role, type (`normal_day`/`long_day`/`night`), start and end, hours, flags (`is_on_call`, `is_night`, `is_long`, `is_late`) |
| `cover` | Number of doctors required per shift by day type (`weekday`/`weekend`/`bank_holiday`). On-call is exact (`n_min = n_max`); normal days have a minimum. |
| `bank_holidays` | English bank holidays in the rota window |
| `staff` | One row per doctor: grade, WTE, LTFT fixed day off, start/end in post, nights-exempt, WTR opt-out, competencies (`can_ICU1` … `can_OBS`) |
| `placements` | Training placement blocks: `ICU`, `OBS` or `GENERAL`, with dates |
| `placement_roles` | Which roles each placement may work. Unlisted roles are forbidden; `penalty` > 0 means allowed but discouraged (soft, per shift). |
| `leave` | Leave periods: type (`annual`, `study`, `parental`, `exam`), status (`approved` = hard, `requested` = soft) |
| `requests` | Soft preferences: `no_oncall`, `no_night`, `no_weekend`, `prefer_oncall` on a date, with a weight |
| `skill_mix` | Seniority rules across groups of shifts: minimum grade, minimum count, weight |

## Output: `example_rota_output.xlsx`

- `grid`: one row per date and one column per doctor, holding shift or leave codes (the
  coordinator's view)
- `long`: tidy `doctor_id, date, code` (the format the code uses)
- `known_violations`: the test fixture lists contract breaches that were planted on purpose (see below)

The example in `inst/extdata/` is a **naive round-robin rota that deliberately breaks the rules**. It
shows the output format and gives the compliance checker something to catch. It is not an
optimised rota.

## Compliance checker

`check_rota()` checks any rota, whether solved or edited by hand, against every rule in the `rules`
sheet. It works on real clock times: each shift becomes an interval, so a night starting 20:00 ends
08:30 the next day. It deliberately does not reuse the optimiser's formulation, so each can catch
mistakes in the other.

```sh
Rscript scripts/check_rota.R inst/extdata/rota_inputs.xlsx inst/extdata/example_rota_output.xlsx report.xlsx
```

This prints the number of violations for each rule and writes a report workbook with three sheets:
`summary`, `violations` (one row per breach, with doctor, dates, detail and penalty) and `doctors`
(hours, average weekly hours, on-calls, nights and weekends per doctor, against each doctor's
WTE-weighted fair share). It exits with status 1 if any hard rule is broken, and 2 if the inputs
fail validation.

From R:

```r
pkgload::load_all()
inputs <- read_rota_inputs("inst/extdata/rota_inputs.xlsx")
validate_inputs(inputs)                 # zero rows = inputs are consistent
rota <- read_rota("inst/extdata/example_rota_output.xlsx")
violations <- check_rota(rota, inputs)
doctors <- summarise_doctors(rota, inputs)

# File paths work too
check_rota("inst/extdata/example_rota_output.xlsx", "inst/extdata/rota_inputs.xlsx")
```

Each function has a help page, e.g. `?check_rota`, `?read_rota_inputs`, `?rotawriter`.

## Solver

```sh
Rscript scripts/solve_rota.R inst/extdata/rota_inputs.xlsx rota.xlsx
```

This solves the rota, writes a workbook with `grid` and `long` sheets (approved leave is shown as
`AL`, `SL`, `EX` or `PL`), and runs the checker on the result. It exits with status 1 if no rota was
found or it breaks a hard rule.

From R:

```r
solution <- solve_rota(inputs)          # time limit from the settings sheet
solution$rota                           # long format: doctor_id, date, code
check_rota(solution$rota, inputs)       # should have no hard violations
write_rota(solution$rota, inputs, "rota.xlsx")
build_rota_model(inputs)                # the model itself: sizes per rule
```

If a shift has fewer eligible doctors than it needs, `solve_rota()` stops before solving and lists
the shifts. Other infeasibility (e.g. too few doctors for the weekend rule) is reported only as
`status = "Infeasible"` for now.

## Repository layout

The project is an R package (`rotawriter`), loaded with `pkgload::load_all()` during development.

```
README.md
DESCRIPTION, NAMESPACE        package metadata
R/read_inputs.R               read_rota_inputs(), validate_inputs(), read_rota()
R/check_rota.R                check_rota(), summarise_doctors()
R/report.R                    write_check_report()
R/model.R                     build_rota_model(): the MILP as tidy tables
R/solve.R                     solve_rota(), write_rota()
man/                          help pages, generated by roxygen2 (do not edit)
scripts/check_rota.R          command-line checker
scripts/solve_rota.R          command-line solver
tests/testthat/               tests for each rule, plus the planted violations
docs/contract-rules.md        rules reference with sources
data-raw/make_test_data.R     regenerates the test workbooks (deterministic)
inst/extdata/rota_inputs.xlsx test inputs: 34 fictional doctors, 13 weeks from 1 Feb 2027
inst/extdata/example_rota_output.xlsx
```

Regenerate the test data, rebuild the help pages (and `NAMESPACE`) from the roxygen comments, and run
the tests:

```sh
Rscript data-raw/make_test_data.R
Rscript -e 'roxygen2::roxygenise()'
Rscript -e 'pkgload::load_all(); testthat::test_dir("tests/testthat", load_package = "none")'
```

## Requirements

R ≥ 4.3 with `tidyverse`, `lubridate`, `readxl`, `openxlsx2`, `pkgload` and `testthat`. The solver
also needs `highs` and `Matrix`. `highs` builds from source and needs `cmake`
(`sudo pacman -S cmake` / `apt install cmake`).

## Roadmap

1. **Data model, test data and README** ✓
2. **Compliance checker** ✓. Loads the workbook, validates inputs, and checks any rota against
   every rule. It catches every planted violation in the example.
3. **Core MILP** ✓. Hard constraints only. Finds a rota for the test data that passes the checker
   with no hard violations, including when contract limits are tightened.
4. **Soft constraints** ← *next*: fairness, requests, skill mix, weights. Report which soft rules were broken
   and why. Slack on cover and weekends will also turn infeasible inputs into a report of what
   could not be met.
5. **Outputs:** coordinator grid (xlsx), per-doctor calendars (`.ics`/PDF), and a compliance and
   fairness report.
6. **No-code interface.** A Shiny app (bslib, editable tables) to upload the workbook, edit
   requests, solve, and download. It may later be packaged as an R package / golem app.
7. Later: re-solving mid-rota with existing shifts fixed, swaps, and non-resident on-call.

## Prior art

There is no maintained open-source NHS rota **optimiser** that we know of. Related work:

- [RudolfCardinal/rota](https://github.com/RudolfCardinal/rota): Python rota design and checking
  tool (banding, New Deal/EWTD). It is a checker, not an optimiser. Apache-2.0.
- [BMA rota checker](https://bma.org.uk/pay-and-contracts/working-hours/work-schedule/bma-rota-checker):
  the reference compliance check for the 2016 contract. Our checker should agree with it.
- Academic work: a [2017 rostering problem set based on the junior doctor contract](https://e-space.mmu.ac.uk/619524)
  (simulated annealing); an [anaesthetist rota system using constraint satisfaction](https://aaai.org/?p=65521);
  and a [MIP model for anaesthetist rostering](https://ftsm.ukm.my/v6/public/assets/file/technicalreport/LP-CAIT-FTSM-2021-001.pdf).
- Industry: [Hartree Centre / Alder Hey on-call optimisation](https://www.hartree.stfc.ac.uk/case-studies/2026/03/09/modernising-nhs-staffing-schedules-with-advanced-rota-optimisation/) (2026),
  and Lantum (built on the open-source [Timefold](https://timefold.ai) solver).
- General tools: Google OR-Tools CP-SAT and Timefold. Nurse rostering is the closest
  well-studied problem.
- R: [Exploring ompr with the HiGHS solver](https://www.r-bloggers.com/2022/09/exploring-ompr-with-highs-solver/).

## Disclaimer

This is not an official BMA or NHS Employers tool. Any rota it produces must still be reviewed by
the rota coordinator and be consistent with the generic work schedule. Concerns go to the
Guardian of Safe Working. All names in the test data are fictional.
