# rota-writer

Write contract-compliant, fair on-call rotas for resident doctors in **Anaesthesia and Intensive Care**,
using constrained optimisation (mixed-integer linear programming) in R.

> **Status:** early development. The solver and checker work from a point-and-click app, the
> command line or R: they write rotas that meet every hard rule and minimise the soft-rule
> penalties, with a coordinator workbook, a compliance report and per-doctor calendars. Inputs are
> still edited in Excel (see [Roadmap](#roadmap)).

## Installation

You need R ≥ 4.3 and `cmake` (the `highs` solver package builds from source):

```sh
sudo apt install cmake        # or: sudo pacman -S cmake / brew install cmake
git clone https://github.com/mark-jeffrey/rota-writer.git
cd rota-writer
Rscript -e 'install.packages(c("dplyr", "highs", "lubridate", "Matrix", "openxlsx2", "purrr",
  "readxl", "rlang", "stringr", "tibble", "tidyr", "withr", "pkgload", "testthat"))'
# For the app:
Rscript -e 'install.packages(c("shiny", "bslib", "reactable", "callr", "zip"))'
```

The scripts load the package straight from the folder, so there is nothing else to install. Run
them from the project root.

## The app

```sh
Rscript scripts/run_app.R
```

This opens the app in your web browser. It runs on your own computer: no staff data is sent
anywhere. In the app you:

1. **Inputs:** upload your inputs workbook (or use the example data). Any problems with it are
   listed, and solving waits until they are fixed in Excel.
2. **Solve:** choose a time limit and press Solve. The solver runs in the background while the
   penalty falls. Press **Stop and keep best rota** to finish early with the best rota so far.
3. **Results:** the rota grid, who is on each shift, fairness within each staffing pool, patterns,
   and every rule broken. **Download** gives `rota.xlsx`, `report.xlsx` and `calendars.zip`, the
   same files as the command-line solver.
4. **Check a rota:** upload a rota workbook, such as one edited by hand, to check it against the
   inputs.

![The Solve tab: a progress bar and the best penalty so far, falling over a 3-minute solve](docs/images/app-solve.png)

![The results for the example rota: no hard rules broken, every shift covered, and the rota grid coloured by shift](docs/images/app-results.png)

If the package is installed (`R CMD INSTALL .`), `rotawriter::run_app()` starts it from any R
session. On Windows, `inst/launchers/rota-writer.bat` can be copied to the desktop as a
double-click launcher.

## Quick start (command line)

1. **Prepare the inputs.** Copy the test workbook `inst/extdata/rota_inputs.xlsx` and replace the
   example data with your own: rota dates in `settings`, doctors in `staff`, and placements, leave
   and requests in their sheets. See [Input data](#input-data-rota_inputsxlsx) for each sheet. The
   rules and weights can stay as they are.
2. **Solve.**

   ```sh
   Rscript scripts/solve_rota.R my_inputs.xlsx out/
   ```

   This runs for the time limit in `settings` (10 minutes in the test workbook); pass a number of
   seconds as a third argument to change it. It prints any input problems and stops, or writes
   `out/rota.xlsx`, `out/report.xlsx` and one calendar per doctor in `out/calendars/`. See
   [Solver](#solver) for what each file contains.
3. **Review** the rota and report. Any unfilled slots are marked `UNFILLED` in the `by_shift` sheet
   and listed in the report.
4. **Re-check hand edits.** Edit the `long` sheet of `rota.xlsx` (the `grid` sheet is for reading
   only), then:

   ```sh
   Rscript scripts/check_rota.R my_inputs.xlsx out/rota.xlsx out/report.xlsx
   ```

## Aims

1. **Compliant by construction.** Every rota produced satisfies the 2016 Terms and Conditions of
   Service for resident doctors in England (Schedule 3, as amended in 2019). These rules are hard
   constraints, not post-hoc checks. See [`docs/contract-rules.md`](docs/contract-rules.md).
2. **Fair.** On-calls, nights and weekends are shared in proportion to each doctor's whole-time
   equivalent (WTE), within each staffing pool. Leave and preference requests are honoured
   wherever the hard rules allow.
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
| ICU 1st on-call (anaesthetist, airway competent) | `ICU1` | long day `ICU1_LD`, night `ICU1_N` |
| ICU 2nd on-call (FY/IMT/ACCS pool) | `ICU2` | `ICU2_LD`, `ICU2_N` |
| Theatre on-call | `ANA1` | `ANA1_LD`, `ANA1_N` |
| Senior anaesthetic tier, supporting theatres, ICU and obstetrics (ICU1 and OBS competent) | `ANA2` | `ANA2_LD`, `ANA2_N` |
| Obstetric anaesthesia | `OBS` | `OBS_LD`, `OBS_N` |
| Normal working days | `ICU_DAY`, `ANA_DAY` | 08:00–17:00 on weekdays; one `ICU_DAY` at weekends and bank holidays |

All on-call shifts are **resident**: long day 08:00–20:30 (12.5 h) and night 20:00–08:30 (12.5 h).
Every on-call tier runs a long day and a night every day. Normal days have a minimum and no
maximum: once the on-calls are filled, the solver adds normal days to bring each doctor up to their
contracted hours. Non-resident on-call is listed in the rules reference for future use.

Doctors belong to a **staffing pool** (`pool` in `staff`). In the test data, the anaesthetics pool
covers theatres, obstetrics and ICU1. A second pool of foundation, internal medicine and ACCS
emergency medicine trainees (`icu_tier2`, on an `ICU_TIER2` placement) covers ICU2. Both pools
work ICU short days. Fair shares are measured within each pool, because the ICU tier 2 rota is
busier out of hours than the anaesthetics one.

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
- Every on-call slot is filled, and normal-day cover meets its minimum. If the other hard rules
  make a slot impossible to fill, it is left empty and reported rather than breaking a contract
  rule (see [Solver](#solver)).
- Doctors only fill roles they are signed off for (`can_*` columns in `staff`)
- Doctors only work while in post (`start_date`/`end_date`)
- Placements: during a placement block, a doctor only works the roles listed for that placement
  (`placement_roles`). An anaesthetics ICU block means ICU1/ICU_DAY only, the ICU tier 2 pool's
  `ICU_TIER2` placement means ICU2/ICU_DAY only, and an OBS block means OBS on-calls and ANA_DAY
  only.
- **Approved** leave is never overwritten. Nights-exempt doctors (e.g. on occupational health
  advice) are never given nights.

### Soft: weighted in the objective
- **Skill mix** (`skill_mix` sheet). For example, at least one ST3+ across ICU1/ANA2/OBS on every
  long day and night (heavy weight), ideally an ST5+ (lighter weight). A shortfall is reported
  rather than making the model infeasible.
- **Fairness:** on-calls, nights and weekends deviate as little as possible from each doctor's
  WTE-weighted share of their pool's total. The share is taken among doctors in the pool who can
  work that category at all (e.g. nights-exempt doctors are left out of nights). A blank `pool`
  puts everyone in one pool. The penalty grows with the distance: the *k*-th
  shift away from the share costs (2*k* − 1) × weight, roughly quadratic. So two doctors one
  shift off cost less than one doctor two shifts off.
- **Requested** (not yet approved) leave, and preference requests (`requests` sheet, with
  per-request weights)
- **Discouraged placement roles** (`penalty` column in `placement_roles`). For example, doctors on
  the general rota can cover OBS on-calls, but at a significant penalty per shift, so obstetric
  on-call goes to OBS-block doctors first.
- **Contracted hours** (`HOURS_TARGET`): each doctor's average weekly hours as close as possible
  to their contracted hours (`contracted_hours` in `staff`; if blank, the rule's value × WTE).
  Approved leave on a day the doctor would otherwise work counts as a normal day's hours. Hours
  short cost more for each further normal day short (the *k*-th day costs (2*k* − 1) × weight per
  hour), so a shortfall is shared. Each hour over costs 10 × weight, so doctors go over only
  when cover needs it.
- **Even normal-day staffing** (`NORMAL_DAY_SPREAD`): each day's count of `ICU_DAY` and
  `ANA_DAY` stays close to a common level for its day type (weekday, weekend, bank holiday),
  rather than piling normal days onto some days. Each doctor-day away from the level (the median)
  costs the weight.
- Weekends no more than 1 in 3 where possible (this avoids the weekend allowance)
- **Run lengths** (`SINGLE_NIGHT`, `LONG_RUN`, `LONG_NIGHT_RUN`): no single nights, and no more
  than 5 shifts in a row. Each isolated single night costs the `SINGLE_NIGHT` weight, and each
  shift beyond 5 in a row (the `LONG_RUN` value) costs the `LONG_RUN` weight, more than a normal
  day short of contracted hours. `LONG_NIGHT_RUN` (off in the test workbook) does the same for
  nights beyond 2 in a row, for departments that want nights in pairs. The hard limits (7 shifts,
  4 nights) still apply.
- **Wellbeing** (`ISOLATED_DAY_OFF`, `SPLIT_WEEKEND`): days off together and whole weekends,
  following the BMA / NHS Employers Good Rostering Guide and the Association of Anaesthetists'
  fatigue guidance. Each single day off between two shifts (leave days aside) and each weekend
  with working time on only Saturday or only Sunday (a Friday night counts as Saturday) costs
  the weight.
- **ICU continuity** (`ICU_SHORT_RUN`): runs of ICU day shifts (`ICU_DAY` and the ICU long days)
  of at least 3 days, so the same doctors follow ICU patients. Each day a run falls short costs
  the weight. With `LONG_RUN`, ICU runs are 3 to 5 days where possible.

The penalty weights are in the `rules` and `skill_mix` sheets, so users can tune them without code.
Each soft rule except fairness is scored exactly as the checker scores it (the row's weight times
the rule's weight), so the solver's penalty for each rule equals the checker's.

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
- **Objective:** minimise the weighted sum of soft-constraint violations and the fairness
  deviations. Each soft rule has a helper variable, e.g. `short[rule, date] ≥ 1 − Σ senior x`
  for skill mix, `single[d,t] ≥ night[t] − night[t−1] − night[t+1]` for isolated nights, or
  `excess[d,w] ≥ Σ x[d, w … w+5] − 5` for runs longer than 5 shifts, with a cost in the objective.
- **Solver:** [HiGHS](https://highs.dev) via `highs::highs_solve()`. The model is built as tidy
  tables with dplyr (one row per variable, one per constraint, one per non-zero) and turned into a
  sparse matrix only at solve time. Variables
  a doctor may never use (wrong competency or placement, leave, LTFT day, not in post) are left
  out, so the test data has about 10k binaries rather than 37k.
- **Unfilled slots:** each cover row has a slack variable costing 100,000 per unfilled slot (more
  than any mix of soft penalties could save). Rest and hours rules have no slack, because an empty
  slot never breaks them. So the model always has a solution, and impossible inputs give a rota
  with gaps instead of just "infeasible".
- **Solve strategy:** HiGHS finds a rota that meets every hard rule in under 10 s, solving only
  the hard-rule rows. Minimising
  the penalties over the whole 13 weeks at once is too slow: the root LP relaxation alone takes
  minutes. So `solve_rota()` improves the first rota by *large neighbourhood search*. It
  repeatedly frees one week, or a random group of 10 doctors, fixes everything else, and
  re-solves that part for up to 5 s (up to 20 s once steps keep failing to improve it), until the
  time limit. Each step solves the full model, so hard rules always hold
  and fairness is judged over the whole rota. The search is seeded, so the same inputs give the
  same rota. If not every slot can be filled, the first rota is instead built a week at a time,
  filling as many slots as possible, before the search improves it.
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
| `staff` | One row per doctor: grade, staffing `pool` (fair shares are measured within a pool), WTE, LTFT fixed day off, start/end in post, nights-exempt, WTR opt-out, competencies (`can_ICU1` … `can_OBS`) |
| `placements` | Training placement blocks: `ICU`, `ICU_TIER2`, `OBS` or `GENERAL`, with dates |
| `placement_roles` | Which roles each placement may work. Unlisted roles are forbidden; `penalty` > 0 means allowed but discouraged (soft, per shift). |
| `leave` | Leave periods: type (`annual`, `study`, `parental`, `exam`), status (`approved` = hard, `requested` = soft) |
| `requests` | Soft preferences: `no_oncall`, `no_night`, `no_weekend`, `prefer_oncall` on a date, with a weight |
| `skill_mix` | Seniority rules across groups of shifts: minimum grade, minimum count, weight |

## Rota types

The `rota_type` setting picks the kind of rota. Each type sets which sheets the inputs workbook
must have, and whether rules left out of its `rules` sheet are on:

| `rota_type` | For | Required sheets | Rules not in the `rules` sheet |
|---|---|---|---|
| `resident` (default) | Resident doctors on the 2016 TCS | all twelve above | on |
| `consultant` | Consultant on-call rotas | `settings`, `rules`, `shifts`, `cover`, `bank_holidays`, `staff`, `leave` | off |

Either type can use these optional pieces:

| Where | What it does | Rule |
|---|---|---|
| `shifts$covers` | Cover slots a shift fills, separated by `;`. A 24-hour `24H` shift covering `2ND;NIGHT` fills the 2nd-on day and the night. Blank means the shift fills its own slot. | `COVER` |
| `shifts$open_to` | `preference`: only doctors with a matching `prefer_shift` row may work it | `PREFERENCE_ONLY` (hard) |
| `shifts$label` | Name of the shift in the workbook key and calendars | |
| `patterns` sheet | Blocks a slot is worked in, within a Mon–Sun week: `pattern_id`, `slot`, `days` (e.g. `Mon;Tue;Wed;Thu`), `open_to` (`all`/`preference`). Each day of a slot outside a block costs the rule's weight; each week is split into blocks the cheapest way. | `PATTERNS` (soft) |
| `PATTERN_UNPREFERRED` rule | Each week a doctor works every day of a preference-only pattern without preferring it, e.g. 1st on Mon–Thu then Fri–Sun for someone who has not asked for 7-day weeks | `PATTERN_UNPREFERRED` (soft) |
| `preferences` sheet | Standing preferences, with a weight each: `prefer_shift` (e.g. 24-hour shifts; working its slots separately costs), `avoid_shift`, `day_off` (`Mon`…`Sun`), `prefer_pattern` (unlocks a preference-only pattern; other blocks for the slot cost), `avoid_pattern` | `PREFERENCES` (soft) |
| `sequences` sheet | `to_shift` worked `days_after` days after `from_shift`, e.g. no 1st on the day before a night. A blank weight makes the row hard. | `SEQUENCE` (hard), `SEQUENCE_SOFT` |
| `FAIR_SLOTS` rule | Each on-call slot shared by WTE, so a 24-hour shift counts towards both slots it fills. `summarise_doctors()` adds a count and fair share for each slot. | `FAIR_SLOTS` (soft) |

[`icu_consultant_inputs.xlsx`](inst/extdata/icu_consultant_inputs.xlsx) is a worked example: 15 ICU
consultants (most at 0.75 WTE, giving a quarter of their time to emergency medicine, anaesthesia or
respiratory medicine) covering 1st on and 2nd on (08:00–18:00) and nights (17:00–08:00, on site to
22:00) for a year. 1st on runs in Mon–Thu and Fri–Sun blocks, or whole weeks for the two who prefer
them. Nights are split each week between a Mon/Wed/Fri/Sun consultant and a Tue/Thu/Sat one. Four
consultants prefer 24-hour shifts. Consultants are off the day after a night (`MIN_REST` 11 h), never
1st on the day before a night, and work at most 1 weekend in 3. The search frees four whole weeks or
five consultants at a time (`neighbourhood_days`, `neighbourhood_doctors` in `settings`).

To add another kind of rota, such as anaesthetic consultants: write a template workbook, reuse a
type or add one to `rota_types` in `R/rules.R`, and register any new rule in `rule_registry()` with
the function that adds it to the model and the one that checks it.

## Compliance checker

`check_rota()` checks any rota, whether solved or edited by hand, against every rule in the `rules`
sheet. It works on real clock times: each shift becomes an interval, so a night starting 20:00 ends
08:30 the next day. It deliberately does not reuse the optimiser's formulation, so each can catch
mistakes in the other.

```sh
Rscript scripts/check_rota.R inputs.xlsx rota.xlsx report.xlsx
```

The rota workbook needs a `long` sheet (`doctor_id`, `date`, `code`), as `rota.xlsx` from the
solver has. This prints the number of violations for each rule and writes a report workbook with three sheets:
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
patterns <- summarise_patterns(rota, inputs)  # run lengths, single nights, split weekends

# File paths work too
check_rota("out/rota.xlsx", "inst/extdata/rota_inputs.xlsx")
```

Each function has a help page, e.g. `?check_rota`, `?read_rota_inputs`, `?rotawriter`.

## Solver

```sh
Rscript scripts/solve_rota.R inputs.xlsx out/ [time_limit_s]
```

This solves the rota, runs the checker on it, and writes to `out/`:

| File | Contents |
|---|---|
| `rota.xlsx` | `grid`: dates down, doctors across, with shift and leave codes (`AL`, `SL`, `EX`, `PL` for approved leave). Weekends and bank holidays are shaded; nights and long days are coloured. `by_shift`: dates down, shifts across, showing who is on, with `UNFILLED` where cover is short. `doctors`: each doctor's hours against contracted hours, and on-calls, nights and weekends against their fair share. `key`: what the codes mean. `long`: the format the checker reads, so a hand-edited rota can be re-checked. |
| `report.xlsx` | The checker's compliance and fairness report: violations by rule, one row per violation, and per-doctor totals with each doctor's distance from their contracted hours (`hours_vs_contracted`, hours a week) and fair shares, shaded blue (fewer) to red (more). A `patterns` sheet counts each doctor's runs of shifts, nights and ICU days, single nights, split weekends and single days off, with department totals. |
| `calendars/` | One `.ics` file per doctor with their shifts and approved leave, to import into Outlook, Google Calendar or Apple Calendar. |

It exits with status 1 if no rota was found or it breaks a hard rule, including leaving slots
unfilled.

From R:

```r
solution <- solve_rota(inputs)          # time limit from the settings sheet
solution$rota                           # long format: doctor_id, date, code
solution$unfilled                       # slots that could not be filled (usually none)
solution$penalties                      # penalty per soft rule
solution$history                        # penalty after each improvement
check_rota(solution$rota, inputs)       # no hard violations; soft ones match the penalties
summarise_doctors(solution$rota, inputs)  # fairness: counts against fair shares
summarise_patterns(solution$rota, inputs) # run lengths and wellbeing patterns
write_rota(solution$rota, inputs, "rota.xlsx")
write_check_report(check_rota(solution$rota, inputs), summarise_doctors(solution$rota, inputs),
                   "report.xlsx", patterns = summarise_patterns(solution$rota, inputs))
write_calendars(solution$rota, inputs, "calendars")
build_rota_model(inputs)                # the model itself: sizes per rule
```

If the inputs make some slots impossible to fill, for example too few doctors with a
competency, or too many on leave at once, `solve_rota()` still returns a rota. It leaves those
slots empty, never breaks a contract rule to fill them, sets `status = "Unfilled shifts"` and lists
them in `unfilled`. The checker reports the same slots as `COVER` violations. For example, with
nobody signed off for OBS, exactly the 182 OBS slots are left empty and everything else is filled.

## Development

The project is an R package (`rotawriter`), loaded with `pkgload::load_all()`.

```
README.md
LICENSE.md                    AGPL-3.0-or-later (code)
LICENSE-docs.md               CC BY-SA 4.0 (documentation)
DESCRIPTION, NAMESPACE        package metadata
R/read_inputs.R               read_rota_inputs(), validate_inputs(), read_rota()
R/rules.R                     rota types and the rule registry
R/check_rota.R                check_rota(), summarise_doctors(), summarise_patterns()
R/report.R                    write_check_report()
R/model.R                     build_rota_model(): the MILP as tidy tables
R/soft.R                      soft rules: objective penalties
R/patterns.R, R/preferences.R, R/sequences.R  block patterns, standing preferences, shift sequences
R/solve.R                     solve_rota(): first rota, then neighbourhood search
R/output.R                    write_rota(), write_calendars()
R/app.R, R/app_*.R            run_app(): the Shiny app (inputs, solve, results, check tabs)
man/                          help pages, generated by roxygen2 (do not edit)
scripts/check_rota.R          command-line checker
scripts/solve_rota.R          command-line solver
scripts/run_app.R             starts the app from the source folder
tests/testthat/               tests for each rule, plus the planted violations
docs/contract-rules.md        rules reference with sources
data-raw/make_test_data.R     regenerates the test workbooks (deterministic)
data-raw/make_consultant_data.R  regenerates the ICU consultant inputs
inst/extdata/rota_inputs.xlsx test inputs: 42 fictional doctors in two pools, 13 weeks from 1 Feb 2027
inst/extdata/icu_consultant_inputs.xlsx  15 fictional ICU consultants, 52 weeks from 1 Feb 2027
inst/extdata/example_rota_output.xlsx  a rota that breaks the rules on purpose (see below)
```

`example_rota_output.xlsx` is a naive round-robin rota, not an optimised one. Its
`known_violations` sheet lists the contract breaches planted in it, and the tests check that the
checker finds every one.

Regenerate the test data, rebuild the help pages (and `NAMESPACE`) from the roxygen comments, and run
the tests:

```sh
Rscript data-raw/make_test_data.R
Rscript data-raw/make_consultant_data.R
Rscript -e 'roxygen2::roxygenise()'
Rscript -e 'pkgload::load_all(); testthat::test_dir("tests/testthat", load_package = "none")'
```

## Roadmap

**Done:** input workbook and validation, independent compliance checker, solver with hard and soft
rules, coordinator workbook, compliance report and `.ics` calendars, and separate staffing pools
with their own fair shares. On the test data (42 doctors in two pools), a 30-minute solve fills
every slot with no hard violations. Every long day and night has an ST3+ across ICU1/ANA2/OBS,
and all but 7 have an ST5+. 1 requested leave day is refused. Every doctor is within 1.3 h a
week of their contracted hours. 1 run is 6 shifts long, 10 nights are single and 16 weekends are
split, out of about 600 runs and 170 night runs.
That rota, its compliance report and the calendars are in
[`examples/test-data/`](examples/test-data/).

A local app (`run_app()`) to upload the workbook, solve with live progress, view and check rotas,
and download the results.

Rota types (`resident`, `consultant`) with block patterns, standing preferences, 24-hour shifts
and shift sequences. On the ICU consultant example, a 30-minute solve of the whole year fills every
slot with no hard violations and every 1st-on day and night in a block. Only the two consultants who
prefer them work 7-day 1st-on weeks. Each consultant is within 2 of their fair share of 1st-on days,
and within 1 of their 2nd-on days, nights and weekends. 16 shifts fall on a preferred day off and 7
weekends are split. That rota, its compliance report and the calendars are in
[`examples/icu-consultants/`](examples/icu-consultants/).

**Next:**
1. Editing leave and requests in the app, and saving and reopening sessions.
2. Re-solving mid-rota with existing shifts fixed, swaps, and non-resident on-call.

## Prior art

There is no maintained open-source NHS rota **optimiser** that we know of. Related work:

- [RudolfCardinal/rota](https://github.com/RudolfCardinal/rota): Python rota design and checking
  tool (banding, New Deal/EWTD). It is a checker, not an optimiser. Apache-2.0.
- [BMA rota checker](https://bma.org.uk/pay-and-contracts/working-hours/work-schedule/bma-rota-checker):
  the reference compliance check for the 2016 contract.
- Academic work: a [2017 rostering problem set based on the junior doctor contract](https://e-space.mmu.ac.uk/619524)
  (simulated annealing); an [anaesthetist rota system using constraint satisfaction](https://aaai.org/?p=65521);
  and a [MIP model for anaesthetist rostering](https://ftsm.ukm.my/v6/public/assets/file/technicalreport/LP-CAIT-FTSM-2021-001.pdf).
- Industry: [Hartree Centre / Alder Hey on-call optimisation](https://www.hartree.stfc.ac.uk/case-studies/2026/03/09/modernising-nhs-staffing-schedules-with-advanced-rota-optimisation/) (2026),
  and Lantum (built on the open-source [Timefold](https://timefold.ai) solver).
- General tools: Google OR-Tools CP-SAT and Timefold. Nurse rostering is the closest
  well-studied problem.

## Disclaimer

This is not an official BMA or NHS Employers tool. Any rota it produces must still be reviewed by
the rota coordinator and be consistent with the generic work schedule. Concerns go to the
Guardian of Safe Working. All names in the test data are fictional.

## Licence

Copyright © 2026 Mark Jeffrey.

- **Code** (everything not listed below, including the R package, scripts, tests, test data and
  generated help pages) is licensed under the
  [GNU Affero General Public License v3.0 or later](LICENSE.md) (`AGPL-3.0-or-later`). If you run
  a modified version as a service that others use over a network (for example a hosted rota app),
  you must offer those users its source code.
- **Documentation** (`README.md` and the `docs/` folder) is licensed under
  [Creative Commons Attribution-ShareAlike 4.0 International](LICENSE-docs.md) (`CC-BY-SA-4.0`).

Rotas, reports and calendars produced with the tool belong to whoever produces them and are not
covered by either licence.
