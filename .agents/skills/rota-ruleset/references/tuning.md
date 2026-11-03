# Weights and tuning

## How the penalty works

The solver minimises one total: the sum, over soft rules, of the **rule's weight** (in `rules`)
× the **number of times it's broken** × the **row weight**, where the rule has rows (`requests`,
`preferences`, `skill_mix`, `sequences`, `placement_roles$penalty`). Hard rules are never
traded off. An unfilled slot costs 100,000, so keep every realistic bundle of soft penalties well
below that. Otherwise the solver would rather leave a slot empty.

Only ratios matter: doubling every weight changes nothing. So compare penalties for the same
amount of harm, in the units below.

| Rule | One unit of penalty is… |
|---|---|
| `LEAVE_REQUESTED` | a requested leave day not granted |
| `REQUESTS` | a request not met (× its row weight) |
| `PREFERENCES` | a shift against a standing preference (× its row weight). `prefer_shift`: each time its slots are worked separately instead. |
| `SKILL_MIX` | a shift short of the minimum (× the row weight) |
| `PATTERNS` | a day of a slot worked outside a block |
| `PATTERN_UNPREFERRED` | a week of a preference-only pattern worked in full by someone who hasn't asked for it |
| `SEQUENCE_SOFT` | an occurrence (× the row weight) |
| `PLACEMENT_AVOID` | a shift in a discouraged role (× `placement_roles$penalty`) |
| `SINGLE_NIGHT` | an isolated night |
| `LONG_RUN`, `LONG_NIGHT_RUN` | a shift (or night) beyond the target run length |
| `ICU_SHORT_RUN` | a day an ICU run falls short of the target |
| `ISOLATED_DAY_OFF` | a single day off between two shifts |
| `SPLIT_WEEKEND` | a weekend with only one day worked |
| `WEEKEND_TARGET` | a weekend above the 1-in-N target |
| `FAIR_*` | the *k*-th shift away from a person's WTE share costs (2*k* − 1) × weight for *k* ≤ 3, then 7 × weight per further shift. So two people 1 off (2×) is cheaper than one person 2 off (4×). |
| `HOURS_TARGET` | an hour short: (2*k* − 1) × weight per hour for the *k*-th normal day short; an hour over costs 10 × weight |
| `NORMAL_DAY_SPREAD` | a doctor-day away from the median staffing for that day type |

## Worked ladders

**Resident** (`data-raw/make_test_data.R`). Agreed order: requested leave > top wellbeing
(whole weekends, runs ≤ 5) > single nights > weekend target > isolated days off > fairness and
normal-day spread > hours > ICU continuity.

| Rule | Weight | The check behind it |
|---|---|---|
| `LEAVE_REQUESTED` | 500 | One refused day must outweigh a bad week: 6-shift run (150) + split weekend (150) + single night (100) = 400 |
| Skill mix ST3+ / ST5+ | 1000 / 100 (row weights) | Missing the only ST3+ is worse than a refused leave day |
| `SPLIT_WEEKEND`, `LONG_RUN` | 150 each | The top wellbeing items |
| `SINGLE_NIGHT` | 100 | |
| `WEEKEND_TARGET` | 50 | |
| `ISOLATED_DAY_OFF` | 30 | |
| `FAIR_*`, `NORMAL_DAY_SPREAD` | 10 | One shift off the share costs about one isolated day off ÷ 3 |
| `HOURS_TARGET` | 5 per hour | A 9 h normal day short costs 45: patterns beat hours |
| `ICU_SHORT_RUN` | 5 | Tie-breaker only |

**Consultant** (`data-raw/make_consultant_data.R`). Agreed order: requested leave > blocks >
split weekends > fairness > individual preferences.

| Rule | Weight | The check behind it |
|---|---|---|
| `LEAVE_REQUESTED` | 500 | A refused day costs more than breaking a block |
| `PATTERNS`, `PATTERN_UNPREFERRED` | 100 | Blocks are a strong soft preference |
| Prefer 7-day weeks / prefer 24 h (row weights × `PREFERENCES` 1) | 50 / 20 | |
| `SPLIT_WEEKEND` | 20 | |
| `FAIR_SLOTS`, `FAIR_WEEKENDS` | 10 | |
| Avoid a night pattern / day off (row weights) | 10 / 5 | Low: 1st-on and night weeks usually override them |

## Turning a ranking into weights

1. Start from the nearest worked ladder above. Keep its weights where the user's ranking agrees.
2. For each pair of neighbouring ranks, write down one realistic trade-off and check that the
   weights decide it the way the user would. Compare bundles that really happen together (a bad
   week, a bad weekend), not single units.
3. Leave a gap of at least 2–3× between ranks that should strictly dominate. Use similar weights
   for things the user is happy to trade evenly.
4. Weight anything the user calls a tie-breaker at under a tenth of the rank above it.
5. Write each weight and its check in the spec.

## Trial solve

From R, at the project root. Change the paths, time limit and run name for each run:

```r
pkgload::load_all(quiet = TRUE)
dept <- "local/<dept>"
run <- "try01"
inputs <- read_rota_inputs(file.path(dept, "inputs_short.xlsx"))
stopifnot(nrow(validate_inputs(inputs)) == 0)
solution <- solve_rota(inputs, time_limit = 300, seed = 1)
print(solution)

out <- file.path(dept, "out", run)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
violations <- check_rota(solution$rota, inputs)
doctors <- summarise_doctors(solution$rota, inputs)
patterns <- summarise_patterns(solution$rota, inputs)
write_rota(solution$rota, inputs, file.path(out, "rota.xlsx"))
write_check_report(violations, doctors, file.path(out, "report.xlsx"), patterns = patterns)
write.csv(solution$penalties, file.path(out, "penalties.csv"), row.names = FALSE)
write.csv(solution$history, file.path(out, "history.csv"), row.names = FALSE)
```

When a report sheet has nothing to list, `write_check_report()` warns "Found data table with zero
rows". That's harmless.

## Review pack

Show these in plain words, with numbers and named examples:

1. **Hard rules and cover:** `dplyr::count(violations, hard_soft, rule_id)` and
   `solution$unfilled`. Anything here is a structure or feasibility problem, not a weights one.
2. **Penalty by rule:** `solution$penalties`, sorted. Compare it with the previous run.
3. **Fairness:** for each category, the spread of count − fair share across people
   (`doctors$on_calls - doctors$fair_on_calls`, and the same for nights and weekends; the
   `FAIR_SLOTS` columns where used). Name the people furthest from their share.
4. **Patterns:** totals of `patterns` (runs, longest run, single nights, split weekends, single
   days off), and the people with the most.
5. **Leave and requests:** the `LEAVE_REQUESTED`, `REQUESTS` and `PREFERENCES` rows of
   `violations`: who, when, what.
6. **Two sample weeks** of the grid, picked to show the most common problem:
   `solution$rota |> dplyr::filter(date >= start, date < start + 7) |> tidyr::pivot_wider(names_from = date, values_from = code)`.
7. **Did the search finish?** If `history` still showed improvements near the time limit, the
   rota may get better with more time. Say so before blaming the weights.
8. **Against the baseline**, if there is one: hard violations, penalty by rule and fairness spread,
   side by side.

## Diagnosing

- **The same complaint after a weight change:** the rota may be stuck because of structure. Check
  whether the convention is possible at all (cover, competencies, sequences that rule it out,
  leave).
- **A fix in one place creates a problem somewhere else:** the two rules compete. Ask the user
  which matters more in this specific case, then set the ratio.
- **Results jump around between runs:** the solve is too short for the rota's size. Lengthen it,
  or for block rotas raise `neighbourhood_days` (whole weeks). For many people with few
  interactions, raise `neighbourhood_doctors`.
- **Every weight is high:** only ratios matter. If a weight is creeping towards 100,000 (the cost
  of an empty slot), rescale everything down.
