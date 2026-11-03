# Requirement → sheet / rule map

How to express common rota requirements in an inputs workbook. The README sections "The inputs
workbook", "Rota types" and "Rule catalogue" have the full column lists. `data-raw/template_parts.R`
(`rule_library`) has every rule with its usual description, value and weight. If a requirement
isn't here, check `rule_registry()` in `R/rules.R` before calling it a gap.

`resident` workbooks need all twelve sheets. `consultant` workbooks need `settings`, `rules`,
`shifts`, `cover`, `bank_holidays`, `staff` and `leave`. Any type can use the other sheets.

## Structure

| Requirement | How | Notes |
|---|---|---|
| Rota dates, type, time limit | `settings`: `rota_type`, `rota_start`, `rota_end`, `solver_time_limit_s` | Block rotas (`consultant`) must start on a Monday |
| Shift times and hours | `shifts`: `shift_code`, `role`, `shift_type` (`normal_day`/`long_day`/`night`), `start`, `end`, `hours` | `hours` must match the clock times. A night ends the next morning. |
| What counts as on-call, night, long or late | `shifts`: `is_on_call`, `is_night`, `is_long` (> 10 h, not a night), `is_late` (finishing after 23:00) | Drives the run limits and fairness categories |
| One shift fills two slots (e.g. 24 h = 2nd-on day + night) | `shifts$covers` = `2ND;NIGHT` | Counts once for each slot under `FAIR_SLOTS` |
| Only volunteers work a shift | `shifts$open_to` = `preference`, plus a `prefer_shift` row in `preferences` | Hard (`PREFERENCE_ONLY`) |
| How many on each shift | `cover`: per `shift_code` and `day_type` (`weekday`/`weekend`/`bank_holiday`), `n_min`/`n_max` | Exact for on-call (`n_min = n_max`); a minimum only for normal days. **Not** per date: one-off extra cover is a gap. |
| Bank holidays | `bank_holidays` sheet; `england_bank_holidays()` for England and Wales | Other nations: type them in |
| Normal days to top up hours | `normal_day` shifts with a cover minimum, plus `HOURS_TARGET` and `NORMAL_DAY_SPREAD` | Resident rotas. Consultant rotas usually leave the other sessions out of the rota and set WTE to the rota's share of the job. |

## Who can work what

| Requirement | How | Notes |
|---|---|---|
| Competency or sign-off | `staff$can_<ROLE>` = Y/N, one column per role | Hard (`COMPETENCY`). With no `can_*` columns, anyone can work any role. |
| Blocks or placements limit the roles (ICU block, obstetric block) | `placements` (who, when) + `placement_roles` (which roles each placement may work) | Hard (`PLACEMENT`). `penalty` > 0 allows a role but discourages it (`PLACEMENT_AVOID`, per shift). |
| Separate pools with their own fair shares | `staff$pool` | Shares are measured within each pool. Blank means one pool. |
| Part-time | `staff$wte`; `contracted_hours` if it isn't the rule value × WTE | Fair shares and hours are pro rata |
| LTFT fixed day off | `staff$ltft_fixed_day_off` (`Mon`…`Fri`) | Hard (`LTFT_FIXED_DAY`) |
| No nights (occupational health) | `staff$nights_exempt` = Y | Hard (`NIGHTS_EXEMPT`); left out of the night shares |
| Joins or leaves mid-rota | `staff$start_date`, `end_date` | Hard (`IN_POST`) |
| Opted out of the Working Time Regulations | `staff$opted_out_wtr` = Y | Uses `AVG_HOURS_OPTOUT` |
| Senior cover across tiers | `grades` (grade → rank) + `skill_mix` rows (shifts, `min_grade`, `min_count`, weight) | Soft, per shift short. Use a heavy weight for "must", lighter for "ideally". |

## Contract and safety limits (hard)

| Requirement | Rule | Value |
|---|---|---|
| Average weekly hours | `AVG_HOURS` (`AVG_HOURS_OPTOUT`) | 48 (56) × WTE |
| Hours in any 7 days | `MAX_7DAY` | 72 |
| Longest shift | `MAX_SHIFT` | 13 h |
| Rest between shifts | `MIN_REST` | 11 h. For consultants, 11 h also gives "off the day after a night". |
| Consecutive shifts / long shifts / late shifts, then rest | `MAX_CONSEC_SHIFTS`, `MAX_CONSEC_LONG`, `MAX_CONSEC_LATE`, `REST_AFTER_RUN` | 7 / 4 / 4, then 48 h |
| Consecutive nights, then rest | `MAX_CONSEC_NIGHTS`, `REST_AFTER_NIGHTS` | 4, then 46 h |
| Weekend frequency | `WEEKEND_MAX` | 1 in N (2 under the TCS; 3 in the consultant example) |
| Approved leave | `leave` with `status` = `approved` (`LEAVE_APPROVED`) | |
| Never X the day before Y | `sequences` row with a blank weight (`SEQUENCE`) | `days_after` counts days |

Under `resident`, all of these are on unless switched off in `rules`. Under `consultant`, only the
listed ones apply. Change a limit through its `value` in `rules`, never in code.

## Conventions and wellbeing (soft)

| Requirement | Rule or sheet | Scored per |
|---|---|---|
| Work in blocks (Mon–Thu / Fri–Sun; nights split Mon/Wed/Fri/Sun + Tue/Thu/Sat) | `patterns` sheet + `PATTERNS` | day of a slot outside a block |
| Allowed odd days in a patterned slot (e.g. single weekday on-calls, with Fri–Sun as a block) | One-day patterns (`days` = `Mon`, …) beside the block patterns | Without them, every day worked outside a listed block costs |
| A block only some people want (e.g. 7-day weeks) | `patterns$open_to` = `preference` + `prefer_pattern`; `PATTERN_UNPREFERRED` | week worked in full without preferring it |
| Avoid X shortly after Y | `sequences` row with a weight (`SEQUENCE_SOFT`) | occurrence × row weight |
| No block straight after another (e.g. 1st on Fri–Sun then Mon–Thu) | `pattern_sequences` row + `PATTERN_SEQUENCE` | pair × row weight |
| Rest either side of a big block (e.g. 48 h around a 7-day week or a week of 24-hour shifts) | `block_rest` sheet + `BLOCK_REST` | shift in the rest |
| No night blocks back to back | `CONSEC_NIGHT_WEEKS` | pair of weeks running with nights in both |
| No single nights | `SINGLE_NIGHT` | isolated night |
| Runs of at most N shifts / N nights | `LONG_RUN` / `LONG_NIGHT_RUN` (value = N) | shift beyond N |
| Continuity: runs of at least N days in a unit | `ICU_SHORT_RUN` | day short. ICU-specific: day shifts whose `role` starts with `ICU`. Another unit is a gap. |
| Days off together | `ISOLATED_DAY_OFF` | single day off between shifts |
| Whole weekends | `SPLIT_WEEKEND` | weekend with only one day worked |
| Weekends no more than 1 in N where possible | `WEEKEND_TARGET` | weekend above 1 in N |

## Leave, preferences, fairness, hours (soft)

| Requirement | Rule or sheet | Scored per |
|---|---|---|
| Requested (not yet approved) leave | `leave` with `status` = `requested` (`LEAVE_REQUESTED`) | day not granted |
| One-off requests: no on-call, no night, no weekend, or wants an on-call on a date | `requests` sheet (`REQUESTS`) | request not met × row weight |
| Standing preferences: prefer or avoid a shift, a regular day off, prefer or avoid a pattern | `preferences` sheet (`PREFERENCES`) | occurrence × row weight |
| Share on-calls / nights / weekends evenly | `FAIR_ONCALL`, `FAIR_NIGHTS`, `FAIR_WEEKENDS` | roughly quadratic in the distance from the WTE share |
| Share each slot evenly (1st on, 2nd on, nights, …) | `FAIR_SLOTS` | as above, per slot |
| Hours close to contracted | `HOURS_TARGET` | hour short (rising); hour over at 10× |
| Normal days spread evenly over the rota | `NORMAL_DAY_SPREAD` | doctor-day away from the median |

## Known gaps (as of this skill's writing)

Check the README roadmap and `rule_registry()` first, as these may have been added since.

- Non-resident on-call limits (`NROC_*` in `docs/contract-rules.md`): listed, not modelled.
- Cover that varies by date (beyond weekday, weekend and bank holiday), e.g. extra cover in
  winter.
- Limits on how many of a particular shift one person works, other than fairness, e.g. "at most
  two 24 h shifts a month".
- Pairing rules ("these two never on together", "the same registrar all week with a consultant").
- Continuity runs for units other than ICU.
- Fixed rolling lines (each person follows a line of a template rota) as a hard structure.
  Patterns approximate it.
- Re-solving mid-rota with issued shifts fixed, and swaps (on the roadmap).
