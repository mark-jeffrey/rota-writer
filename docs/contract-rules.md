# Contract rules reference

Rota safety rules from the **2016 Terms and Conditions of Service (TCS) for
resident doctors in England, Schedule 3**, as amended by the 2018/2019 contract
refresh. Each rule has a `rule_id` that matches the `rules` sheet in
`data/test/rota_inputs.xlsx`. Users can change the parameters there, and the
model reads them from that sheet, so nothing is hard-coded.

**Hard** rules must be satisfied, otherwise the model is infeasible. **Soft**
rules are penalised in the objective function with the weight given in the
`rules` sheet.

## Working hours and rest

| rule_id | Rule | Parameter | Type | How modelled |
|---|---|---|---|---|
| `AVG_HOURS` | Average weekly working hours over the rota reference period | ≤ 48 h × WTE (≤ 56 h if opted out of the Working Time Regulations) | Hard | Sum of shift hours per doctor ≤ limit × weeks in post |
| `MAX_7DAY` | Maximum hours in any 168-hour period | ≤ 72 h | Hard | Sliding 7-day window over day index: Σ hours ≤ 72 |
| `MAX_SHIFT` | Maximum shift length | ≤ 13 h | Hard | Checked on the `shifts` sheet at load time; no shift may exceed it |
| `MIN_REST` | Minimum rest between shifts | ≥ 11 h | Hard | Pre-computed table of incompatible shift pairs on consecutive days (e.g. night → any day shift next day) |
| `MAX_CONSEC_SHIFTS` | Maximum consecutive shifts | ≤ 7, then 48 h rest | Hard | Any 8-day window has ≤ 7 working days; after 7 days in a row, the next 2 days are off |
| `MAX_CONSEC_LONG` | Maximum consecutive long shifts (> 10 h) | ≤ 4, then 48 h rest | Hard | Sliding window on `is_long` shifts that are not nights (nights have their own rules), plus forced rest days |
| `MAX_CONSEC_LATE` | Maximum consecutive long evening shifts (finishing after 23:00) | ≤ 4, then 48 h rest | Hard | Same pattern on `is_late` shifts. Not used by the test shift set, but kept for completeness |
| `MAX_CONSEC_NIGHTS` | Maximum consecutive night shifts | ≤ 4 | Hard | Any 5-day window has ≤ 4 nights |
| `REST_AFTER_NIGHTS` | Rest after any run of nights | ≥ 46 h | Hard | If night on *d* and no night on *d+1*, then no shift on *d+1* or *d+2* |
| `WEEKEND_MAX` | Weekend working frequency | no more than 1 in 2 | Hard | A weekend is worked if any shift falls between 00:00 Saturday and 24:00 Sunday (so Friday nights count). The checker tests the frequency over the doctor's weekends in post. The solver may use the stricter "never two weekends in a row" form, which implies the frequency rule. |
| `WEEKEND_TARGET` | Weekend frequency target | no more than 1 in 3 | Soft | Penalty per weekend worked above 1 in 3 over the doctor's weekends in post (more than this triggers the weekend allowance) |
| `BREAKS` | Breaks within shifts | 30 min per 5 h worked (approx.) | Not modelled | Managed on the day; outside the rota |

### How the checker interprets these rules

- Times are wall-clock times with no BST adjustment, so the night when the clocks change counts as 12.5 h.
- Average hours are total rostered hours divided by weeks in post within the rota period. Leave
  days count as zero hours.
- Consecutive runs are counted by the date each shift starts. A run's rest period is measured from
  the end of its last shift to the start of the doctor's next shift of any kind.
- A shift clashes with leave if any part of it falls on a leave day, so a night may not finish
  on the first morning of leave.

## Less than full time (LTFT)

| rule_id | Rule | Type | How modelled |
|---|---|---|---|
| `LTFT_PRORATA` | Hours, on-calls, nights and weekends pro rata to WTE | Hard (hours), Soft (fairness of distribution) | Hours cap × WTE; fairness deviations measured against the WTE-weighted share |
| `LTFT_FIXED_DAY` | No work on an agreed fixed non-working day | Hard | x = 0 on that weekday |

## Non-resident on-call (for future use; the test rota is all resident)

| rule_id | Rule | Parameter |
|---|---|---|
| `NROC_MAX_CONSEC` | Maximum consecutive non-resident on-call periods | ≤ 3 (except by agreement) |
| `NROC_REST` | Expected rest during on-call | 8 h per 24 h, of which 5 h continuous between 22:00 and 07:00 |
| `NROC_NEXT_DAY` | Next day's work after on-call | ≤ 10 h |

## Rostering process (not optimisation constraints)

- Rota template issued **8 weeks** before the start, and the individual duty roster **6 weeks** before.
- Changes to an issued rota need **6 weeks'** notice.
- The April 2024 agreement says rota planners should honour annual leave that is already booked. In the model, leave with status `approved` is **hard**, and leave with status `requested` is **soft** with a high weight.

## Sources

- BMA, [Good rostering guide](https://www.bma.org.uk/pay-and-contracts/working-hours/work-schedule/managing-rotas-and-duty-rosters-for-resident-doctors-in-england/good-rostering-guide)
- BMA, [Rota checker](https://bma.org.uk/pay-and-contracts/working-hours/work-schedule/bma-rota-checker)
- NHS Employers, [2018 contract refresh](https://nhsemployers.org/your-workforce/2018-contract-refresh)
- East Sussex Healthcare NHS Trust, [Rota rules at a glance factsheet](https://www.esht.nhs.uk/medical-education/wp-content/uploads/2019/03/Factsheet-rota-rules-at-a-glance.pdf)
- Lancashire Teaching Hospitals, [Exception reporting fact sheet](https://healthacademy.lancsteachinghospitals.nhs.uk/app/uploads/2021/03/Appendix-1-Exception-Reporting-Fact-Sheet.pdf)
- GOV.UK, [Junior doctors contract offer: main points](https://www.gov.uk/government/publications/junior-doctors-contract-offer-main-points/junior-doctors-contract-offer-main-points)
