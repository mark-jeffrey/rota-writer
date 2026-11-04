---
name: rota-ruleset
description: Guide a rota coordinator through setting up rota-writer for their own department or staff group. Interview them about the rota's structure, contract, workforce, conventions and preferences; write a plain-English spec; build their inputs workbook from the shared template parts; check it's feasible; then solve trial rotas and tune the penalty weights from their feedback. Use when someone wants a new rota, a new department, new rules or a new rota type, or wants to tune the weights of an existing one.
---

# Set up a rota for a new department

You are helping a rota coordinator (often a clinician, rarely a programmer) turn "how our rota
works" into an inputs workbook that rota-writer can solve, with penalty weights that match their
priorities. They know the rota. You know the tool. Your job is to translate between the two,
honestly, including when the tool can't do something yet.

If rota-writer isn't installed and working, run the `rota-setup` skill first. Tuning also needs
tidyverse: `Rscript scripts/setup.R --templates`.

## Working rules

- **Everything goes in the spec.** Keep `local/<dept>/rota-spec.md`, starting from
  [references/spec-template.md](references/spec-template.md). Write each answer into it as you
  go. Log every decision with its date and reason in the decision log. The spec, not your memory,
  is the record: another session or another agent must be able to pick up from it.
- **Work on anonymised or dummy data.** Whatever the user shows you goes to your model's provider.
  The rule set depends on the shape of the staff group (head count, grades, WTE, competencies, LTFT
  days, typical leave), not on who they are. So ask for staff codes (`C01`, "Consultant A") instead
  of names, and realistic but made-up leave and preferences, or generate them as the
  `data-raw/make_*_data.R` scripts do. If the user starts pasting real names or leave, stop, explain
  why, and suggest anonymising it, unless they confirm their organisation has approved this AI tool
  for personal data. The user puts the real data in themselves after handover (phase 8).
- **Everything stays in `local/`** (gitignored). Never commit it or send it to any other outside
  service. Commit a template only if the user asks, and only with fictional staff.
- **Ask a few questions at a time** (three to five), in their language, not the tool's. After each
  batch, read your understanding back in a short summary and ask them to correct it.
- **Don't guess what matters to them.** Whether a requirement is hard or soft, and how heavily it
  weighs, are their decisions. Offer a default with a reason, then ask.
- **Hard or soft?** Ask: "If the only way to meet this were to leave a shift empty, which would you
  rather?" If they'd break it, it's soft. Contract limits are hard. Keep the other hard rules few:
  each one makes the rota less flexible.
- **Make trade-offs concrete.** Ask about examples ("Dr A gets their requested Friday off, but Dr B
  then works 6 days in a row: right call?"), not about abstract weights.
- **Say what the tool can't do.** If a requirement doesn't fit, say so and log it as a gap
  (phase 3). Don't quietly approximate it.

## Phase 1: Intake

Goal: a complete description of the rota, in the spec. Use the spec template's sections as your
checklist, roughly in this order:

1. **Who and which contract.** The staff group (resident doctors, consultants, SAS doctors,
   nurses, …), the country and contract (e.g. 2016 TCS for resident doctors in England), and the
   rota's length and start date. This decides the base rota type:
   - `resident`: every rule in the library is on unless switched off. Use it for any group on the
     2016 resident doctor TCS.
   - `consultant`: only the rules listed in the `rules` sheet apply. Use it for any group without
     those contractual limits, or with their own (consultants, SAS, other professions). Start with
     the hard rules they name.
   - A new type, only if neither fits the defaults (e.g. a different search strategy). See phase 3.
2. **Structure.** The roles or tiers, the shifts each one works (start and end times, on site or
   from home), how many are needed on each shift by day type (weekday, weekend, bank holiday), and
   the normal (non-on-call) days, if the rota should place them.
3. **Workforce**, by staff code, not name. Head count, grades, WTE (and what makes up the rest of each person's job), start
   and end dates, LTFT fixed days off, nights exemptions, staffing pools that are shared fairly
   among themselves, and who can work which role (competencies, placements).
4. **Skill mix.** Seniority needed together on a shift, e.g. "at least one ST3+ across these three
   tiers at night".
5. **Conventions to keep.** Blocks (e.g. Mon–Thu / Fri–Sun), how nights are split, sequences
   ("never 1st on the day before a night", "off the day after a night"), and continuity. **Ask for
   the current rota**, as an export or a screenshot, with names replaced by the same staff codes:
   it is the best evidence of conventions people rely on without saying so. If they can export it in long format (`doctor_id`, `date`, `code`),
   save it in `local/<dept>/current_rota.xlsx` as a baseline for later.
6. **Leave and preferences.** How leave is booked, and which requests should be firm (approved)
   and which flexible (requested). Standing preferences (e.g. "prefers 24-hour shifts", "never
   Tuesdays"). One-off requests.
7. **Fairness.** What should be shared out evenly (on-calls, nights, weekends, each slot
   separately), and among whom (the whole group or each pool).

Exit when the user agrees that the spec describes their rota.

## Phase 2: Map requirements to the tool

For each requirement, find the sheet, column or rule that expresses it, using
[references/rule-map.md](references/rule-map.md). Fill in the spec's requirement map, tagging each
row as:

- **supported**: an existing sheet or rule does exactly this;
- **approximated**: close enough with existing features. Say what differs, and confirm with the
  user that it's acceptable;
- **gap**: not possible yet.

Then ask the user to **rank the soft requirements** from most to least important, with ties
allowed. Record the ranking in the spec: phase 5 builds the weights from it.

## Phase 3: Gaps

For each gap, give the user the choices in plain words:

1. Leave it out, and handle it by hand after solving (then re-check with `check_rota()`).
2. Approximate it (if phase 2 found a way).
3. Add it to rota-writer as a new rule. This is a code change: a `rule_entry()` in
   `rule_registry()` (`R/rules.R`) with a model part and an independent checker, and a row in
   `rule_library` (`data-raw/template_parts.R`). Any new input goes in an optional sheet or
   column. It also needs tests, including one that the solver's penalty equals the checker's, and
   a README entry. Follow `AGENTS.md`. Tell the user this is development work, roughly how big it
   is, and that existing rotas must keep working unchanged.

Log the choice. Build the first version without the gaps where you can: an early trial rota is
worth more than a complete spec.

## Phase 4: Build and check the workbook

1. **Feasibility first, before any solve.** In the spec, work out for each role and pool:
   - slots per week against the people who can fill them (doctor-days available ÷ slots, after
     typical leave: ask the user for it, or assume about 15% of working days);
   - hours needed against the cap (sum of WTE × the weekly hours limit) for resident rotas;
   - nights per eligible person per rota, against the night run limits;
   - weekend slots ÷ eligible people, against the hard weekend limit (1 in N);
   - senior doctor-days against each skill-mix rule.

   `data-raw/make_test_data.R` ("Capacity check") shows these sums in code. If any ratio is near or
   below 1, stop and discuss it with the user. It's a staffing problem, not a tuning one.
2. **Write the generator script** `local/<dept>/make_inputs.R`. It should
   `source("data-raw/template_parts.R")`, then build each sheet as a tibble:
   - rules from `rule_rows()`, with the changes in named arguments;
   - bank holidays from `england_bank_holidays()`, after checking the nation and any one-off
     holidays;
   - the workbook with `write_inputs_workbook()`, which adds the front `about` sheet (give it a
     title and a sentence or two on the rota), puts the sheets in the standard order and colours,
     and adds the header notes and drop-downs. Pass extra drop-downs only for the department's own
     values, such as placement names.

   Copy the shape of `data-raw/make_consultant_data.R` (short, optional sheets) or
   `data-raw/make_test_data.R` (resident, with grades, placements and skill mix). Staff, leave
   and preferences are read from the user's anonymised or dummy list in `local/<dept>/`, or
   generated with a fixed seed, not typed into the script. Write `local/<dept>/inputs.xlsx`.

   If the user would rather keep editing in Excel, the generator is a one-off. From then on,
   `inputs.xlsx` is the source, and the spec records that.
3. **Validate:** `validate_inputs(read_rota_inputs("local/<dept>/inputs.xlsx"))` must return zero
   rows. Fix the script, not the workbook, while the script is the source.
4. If they gave a baseline rota, score it: `check_rota()`, `summarise_doctors()`,
   `summarise_patterns()`. Its hard violations and penalties are the "no worse than now" bar. Old
   rotas often break rules the department considers normal, so discuss what you find tactfully.

## Phase 5: First weights

Turn the ranking from phase 2 into weights, using
[references/tuning.md](references/tuning.md) (how each rule is scored, and the ladders the
example workbooks use). Start from the closest example workbook's weights. Then check each
neighbouring pair in the ranking with a concrete trade-off: "one refused leave day (500) should
cost more than a bad week (a 6-shift run 150 + a split weekend 150 + a single night 100 = 400)".
Write the weights and the trade-offs behind them in the spec.

## Phase 6: Trial solve and review

- Iterate on a **short window first**: copy the inputs with `rota_end` brought in to 4–8 weeks,
  ending on a Sunday. For consultant-type rotas, also keep `rota_start` on a Monday. Use a time
  limit of 2–5 minutes and the default seed, so runs are comparable. Use the trial snippet in
  tuning.md: it writes the usual outputs and the penalty history.

- Build the **review pack** from the output (see tuning.md) and show it in plain words:
  - hard violations and unfilled slots;
  - penalty by rule;
  - fairness spread;
  - pattern totals;
  - two sample weeks of the grid, picked to show the most common problem;
  - a comparison with the baseline, if there is one.

  The user can also open `rota.xlsx`, `report.xlsx` and the app's Results tab.
- Ask what looks wrong to them, and point out what looks wrong to you, with examples (person,
  dates).

## Phase 7: Tuning loop

For each problem, **diagnose before changing anything**:

| Kind | Signs | Fix |
|---|---|---|
| Structure | Unfilled slots, the wrong people on a role, blocks that can't fit, a convention that never appears | Inputs: cover, competencies, placements, patterns, sequences. Re-check feasibility. |
| Weights | The rota is valid, but a trade-off went the way the user wouldn't choose | Change one or two weights, guided by a question about a real example |
| Solver time | The penalty in `history` was still falling at the time limit, or results vary a lot between seeds | Longer solve, or `neighbourhood_days` / `neighbourhood_doctors` in `settings` |

Then:
1. Ask the user a targeted question about the real example, and record the answer.
2. Change as little as you can: one weight, or one input fix. Re-solve with the same window, time
   limit and seed. Name the outputs `try02`, `try03`, ….
3. Report what changed: penalty by rule before and after, and whether the example got better, and
   at what cost elsewhere. A weight change moves problems around more often than it removes them.
   Say where they went.
4. Log it in the decision log.

Expect three to six rounds. Stop when the user is happy with the short window. Then do one
**full-length** solve with the time limit they'd use for real (often 30 minutes or more), and
review it the same way. Long rotas can show problems that a short window hides, such as fairness
over the year or leave clusters.

## Phase 8: Handover

- Final full-length solve into `local/<dept>/out/final/`.
- Tell the user, in the spec and in your reply:
  - which files are which;
  - **how to put in their real data without you:** in Excel, replace the dummy rows in `staff`,
    `leave`, `requests` and `preferences` (and `placements`) with real ones, keeping the columns
    and the role codes in `can_*`, and leave the structure sheets (`rules`, `shifts`, `cover`,
    `patterns`, `sequences`, `skill_mix`) alone. Then solve locally in the app or with
    `scripts/solve_rota.R`. If validation fails or slots go unfilled, the real staff group differs
    from the dummy one in a way that matters: they can fix it, or describe the difference to an
    agent in anonymised terms;
  - how to update leave, requests and staff for the next rota period (the same way);
  - how to re-check hand edits (`scripts/check_rota.R`, or the app's Check tab);
  - that the rota coordinator must still review the rota before it's issued (README disclaimer).
- Optional, if the user wants to share their template: write a fictional version as
  `data-raw/make_<dept>_data.R` → `examples/<dept>/input/inputs.xlsx`, add it to the README's "Example
  rule sets" section, and add a test that it validates. Commit only when asked.
