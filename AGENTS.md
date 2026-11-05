# Agent guide to rota-writer

rota-writer writes contract-compliant, fair on-call rotas for doctors with mixed-integer linear
programming in R (HiGHS solver). A department describes its rota in one Excel inputs workbook
(shifts, cover, staff, leave, rules and weights). The solver writes the rota, and an independent
checker re-checks any rota against the same workbook. `README.md` is the user documentation;
read it before changing behaviour.

Many users are rota coordinators and clinicians, not programmers. Explain what you are doing in
plain words, and ask before anything that installs software, needs admin rights or changes their
files outside this folder.

## Skills

Three step-by-step workflows live in `.agents/skills/`. Follow the matching one when a user asks for
it:

- **`rota-setup`**: install R and the packages, check the install works, and start the app, on
  Windows, macOS or Linux.
- **`rota-ruleset`**: set up a rota for a new department or staff group: interview the user, write
  a spec, build their inputs workbook, solve a trial rota and tune the penalty weights with them.
- **`rota-share`**: only when the user asks to share: turn their rule set (or an interoperability
  tool) into a fictional example, check that no real data is in it, and open a pull request.

## Commands

Run everything from the project root.

```sh
Rscript scripts/setup.R                     # install missing packages, then a 60 s check solve
Rscript scripts/run_app.R                   # the point-and-click app
Rscript scripts/solve_rota.R inputs.xlsx out/ [time_limit_s]
Rscript scripts/check_rota.R inputs.xlsx out/rota.xlsx out/report.xlsx
Rscript -e 'pkgload::load_all(); testthat::test_dir("tests/testthat", load_package = "none")'
Rscript -e 'roxygen2::roxygenise()'         # after changing roxygen comments
Rscript data-raw/make_test_data.R           # regenerate the resident inputs and the test fixture rota
Rscript data-raw/make_consultant_data.R     # regenerate the ICU consultant inputs
Rscript data-raw/make_screenshots.R         # retake the README's app screenshots (needs Chromium)
Rscript data-raw/update_bank_holidays.R     # refresh inst/extdata/uk-bank-holidays.csv from GOV.UK
```

From R: `pkgload::load_all()`, then `read_rota_inputs()`, `validate_inputs()`, `solve_rota()`,
`check_rota()`, `summarise_doctors()`, `summarise_patterns()`, `write_rota()`,
`write_check_report()`, `write_calendars()`, `write_rota_csv()`, `write_violations_csv()` and
`uk_bank_holidays()`. Each has a help page.

## Layout

| Path | What |
|---|---|
| `R/read_inputs.R` | Reading and validating the inputs workbook |
| `R/rules.R` | Rota types (`rota_types`), optional sheets and columns, and the rule registry |
| `R/model.R`, `R/soft.R`, `R/patterns.R`, `R/preferences.R`, `R/sequences.R` | The model: hard rules as constraints, soft rules as penalties |
| `R/solve.R` | `solve_rota()`: first rota, then large neighbourhood search |
| `R/check_rota.R`, `R/report.R` | The independent checker and its report |
| `R/output.R`, `R/csv.R`, `R/app*.R` | Rota workbook, calendars, CSV exports and the Shiny app |
| `R/bank_holidays.R`, `inst/extdata/uk-bank-holidays.csv` | UK bank holidays, a copy of GOV.UK's list refreshed by `data-raw/update_bank_holidays.R` |
| `data-raw/template_parts.R` | Shared pieces for writing an inputs workbook: `rule_library`, `rule_rows()`, drop-down values, and `write_inputs_workbook()` with its sheet order, colours and header notes (`sheet_guide`, `column_notes`) |
| `data-raw/make_*_data.R` | Scripts that write the example inputs workbooks |
| `docs/contract-rules.md` | The contract rules, their parameters and sources |
| `examples/<rota>/input/` | An example's inputs workbook; find it from R with `rota_example()` |
| `examples/<rota>/output/` | Its solved rota, check report, CSV files and calendars |
| `tests/testthat/fixtures/` | A naive rota with planted breaches, for the checker's tests |
| `local/` | Gitignored: a department's real inputs, spec and solve output |

## Rules for changing the code

- **Every rule is in the registry.** A rule is a `rule_entry()` in `rule_registry()`
  (`R/rules.R`) with a function that adds it to the model and one that checks a rota against it.
  A new rule also needs a row in `rule_library` (`data-raw/template_parts.R`), a row in the README
  rule catalogue and tests in `tests/testthat/`.
- **The solver's penalty for a soft rule equals the checker's** (fairness aside). Test it.
- **The checker never reuses the model's formulation.** It works on clock-time intervals, so each
  can catch the other's mistakes.
- **Existing rotas keep their behaviour.** New features go in optional sheets or columns
  (`optional_sheets`, `optional_columns` in `R/rules.R`) or new rules. A rule that is off or absent
  changes nothing. A new sheet also needs a row in `sheet_guide`, and a new column a row in
  `column_notes` (`data-raw/template_parts.R`), so the workbook places it and explains it.
- **The data scripts are deterministic** (fixed seed and workbook timestamp). Regenerating a
  workbook without changing its script must not change its contents.
- `man/` and `NAMESPACE` are generated by roxygen2. Don't edit them by hand.
- Style: tidyverse with the base pipe `|>`, short comments that say why, and plain British English
  in the docs and messages, readable by a rota coordinator.

## Privacy

Real staff names, leave and preferences are personal data. Keep a department's real inputs and
outputs in `local/` (gitignored) and never commit them, paste them into issues, or send them to an
outside service. Reading them sends them to your model's provider, so develop a department's rule
set on anonymised or dummy staff data. The user puts in the real data and solves locally, without
an agent. The example data is fictional. A template shared in the repository must use
fictional staff too.

## Git

Commit only when the user asks. Don't push without being asked.
