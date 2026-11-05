---
name: rota-share
description: Help a user contribute their rota rule set (or an interoperability tool, such as an import from rota software) to the public rota-writer repository as a pull request, with fictional example data only. Use only when the user says they want to share or contribute their rule set, template, example or tool upstream. Never offer to share real staff data, and never open a pull request without the user's explicit go-ahead.
---

# Share a rule set with rota-writer

You are helping someone give their department's rule set back to the project, so that other
departments can start from it. What gets shared is the **structure** of their rota (shifts, cover,
rules, weights, patterns) with **fictional staff**. Nothing from their real inputs goes into the
repository, and the user decides at every step.

The repository is public. Anything pushed to it can be copied, cached and indexed, and it stays in
the git history even if it is deleted later. Treat every step as publishing.

## Ground rules

- **Only when asked.** Start this skill only if the user says they want to share. At the end of
  the `rota-ruleset` skill you may mention, in one line, that sharing is possible. Don't push for
  it.
- **Never open the real data.** Don't read, print or copy the user's real inputs
  (`local/<dept>/inputs.xlsx` once they have put their real staff in, or any export from their
  rota software). Reading them sends them to your model's provider. The check in step 4 compares
  files on their computer and prints only counts.
- **Fictional data comes from a script.** The shared workbook is written by
  `data-raw/make_<name>_data.R` from a fixed seed and invented staff. It is never a copy of the
  user's workbook with names swapped. Real leave dates, LTFT days and preferences can identify
  people even without names.
- **Stage files by name.** Use `git add <path>` for each file on the list in step 5. Never
  `git add -A` or `git add .`, and never add anything under `local/`.
- **Ask before anything outward-facing**: creating a fork, pushing, and opening the pull request.
  Show the user exactly what will be sent first.

## Step 1: Agree what is shared

Explain in plain words, then ask:

1. **What:** the rule set and settings, shifts and cover, patterns and sequences, weights, a
   fictional staff group of the same shape (head count, grades, WTE mix, competencies), and a
   solved example rota made from that fictional staff group. Optionally, a short write-up of the
   department's conventions.
2. **Where:** a public GitHub repository, under the project's licences (code AGPL-3.0-or-later,
   docs CC BY-SA 4.0). They keep the credit as the author of the contribution.
3. **Permission:** is it all right with their department or rota lead to describe the rota's
   structure publicly? It is not personal data, but some teams prefer to be asked.
4. **Naming:** should the example name the hospital or department, or use a generic name
   ("District general ICU consultants")? Default to generic.
5. **GitHub account:** do they have one, and is the `gh` command logged in (`gh auth status`)? If
   not, they can still prepare everything (steps 2–5) and send it another way, or log in with
   `! gh auth login`.

Record the answers in `local/<dept>/rota-spec.md` under the decision log.

## Step 2: Write the fictional example

Pick a short name for the example, in kebab case (e.g. `dgh-icu-consultants`).

1. Write `data-raw/make_<name>_data.R`, starting from the department's generator
   (`local/<dept>/make_inputs.R`, written with fictional or anonymised staff in the
   `rota-ruleset` skill) or from `data-raw/make_consultant_data.R`. It should:
   - `source("data-raw/template_parts.R")` and build each sheet as the other examples do;
   - keep the structure sheets (settings, rules, shifts, cover, patterns, sequences, skill mix,
     placements' roles) as agreed with the user;
   - **generate** the staff group with `set.seed()`: staff codes (`C01`…), invented names in the
     style of the other examples, and the same mix of grades, WTE, competencies and LTFT days as the
     real group, but not the same people in the same order;
   - **generate** leave, requests and preferences at realistic rates from the seed, not copied
     from the real ones;
   - leave every `notes` column blank, or fill it with generic text;
   - use a rota start date in the future, and a fixed workbook timestamp, as the other examples
     do, so regenerating gives the same file;
   - write `examples/<name>/input/inputs.xlsx` with `write_inputs_workbook()`, with an `about`
     sheet that says the staff are fictional.
2. Run it: `Rscript data-raw/make_<name>_data.R`. Then
   `validate_inputs(read_rota_inputs("examples/<name>/input/inputs.xlsx"))` must return zero rows.
3. Solve the example and write its output:
   `Rscript scripts/solve_rota.R examples/<name>/input/inputs.xlsx examples/<name>/output <seconds>`,
   using the time limit from the workbook's settings. Check that it has no hard violations, or
   explain any unfilled slots in the README entry.

## Step 3: Docs and tests

- **README:** add a subsection under "Example rule sets" in the style of the existing ones: who it's
  for, the contract, the shifts and conventions, and what is notable. Add it to the contents list.
- **AGENTS.md:** add the `Rscript data-raw/make_<name>_data.R` line to the commands.
- **Test:** add `tests/testthat/test-<name>.R`: the workbook reads, validates with zero rows, and
  has the expected rota type, as `test-consultant.R` does.
- **New rules or code:** if the department needed a new rule, sheet or column, that is a code
  contribution too. Follow `AGENTS.md` (registry entry, independent checker, tests, README rule
  catalogue). Keep it in the same pull request only if it's small, otherwise suggest a separate
  one.
- Run the tests:
  `Rscript -e 'pkgload::load_all(); testthat::test_dir("tests/testthat", load_package = "none")'`.

## Step 4: Check for real data

Run the bundled check against everything to be shared, giving it the user's real inputs workbook
with `--real` (more than one if they have several). It runs on their computer and prints only
counts and where they are, so its output is safe for you to read:

```sh
Rscript .agents/skills/rota-share/scripts/check_no_real_data.R \
  --real local/<dept>/inputs.xlsx \
  data-raw/make_<name>_data.R examples/<name> tests/testthat/test-<name>.R
```

It looks for:
- names and notes from the real staff, leave, requests and preferences sheets (whole values, and
  first names or surnames anywhere);
- leave periods with the same dates as real leave;
- email addresses, phone numbers, and 7- or 10-digit numbers (GMC or NHS numbers) anywhere.

If it finds anything, don't look at the values. Tell the user which file, sheet and column, and
fix the generator script so the data comes from the seed, then go back to step 2. A common false
alarm is a fictional name that happens to match a real one: change the fictional name. Ask the user
to open the files and confirm by eye that the staff, leave and notes look invented.

If the user has no real workbook on this computer, run it without `--real`, and say that only the
patterns were checked.

Also check, without opening anything:
- `git status --short` lists nothing under `local/`, and no workbook outside `examples/<name>/`;
- `git diff --stat` touches only the files you expect.

## Step 5: Show the user, then open the pull request

1. List the files to be shared, with one line each on what they are. Typically:
   - `data-raw/make_<name>_data.R`
   - `examples/<name>/input/inputs.xlsx`
   - `examples/<name>/output/` (rota, report, CSV files, calendars)
   - `README.md`, `AGENTS.md`
   - `tests/testthat/test-<name>.R`

   Ask the user to confirm the list, and to open the example workbook and output once more.
2. Only with their go-ahead:
   - create a branch `ruleset/<name>` from an up-to-date `main`;
   - `git add` each file on the list by name, and commit with a message saying what the rule set is;
   - if they can't push to the project, fork it first: `gh repo fork --remote`;
   - push the branch, then open the pull request with `gh pr create`. The body says:
     - who the rota is for, the contract, and what's notable;
     - that the staff, leave and preferences are fictional and generated by
       `data-raw/make_<name>_data.R`;
     - that `check_no_real_data.R` found nothing;
     - anything the reviewers should know, such as new rules or unfilled slots.
3. Give the user the pull request link. Tell them reviewers may ask for changes, which you can help
   with in a later session.

## Interoperability tools

The same steps apply to a tool that converts to or from rota software (for example, filling the
`leave` sheet from an e-rostering export, or writing a rota in another system's import format):

- The code goes in `R/` with tests, following `AGENTS.md`.
- Any sample file the tests use must be **made up** in the other system's format. Never use a real
  export, which will hold real names. Generate it from a script with a fixed seed, and run the
  check in step 4 against it and the real export.
- In the pull request, say which system and version the format comes from, and how it was checked
  (for example, "imported into CLWRota's test environment").
