---
name: rota-setup
description: Install and set up rota-writer on Windows, macOS or Linux for someone who may never have used R. Use when the user asks to install, set up or get started with rota-writer, when the app or scripts won't run, or when installing packages fails.
---

# Set up rota-writer

The user is probably a rota coordinator or clinician, not a programmer. Work in small steps, say
in one plain sentence what each step does and why, and check that it worked before going on.

## Ground rules

- **Ask first** whether they want you to run the commands, or to be talked through doing it
  themselves. Some people prefer to click through installers, and some work PCs block command-line
  installs. If you can't run commands, give one step at a time and ask them to paste back what
  they see.
- **Ask before** anything that needs administrator or `sudo` rights, installs software system-wide,
  or changes PATH. Say what it will change.
- **Work PCs** (NHS trusts and other managed machines) often have no admin rights. If an installer
  is blocked, see [Troubleshooting](#troubleshooting) before trying anything clever, and suggest
  asking IT where needed. Never try to get round security controls.
- Nothing is sent anywhere: the app runs on their own computer. Say so if they ask about staff data.

## 1. Find out what's there

Work out the operating system (Windows, macOS, or which Linux) from the environment, or ask. Then
check:

```sh
Rscript --version        # R 4.3 or newer is needed
git --version            # optional: the code can be downloaded as a ZIP instead
cmake --version          # Linux only: needed to build the solver package
```

On Windows, if `Rscript` isn't found but R may be installed, look for
`C:\Program Files\R\R-*\bin\Rscript.exe`.

## 2. Install R (4.3 or newer)

**Windows**
- Command line (needs admin): `winget install --id RProject.R -e`
- Or by hand: download "R for Windows" (the "base" installer) from https://cran.r-project.org,
  run it and accept the defaults. Without admin rights the installer offers to install for the
  current user only. Choose that.
- R isn't added to PATH. Either call it by its full path
  (`"C:\Program Files\R\R-4.x.x\bin\Rscript.exe"`), or with their agreement add that `bin` folder
  to the user PATH and open a new terminal.
- Rtools isn't needed: Windows gets ready-built packages.

**macOS**
- Download the `.pkg` from https://cran.r-project.org ("Download R for macOS"). Pick **arm64** for
  Apple silicon (M1 and later) or **x86_64** for Intel, as shown in  → About This Mac. Open it
  and follow the installer.
- Or, if they use Homebrew: `brew install --cask r`.
- No compilers are needed: macOS gets ready-built packages.

**Linux**
- Debian/Ubuntu: `sudo apt install r-base r-base-dev cmake build-essential`
- Fedora: `sudo dnf install R cmake gcc-c++`
- Arch: `sudo pacman -S r cmake base-devel`
- Packages build from source on Linux, so the first install takes 10–30 minutes. If the distro's
  R is older than 4.3, follow the CRAN instructions for that distro.

Check with `Rscript --version` in a **new** terminal.

## 3. Get the code

- With git: `git clone https://github.com/mark-jeffrey/rota-writer.git`
- Without git: on the GitHub page choose **Code → Download ZIP** and unzip it somewhere easy to find,
  such as `Documents\rota-writer`. Avoid OneDrive or other synced folders if they cause trouble.

Every command from here on runs **inside that folder** (the one containing `DESCRIPTION`).

## 4. Install the R packages and check

```sh
Rscript scripts/setup.R
```

This installs only the packages that are missing (into their own R library if they lack admin
rights). Then it reads the example inputs and solves a rota for about a minute. Success ends with
"Setup done". Options:
- `--no-app`: skip the app packages (command line only).
- `--templates`: also install tidyverse, which is needed to write a new inputs template (the
  `rota-ruleset` skill).
- `--no-check`: skip the check solve.

If it fails, read the **first** error in the output, not the last, and see Troubleshooting.

## 5. Start the app

```sh
Rscript scripts/run_app.R
```

Their web browser opens the app. If it doesn't, copy the `http://127.0.0.1:…` address printed in
the terminal into the browser. Closing the terminal stops the app. Walk them through the
**Inputs → Solve → Results** tabs with the example data.

**Windows desktop shortcut (optional):** install the package once with `R CMD INSTALL .` (from the
project folder, using the full path to `R.exe` if needed). Then copy
`inst/launchers/rota-writer.bat` to the desktop. It needs `Rscript` on PATH; if it isn't, edit the
`.bat` file to use the full path.

## 6. Next

Once the example works, offer the **`rota-ruleset`** skill to set up their own department's rota.
If they'd rather just adapt the example, the README's "Quick start" explains the workbook sheets.

## Troubleshooting

| Symptom | Likely cause and fix |
|---|---|
| `'Rscript' is not recognized` (Windows) | R isn't on PATH. Use the full path to `Rscript.exe`, or add `C:\Program Files\R\R-x.y.z\bin` to the user PATH and open a new terminal. |
| `command not found: Rscript` (macOS/Linux) | R isn't installed, or the terminal was opened before installing. Open a new terminal. |
| `highs` fails to install on Linux, mentioning cmake | Install `cmake` and the compilers (step 2), then run `setup.R` again. |
| Installer blocked, or "you need administrator permission" | Use the per-user install option, if there is one. Otherwise ask IT to install R 4.3+ (it's free and widely used). `setup.R` installs packages without admin rights. |
| Packages fail to download (`cannot open URL`, timeouts) | Probably a proxy or firewall. Try again on another network, or ask IT for the proxy settings (`http_proxy` / `https_proxy` environment variables). |
| `setup.R` says to run it from the rota-writer folder | The terminal is in the wrong folder. `cd` to the folder that contains `DESCRIPTION`. |
| The check solve takes much longer than a minute | Very slow or busy machine. Fine to skip (`--no-check`), but expect long solves. |
| The app opens but the browser shows nothing | Copy the printed address into the browser by hand, or try another browser. |
| Excel "file is locked" when the app or a script writes output | Close the workbook in Excel and try again. |
