# Check that files about to be shared contain no real staff data.
#
#   Rscript .agents/skills/rota-share/scripts/check_no_real_data.R \
#     --real local/<dept>/inputs.xlsx [--real another.xlsx] <file or folder> ...
#
# Run from the project root. Compares every file to be shared (workbooks, CSV,
# calendars, scripts, Markdown) against the department's real inputs
# workbook(s), and looks for things that should never be shared, such as
# email addresses and phone numbers.
#
# It prints only counts and where they are (file, sheet, column), never the
# values themselves, so its output is safe to show to an AI agent. Exits with
# status 1 if anything is found.

suppressPackageStartupMessages(library(readxl))

args <- commandArgs(trailingOnly = TRUE)
flag <- which(args == "--real")
real_paths <- args[flag + 1]
shared_paths <- if (length(flag) > 0) args[-c(flag, flag + 1)] else args
if (length(shared_paths) == 0) {
  stop("Usage: check_no_real_data.R [--real real_inputs.xlsx] <file or folder> ...", call. = FALSE)
}
shared_files <- unlist(lapply(shared_paths, \(p) {
  if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE) else p
}))
missing <- shared_files[!file.exists(shared_files)]
if (length(missing) > 0) stop("Not found: ", paste(missing, collapse = ", "), call. = FALSE)

read_sheets <- function(path) {
  sheets <- excel_sheets(path)
  setNames(lapply(sheets, \(s) suppressMessages(read_excel(path, sheet = s, col_types = "text"))), sheets)
}

# Cells of every file as (where, column, value), all as text
file_cells <- function(path) {
  if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    do.call(rbind, lapply(names(sheets <- read_sheets(path)), \(s) {
      df <- sheets[[s]]
      if (nrow(df) == 0 || ncol(df) == 0) return(NULL)
      data.frame(where = paste0(path, " [", s, "]"), column = rep(names(df), each = nrow(df)),
                 value = unlist(df, use.names = FALSE))
    }))
  } else {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    if (length(lines) == 0) return(NULL)
    data.frame(where = path, column = "(text)", value = lines)
  }
}

cells <- do.call(rbind, lapply(shared_files, file_cells))
cells <- cells[!is.na(cells$value) & nzchar(trimws(cells$value)), ]
found <- 0
report <- function(n, what) {
  if (n == 0) return(invisible())
  found <<- found + n
  hits <- aggregate(list(n = rep(1, nrow(what))), what[c("where", "column")], sum)
  for (i in seq_len(nrow(hits))) {
    cat(sprintf("FOUND  %3d  %s  column %s\n", hits$n[i], hits$where[i], hits$column[i]))
  }
}

# Things no shared file should hold, real or not
patterns <- c(
  "an email address" = "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}",
  "a UK phone number" = "(\\+44|\\b0)[ -]?[0-9]{2,4}[ -]?[0-9]{3,4}[ -]?[0-9]{3,4}\\b",
  "a 7-digit number (GMC number?)" = "(^|[^0-9.])[0-9]{7}($|[^0-9.])",
  "a 10-digit number (NHS number?)" = "\\b[0-9]{3}[ -]?[0-9]{3}[ -]?[0-9]{4}\\b"
)
for (what in names(patterns)) {
  hit <- grepl(patterns[[what]], cells$value, perl = TRUE)
  if (any(hit)) cat(sprintf("\n%d cells look like %s:\n", sum(hit), what))
  report(sum(hit), cells[hit, ])
}

# Values from the real workbook
personal <- c("staff", "leave", "requests", "preferences", "placements")
for (real in real_paths) {
  if (!file.exists(real)) stop("Not found: ", real, call. = FALSE)
  sheets <- read_sheets(real)

  # Columns about people: names, contact details, notes and comments
  values_in <- function(pattern) {
    v <- unlist(lapply(intersect(personal, names(sheets)), \(s) {
      df <- sheets[[s]]
      unlist(df[grep(pattern, names(df), ignore.case = TRUE, value = TRUE)], use.names = FALSE)
    }))
    v <- unique(trimws(v[!is.na(v)]))
    v[nchar(v) >= 4]
  }
  names_etc <- values_in("name|email|phone")
  free_text <- values_in("note|comment")
  # A name or note appearing whole, or a word of a name (a first name or
  # surname) anywhere. Words of notes are ordinary vocabulary, so only whole
  # notes count.
  words <- unique(tolower(unlist(strsplit(names_etc, "[^[:alpha:]'-]+"))))
  words <- setdiff(words[nchar(words) >= 3], c("dr", "mr", "mrs", "ms", "miss", "prof"))
  whole <- cells$value %in% c(names_etc, free_text)
  in_text <- vapply(strsplit(tolower(cells$value), "[^[:alpha:]_'-]+"), \(w) any(w %in% words),
                    logical(1))
  hit <- whole | in_text
  if (any(hit)) cat(sprintf("\n%d cells match names or notes in %s:\n", sum(hit), real))
  report(sum(hit), cells[hit, ])

  # Leave: the same start and end dates as real leave is a fingerprint, even
  # with the names changed
  if ("leave" %in% names(sheets) && nrow(sheets$leave) > 0) {
    key <- \(df) paste(df$start_date, df$end_date)
    real_leave <- unique(key(sheets$leave))
    for (f in shared_files[grepl("\\.xlsx$", shared_files, ignore.case = TRUE)]) {
      shared <- read_sheets(f)
      if (!"leave" %in% names(shared) || nrow(shared$leave) == 0) next
      same <- key(shared$leave) %in% real_leave
      if (mean(same) > 0.2 && sum(same) >= 3) {
        cat(sprintf("\n%d of %d leave periods in %s have the same dates as real leave\n",
                    sum(same), length(same), f))
        found <- found + sum(same)
      }
    }
  }
}

if (length(real_paths) == 0) {
  cat("\nNo --real workbook given, so only the patterns above were checked.\n")
}
if (found > 0) {
  cat(sprintf("\n%d possible pieces of real data found. Values are not shown: open the files",
              found), "yourself to look, and regenerate them from fictional data.\n")
  quit(status = 1)
}
cat(sprintf("No real data found in %d files.\n", length(shared_files)))
