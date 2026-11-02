# Writing rotas for people: the coordinator's workbook and per-doctor
# calendars.

leave_codes <- c(annual = "AL", study = "SL", exam = "EX", parental = "PL")

# "D01 Ada Alder": short, unique labels for columns and calendars
doctor_labels <- function(staff) {
  set_names(paste(staff$doctor_id, sub("^Dr\\.? ", "", staff$name)), staff$doctor_id)
}

# "ICU day", "ICU1 long day", "OBS night", unless the shifts sheet gives a
# label
shift_label <- function(role, shift_type, label = NA_character_) {
  coalesce(label, if_else(shift_type == "normal_day",
                          paste(sub("_DAY$", "", role), "day"),
                          paste(role, sub("_", " ", shift_type))))
}

# The rota plus approved leave, within the rota period
rota_with_leave <- function(rota, inputs) {
  leave <- leave_days(inputs) |>
    filter(status == "approved") |>
    transmute(doctor_id, date, code = unname(leave_codes[leave_type])) |>
    distinct(doctor_id, date, .keep_all = TRUE)

  bind_rows(rota, anti_join(leave, rota, by = c("doctor_id", "date"))) |>
    filter(date >= inputs$period$start, date <= inputs$period$end) |>
    arrange(date, doctor_id)
}

#' Write a rota workbook
#'
#' Saves a rota as an Excel workbook for the rota coordinator. It can be read
#' back with [read_rota()] and checked with [check_rota()], including after
#' editing the `long` sheet by hand. Sheets:
#' * `grid`: one row per date and one column per doctor, holding shift and
#'   leave codes. Weekends and bank holidays are shaded, nights and long days
#'   coloured.
#' * `by_shift`: one row per date and one column per shift, holding who is
#'   on it. Slots short of their minimum cover say `UNFILLED`.
#' * `doctors`: each doctor's hours, on-calls, nights and weekends against
#'   their fair share (see [summarise_doctors()]).
#' * `key`: what the shift and leave codes mean.
#' * `long`: one row per doctor per day (`doctor_id`, `date`, `code`), the
#'   format the checker reads.
#'
#' Approved leave is shown as `AL` (annual), `SL` (study), `EX` (exam) or
#' `PL` (parental) on days the doctor has no shift.
#'
#' @param rota A rota in long format, e.g. `solve_rota(inputs)$rota`.
#' @inheritParams check_rota
#' @param path Path of the `.xlsx` file to write. An existing file is
#'   overwritten.
#'
#' @returns `path`, invisibly.
#' @seealso [write_calendars()] for per-doctor calendars,
#'   [write_check_report()] for the compliance report.
#' @export
write_rota <- function(rota, inputs, path) {
  inputs <- as_rota_inputs(inputs)
  rota <- as_rota(rota)
  long <- rota_with_leave(rota, inputs)
  days <- rota_days(inputs)
  grid <- rota_grid(rota, inputs)

  wb <- openxlsx2::wb_workbook(creator = "rota-writer") |>
    add_rota_styles() |>
    add_table_sheet("grid", grid) |>
    openxlsx2::wb_freeze_pane("grid", first_active_row = 2, first_active_col = 4) |>
    shade_rows("grid", grid, cols = 1:3) |>
    colour_codes("grid", grid, cols = 4:max(4, ncol(grid)), inputs$shifts)

  by_shift <- shift_table(rota, inputs, days)
  wb <- wb |>
    add_table_sheet("by_shift", by_shift) |>
    openxlsx2::wb_freeze_pane("by_shift", first_active_row = 2, first_active_col = 4) |>
    shade_rows("by_shift", by_shift, cols = 1:3) |>
    openxlsx2::wb_add_conditional_formatting(
      "by_shift", dims = data_dims(by_shift, 4:max(4, ncol(by_shift))),
      type = "containsText", rule = "UNFILLED", style = "unfilled"
    )

  doctors <- summarise_doctors(rota, inputs) |>
    left_join(select(inputs$staff, doctor_id, name), by = "doctor_id") |>
    relocate(name, .after = doctor_id) |>
    left_join(placement_summary(inputs), by = "doctor_id") |>
    relocate(placement, .after = wte)

  key <- bind_rows(
    inputs$shifts |>
      transmute(code = shift_code, meaning = shift_label(role, shift_type, label),
                start, end, hours),
    tibble::tibble(code = unname(leave_codes),
                   meaning = paste(stringr::str_to_title(names(leave_codes)), "leave (approved)"))
  )

  wb <- wb |>
    add_table_sheet("doctors", doctors) |>
    add_table_sheet("key", key) |>
    add_table_sheet("long", long)

  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}

# One row per rota date: day name and bank holiday note
rota_days <- function(inputs) {
  tibble::tibble(date = rota_dates(inputs)) |>
    mutate(
      day = as.character(lubridate::wday(date, label = TRUE, locale = "C")),
      holiday = if_else(date %in% inputs$bank_holidays$date, "Bank holiday", NA_character_)
    )
}

# Dates down, doctors across (labelled "D01 Ada Alder"), holding shift and
# approved leave codes
rota_grid <- function(rota, inputs) {
  labels <- doctor_labels(inputs$staff)
  grid <- rota_with_leave(rota, inputs) |>
    pivot_wider(names_from = doctor_id, values_from = code) |>
    right_join(rota_days(inputs), by = "date") |>
    arrange(date) |>
    select(date, day, holiday, any_of(inputs$staff$doctor_id))
  names(grid) <- coalesce(labels[names(grid)], names(grid))
  grid
}

# Who is on each shift each day, with UNFILLED where cover is short
shift_table <- function(rota, inputs, days) {
  labels <- doctor_labels(inputs$staff)
  codes <- unique(shift_covers(inputs$shifts)$slot)

  expected_cover(inputs) |>
    filter(n_max > 0) |>
    left_join(transmute(rota_slots(rota, inputs), date, shift_code = slot, who = labels[doctor_id]),
              by = c("date", "shift_code")) |>
    summarise(
      short = sum(!is.na(who)) < first(n_min),
      who = paste(sort(who[!is.na(who)]), collapse = ", "),
      .by = c(date, shift_code)
    ) |>
    mutate(
      who = case_when(short & who == "" ~ "UNFILLED",
                      short ~ paste0(who, ", UNFILLED"),
                      .default = who),
      shift_code = factor(shift_code, levels = codes)
    ) |>
    arrange(shift_code) |>
    select(date, shift_code, who) |>
    pivot_wider(names_from = shift_code, values_from = who) |>
    right_join(days, by = "date") |>
    arrange(date) |>
    select(date, day, holiday, any_of(codes))
}

placement_summary <- function(inputs) {
  inputs$placements |>
    arrange(start_date) |>
    summarise(placement = paste(unique(placement), collapse = " then "), .by = doctor_id)
}

# Cell colours for the workbook and the app: fill, and text where not black
rota_colours <- tibble::tribble(
  ~style,     ~fill,     ~text,
  "night",    "#3F3F7F", "#FFFFFF",
  "long_day", "#FFE08A", NA,
  "leave",    "#B7E1CD", NA,
  "weekend",  "#E7E6E6", NA,
  "holiday",  "#F8CBAD", NA,
  "unfilled", "#F4C7C3", "#9C0006"
)

# Style of each rota code, matching colour_codes(): nights, long days and
# leave, otherwise NA. Codes are recognised by "_N" and "_LD", or else by
# their shift_type in `shifts`.
code_style <- function(code, shifts = NULL) {
  by_type <- if (!is.null(shifts)) shifts$shift_type[match(code, shifts$shift_code)] else NA
  case_when(
    stringr::str_detect(code, "_N") ~ "night",
    stringr::str_detect(code, "_LD") ~ "long_day",
    code %in% leave_codes ~ "leave",
    by_type %in% c("night", "long_day") ~ by_type,
    .default = NA_character_
  )
}

add_rota_styles <- function(wb) {
  argb <- \(hex) openxlsx2::wb_color(paste0("FF", sub("#", "", hex)))
  for (i in seq_len(nrow(rota_colours))) {
    style <- rota_colours[i, ]
    wb <- openxlsx2::wb_add_dxfs_style(
      wb, name = style$style, bg_fill = argb(style$fill),
      font_color = if (!is.na(style$text)) argb(style$text)
    )
  }
  wb
}

add_table_sheet <- function(wb, name, x) {
  wb |>
    openxlsx2::wb_add_worksheet(name) |>
    openxlsx2::wb_add_data_table(name, x, table_style = "TableStyleLight1", na.strings = "") |>
    openxlsx2::wb_freeze_pane(name, first_row = TRUE) |>
    openxlsx2::wb_set_col_widths(name, cols = seq_along(x), widths = "auto")
}

data_dims <- function(x, cols) {
  openxlsx2::wb_dims(rows = seq_len(max(nrow(x), 1)) + 1, cols = cols)
}

# Shade the date columns of weekend and bank holiday rows. Column B is the
# day name and column C the bank holiday note.
shade_rows <- function(wb, sheet, x, cols) {
  dims <- data_dims(x, cols)
  wb |>
    openxlsx2::wb_add_conditional_formatting(sheet, dims = dims, rule = '$C2<>""', style = "holiday") |>
    openxlsx2::wb_add_conditional_formatting(sheet, dims = dims, rule = 'OR($B2="Sat",$B2="Sun")',
                                             style = "weekend")
}

colour_codes <- function(wb, sheet, x, cols, shifts = NULL) {
  dims <- data_dims(x, cols)
  rules <- c("_N" = "night", "_LD" = "long_day", set_names(rep("leave", length(leave_codes)), leave_codes))
  if (!is.null(shifts)) {
    # Codes without "_N" or "_LD", styled by their shift type
    other <- shifts$shift_code[!stringr::str_detect(shifts$shift_code, "_N|_LD")]
    styles <- code_style(other, shifts)
    rules <- c(rules, set_names(styles, other)[!is.na(styles)])
  }
  for (text in names(rules)) {
    wb <- openxlsx2::wb_add_conditional_formatting(wb, sheet, dims = dims, type = "containsText",
                                                   rule = text, style = rules[[text]])
  }
  wb
}

#' Write per-doctor calendars
#'
#' Writes one iCalendar (`.ics`) file per doctor with their shifts and
#' approved leave, which they can import into Outlook, Google Calendar or
#' Apple Calendar. Shift times are local wall-clock times (no time zone), so
#' they show as rostered whatever the calendar's time zone.
#'
#' @inheritParams write_rota
#' @param dir Folder to write the files into, created if needed. Files are
#'   named after the doctor, e.g. `D01_Ada_Alder.ics`, and overwritten if
#'   they exist.
#' @param doctors Doctor IDs to write calendars for. Defaults to every doctor
#'   in the staff list with at least one shift or day of leave.
#'
#' @returns The paths of the files written, invisibly.
#' @export
#' @examples
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' rota <- read_rota(system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"))
#' write_calendars(rota, inputs, tempfile("calendars"), doctors = "D01")
write_calendars <- function(rota, inputs, dir, doctors = NULL) {
  inputs <- as_rota_inputs(inputs)
  rota <- as_rota(rota)
  labels <- doctor_labels(inputs$staff)
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")

  shifts <- shift_instances(rota, inputs) |>
    transmute(
      doctor_id,
      uid = paste(doctor_id, date, code, sep = "-"),
      start = format(start_dt, "%Y%m%dT%H%M%S", tz = "UTC"),
      end = format(end_dt, "%Y%m%dT%H%M%S", tz = "UTC"),
      summary = shift_label(role, shift_type, label),
      description = paste0(code, ", ", start_dt |> format("%H:%M", tz = "UTC"), "-",
                           end_dt |> format("%H:%M", tz = "UTC"), ", ", hours, " h"),
      all_day = FALSE
    )
  leave <- inputs$leave |>
    filter(status == "approved", end_date >= inputs$period$start, start_date <= inputs$period$end) |>
    transmute(
      doctor_id,
      uid = paste(doctor_id, start_date, leave_type, "leave", sep = "-"),
      start = format(start_date, "%Y%m%d"),
      end = format(end_date + 1, "%Y%m%d"),
      summary = paste(stringr::str_to_title(leave_type), "leave"),
      description = coalesce(notes, ""),
      all_day = TRUE
    )
  events <- bind_rows(shifts, leave)

  doctors <- doctors %||% intersect(inputs$staff$doctor_id, events$doctor_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- map_chr(doctors, \(id) {
    path <- file.path(dir, paste0(gsub("[^A-Za-z0-9]+", "_", labels[[id]]), ".ics"))
    mine <- arrange(filter(events, doctor_id == id), start)
    lines <- c(
      "BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//rota-writer//EN", "CALSCALE:GREGORIAN",
      paste0("X-WR-CALNAME:", ics_text(paste("Rota", labels[[id]]))),
      unlist(pmap(mine, \(uid, start, end, summary, description, all_day, ...) c(
        "BEGIN:VEVENT",
        paste0("UID:", uid, "@rota-writer"),
        paste0("DTSTAMP:", stamp),
        if (all_day) paste0("DTSTART;VALUE=DATE:", start) else paste0("DTSTART:", start),
        if (all_day) paste0("DTEND;VALUE=DATE:", end) else paste0("DTEND:", end),
        paste0("SUMMARY:", ics_text(summary)),
        if (nzchar(description)) paste0("DESCRIPTION:", ics_text(description)),
        "TRANSP:OPAQUE",
        "END:VEVENT"
      ))),
      "END:VCALENDAR"
    )
    writeBin(charToRaw(paste0(paste(ics_fold(lines), collapse = "\r\n"), "\r\n")), path)
    path
  })
  invisible(paths)
}

# Escape iCalendar TEXT values (RFC 5545 3.3.11)
ics_text <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("([;,])", "\\\\\\1", x)
  gsub("\r?\n", "\\\\n", x)
}

# Fold lines longer than 75 octets onto continuation lines starting with a
# space (RFC 5545 3.1)
ics_fold <- function(lines) {
  map_chr(lines, \(line) {
    if (nchar(line, type = "bytes") <= 75) {
      return(line)
    }
    chars <- strsplit(line, "")[[1]]
    bytes <- nchar(chars, type = "bytes")
    pieces <- character()
    while (length(chars) > 0) {
      limit <- if (length(pieces) == 0) 75 else 74
      n <- max(which(cumsum(bytes) <= limit))
      pieces <- c(pieces, paste(chars[seq_len(n)], collapse = ""))
      chars <- chars[-seq_len(n)]
      bytes <- bytes[-seq_len(n)]
    }
    paste(pieces, collapse = "\r\n ")
  })
}
