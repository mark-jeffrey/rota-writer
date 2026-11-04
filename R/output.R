# Writing rotas for people: the coordinator's workbook and per-doctor
# calendars.

leave_codes <- c(annual = "AL", study = "SL", exam = "EX", parental = "PL")

# "D01 Ada Alder": short, unique labels for columns and calendars
doctor_labels <- function(staff) {
  set_names(paste(staff$doctor_id, sub("^Dr\\.? ", "", staff$name)), staff$doctor_id)
}

# "Ada Alder", or "D01 Ada Alder" where two doctors share a name: for lists
# of who is on, where space is short
doctor_names <- function(staff) {
  short <- sub("^Dr\\.? ", "", staff$name)
  shared <- short %in% short[duplicated(short)]
  set_names(if_else(shared, paste(staff$doctor_id, short), short), staff$doctor_id)
}

# Doctors by grade, most senior first, then in staff sheet order
staff_order <- function(inputs) {
  rank <- inputs$grades$rank[match(inputs$staff$grade, inputs$grades$grade)]
  inputs$staff$doctor_id[order(-coalesce(as.numeric(rank), -Inf))]
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
#' * `by_role`: one row per date and one column per shift, holding who is on
#'   it. On-call shifts come first, then normal days, each grouped by area
#'   (the shifts sheet's `area` column, or else the shift's role) under a
#'   band naming the area. Slots short of their minimum cover say
#'   `UNFILLED`. Doctors are named without their ID unless two share a name.
#' * `by_doctor`: one row per date and one column per doctor, holding shift
#'   and leave codes. Doctors are ordered by grade, most senior first, under
#'   a band of grades. Below the rota, each doctor's shifts, hours, nights,
#'   weekend days and leave days are formulas, so they follow hand edits.
#' * `doctors`: each doctor's hours, on-calls, nights and weekends against
#'   their fair share (see [summarise_doctors()]).
#' * `key`: what the shift and leave codes mean, in their colours.
#' * `long`: one row per doctor per day (`doctor_id`, `date`, `code`), the
#'   format the checker reads.
#'
#' Each area has its own colour, pale for normal days, mid for long days and
#' dark for nights, and both rota sheets use the same colours. They are
#' conditional formats, so a code typed into `by_doctor` takes its colour.
#' Weekends and bank holidays are shaded, and a line under each Sunday
#' marks the weeks. Both rota sheets print landscape,
#' one page wide, with their headings on every page.
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
  withr::local_options(openxlsx2.dateFormat = "dd/mm/yyyy")
  long <- rota_with_leave(rota, inputs)
  colours <- code_colours(inputs$shifts)

  shifts <- inputs$shifts |>
    mutate(area = as.character(shift_areas(inputs$shifts))) |>
    arrange(match(shift_code, slot_order(inputs$shifts)))
  key <- bind_rows(
    shifts |>
      transmute(code = shift_code, area, meaning = shift_label(role, shift_type, label),
                type = shift_type, start, end, hours),
    tibble::tibble(code = unname(leave_codes),
                   meaning = paste(stringr::str_to_title(names(leave_codes)), "leave (approved)"))
  )

  wb <- openxlsx2::wb_workbook(creator = "rota-writer") |>
    add_rota_styles(colours) |>
    add_role_sheet(shift_table(rota, inputs, rota_days(inputs)), inputs$shifts, colours) |>
    add_doctor_sheet(rota_grid(rota, inputs), inputs, colours, key)

  doctors <- summarise_doctors(rota, inputs) |>
    left_join(select(inputs$staff, doctor_id, name), by = "doctor_id") |>
    relocate(name, .after = doctor_id) |>
    left_join(placement_summary(inputs), by = "doctor_id") |>
    relocate(placement, .after = wte)

  wb <- wb |>
    add_table_sheet("doctors", doctors) |>
    add_table_sheet("key", key) |>
    fill_codes("key", key$code, colours) |>
    add_table_sheet("long", long)

  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}

# The by_role sheet: a band of areas over the table, which starts on row 2
add_role_sheet <- function(wb, x, shifts, colours) {
  slots <- setdiff(names(x), c("date", "day", "holiday"))
  groups <- slot_groups(slots, shifts)
  first <- 4
  wb <- wb |>
    openxlsx2::wb_add_worksheet("by_role") |>
    openxlsx2::wb_add_data_table("by_role", x, dims = "A2", table_style = "TableStyleLight1",
                                 na.strings = "") |>
    openxlsx2::wb_set_col_widths("by_role", cols = seq_along(x), widths = "auto") |>
    openxlsx2::wb_freeze_pane("by_role", first_active_row = 3, first_active_col = first) |>
    add_bands("by_role", groups$label, groups$colour, first_col = first)
  wb <- openxlsx2::wb_add_cell_style(wb, "by_role", dims = data_dims(x, seq_along(x), header_rows = 2),
                                     vertical = "top")
  # Normal days list many doctors: cap their width and wrap
  wide <- which(map_int(x[slots], \(v) max(c(0, nchar(v)), na.rm = TRUE)) > 40)
  for (j in first - 1 + wide) {
    wb <- wb |>
      openxlsx2::wb_set_col_widths("by_role", cols = j, widths = 45) |>
      openxlsx2::wb_add_cell_style("by_role", dims = data_dims(x, j, header_rows = 2),
                                   wrap_text = TRUE, vertical = "top")
  }
  wb <- fill_codes(wb, "by_role", slots, colours, row = 2, first_col = first)
  data <- data_dims(x, first - 1 + seq_along(slots), header_rows = 2)
  wb <- openxlsx2::wb_add_conditional_formatting(wb, "by_role", dims = data, type = "containsText",
                                                 rule = "UNFILLED", style = "unfilled")
  for (j in seq_along(slots)) {
    if (!slots[j] %in% colours$code) next
    col <- openxlsx2::int2col(first - 1 + j)
    wb <- openxlsx2::wb_add_conditional_formatting(
      wb, "by_role", dims = data_dims(x, first - 1 + j, header_rows = 2),
      rule = sprintf('LEN(%s3)>0', col), style = code_style_name(slots[j])
    )
  }
  wb |>
    shade_rows("by_role", x, cols = 1:3, header_rows = 2) |>
    mark_weeks("by_role", x, header_rows = 2) |>
    print_setup("by_role", title_rows = 1:2)
}

# The by_doctor sheet: doctors by grade, under a band of grades if they have
# them, with each doctor's totals below
add_doctor_sheet <- function(wb, grid, inputs, colours, key) {
  ids <- staff_order(inputs)
  ids <- ids[doctor_labels(inputs$staff)[ids] %in% names(grid)]
  grades <- inputs$staff$grade[match(ids, inputs$staff$doctor_id)]
  banded <- any(!is.na(grades))
  header_rows <- if (banded) 2 else 1
  first <- 4

  wb <- wb |>
    openxlsx2::wb_add_worksheet("by_doctor") |>
    openxlsx2::wb_add_data_table("by_doctor", grid, dims = openxlsx2::wb_dims(rows = header_rows, cols = 1),
                                 table_style = "TableStyleLight1", na.strings = "") |>
    openxlsx2::wb_set_col_widths("by_doctor", cols = seq_along(grid), widths = "auto") |>
    openxlsx2::wb_freeze_pane("by_doctor", first_active_row = header_rows + 1, first_active_col = first)
  if (banded) {
    grades <- coalesce(grades, "No grade")
    runs <- cumsum(c(TRUE, grades[-1] != grades[-length(grades)]))
    wb <- add_bands(wb, "by_doctor", grades, c("#595959", "#8C8C8C")[(runs - 1) %% 2 + 1],
                    first_col = first)
  }
  wb |>
    shade_rows("by_doctor", grid, cols = 1:3, header_rows = header_rows) |>
    colour_codes("by_doctor", grid, cols = first:max(first, ncol(grid)), colours,
                 header_rows = header_rows) |>
    mark_weeks("by_doctor", grid, header_rows = header_rows) |>
    add_totals("by_doctor", grid, cols = first - 1 + seq_along(ids), header_rows, key) |>
    print_setup("by_doctor", title_rows = seq_len(header_rows))
}

# Bands over a table's columns in row 1: one merged cell per run of equal
# labels
add_bands <- function(wb, sheet, labels, colours, first_col) {
  runs <- cumsum(c(TRUE, labels[-1] != labels[-length(labels)]))
  for (g in split(seq_along(labels), runs)) {
    dims <- openxlsx2::wb_dims(rows = 1, cols = first_col - 1 + (min(g):max(g)))
    wb <- wb |>
      openxlsx2::wb_add_data(sheet, labels[g[1]], dims = dims) |>
      openxlsx2::wb_merge_cells(sheet, dims = dims) |>
      openxlsx2::wb_add_fill(sheet, dims = dims, color = argb(colours[g[1]])) |>
      openxlsx2::wb_add_font(sheet, dims = dims, bold = TRUE, color = argb("#FFFFFF")) |>
      openxlsx2::wb_add_cell_style(sheet, dims = dims, horizontal = "center")
  }
  wb
}

# A line under each Sunday, so the weeks stand out
mark_weeks <- function(wb, sheet, x, header_rows) {
  for (row in header_rows + which(x$day == "Sun")) {
    wb <- openxlsx2::wb_add_border(wb, sheet, dims = openxlsx2::wb_dims(rows = row, cols = seq_along(x)),
                                   bottom_border = "medium", top_border = NULL, left_border = NULL,
                                   right_border = NULL)
  }
  wb
}

# Each doctor's shifts, hours, nights, weekend days and leave days under the
# rota, as formulas that look codes up in the key sheet, so they follow hand
# edits
add_totals <- function(wb, sheet, x, cols, header_rows, key) {
  n_shifts <- sum(!is.na(key$type))
  key_col <- \(name) openxlsx2::int2col(match(name, names(key)))
  key_range <- \(name, from, to) sprintf("key!$%1$s$%2$d:$%1$s$%3$d", key_col(name), from, to)
  codes <- key_range("code", 2, n_shifts + 1)
  leave <- key_range("code", n_shifts + 2, nrow(key) + 1)
  hours <- key_range("hours", 2, n_shifts + 1)
  types <- key_range("type", 2, n_shifts + 1)
  rows <- header_rows + c(1, max(nrow(x), 1))
  days <- sprintf("$B$%d:$B$%d", rows[1], rows[2])
  totals <- function(range) {
    c(
      "Shifts"       = sprintf("SUMPRODUCT(COUNTIF(%s,%s))", codes, range),
      "Hours"        = sprintf("SUMPRODUCT(SUMIF(%s,%s,%s))", codes, range, hours),
      "Nights"       = sprintf("SUMPRODUCT(COUNTIFS(%s,%s,%s,\"night\"))", codes, range, types),
      "Weekend days" = sprintf("SUMPRODUCT(((%1$s=\"Sat\")+(%1$s=\"Sun\"))*COUNTIF(%2$s,%3$s))",
                               days, codes, range),
      "Leave days"   = sprintf("SUMPRODUCT(COUNTIF(%s,%s))", leave, range)
    )
  }
  labels <- names(totals("A1:A1"))
  top <- rows[2] + 2
  wb <- wb |>
    openxlsx2::wb_add_data(sheet, labels, dims = openxlsx2::wb_dims(rows = top, cols = 1),
                           col_names = FALSE) |>
    openxlsx2::wb_add_font(sheet, dims = openxlsx2::wb_dims(rows = top - 1 + seq_along(labels), cols = 1),
                           bold = TRUE)
  for (j in cols) {
    col <- openxlsx2::int2col(j)
    range <- sprintf("%1$s%2$d:%1$s%3$d", col, rows[1], rows[2])
    formulas <- totals(range)
    for (i in seq_along(formulas)) {
      wb <- openxlsx2::wb_add_formula(wb, sheet, formulas[[i]], dims = paste0(col, top - 1 + i))
    }
  }
  wb
}

# One row per rota date: day name and bank holiday note
rota_days <- function(inputs) {
  tibble::tibble(date = rota_dates(inputs)) |>
    mutate(
      day = as.character(lubridate::wday(date, label = TRUE, locale = "C")),
      holiday = if_else(date %in% inputs$bank_holidays$date, "Bank holiday", NA_character_)
    )
}

# Dates down, doctors across (labelled "D01 Ada Alder", most senior grade
# first), holding shift and approved leave codes
rota_grid <- function(rota, inputs) {
  labels <- doctor_labels(inputs$staff)
  grid <- rota_with_leave(rota, inputs) |>
    pivot_wider(names_from = doctor_id, values_from = code) |>
    right_join(rota_days(inputs), by = "date") |>
    arrange(date) |>
    select(date, day, holiday, any_of(staff_order(inputs)))
  names(grid) <- coalesce(labels[names(grid)], names(grid))
  grid
}

# Who is on each shift each day, with UNFILLED where cover is short
shift_table <- function(rota, inputs, days) {
  labels <- doctor_names(inputs$staff)
  codes <- slot_order(inputs$shifts)

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

# Colours ----------------------------------------------------------------

# Cell colours for the workbook and the app that are not shift colours:
# fill, and text where not black
rota_colours <- tibble::tribble(
  ~style,     ~fill,     ~text,
  "leave",    "#B7E1CD", NA,
  "weekend",  "#E7E6E6", NA,
  "holiday",  "#F8CBAD", NA,
  "unfilled", "#F4C7C3", "#9C0006"
)

# Hues for areas, in the order they first appear in the shifts sheet: blue,
# orange, magenta, teal, gold, purple, red, steel. Green is left for leave.
area_hues <- c(250, 45, 340, 190, 80, 290, 10, 220)

# Normal days pale, long days mid, nights dark
type_tones <- tibble::tribble(
  ~shift_type,  ~luminance, ~chroma, ~text,
  "normal_day", 93,         18,      NA,
  "long_day",   78,         45,      NA,
  "night",      38,         45,      "#FFFFFF"
)

# Each shift's area: the shifts sheet's area column, or else its role
shift_areas <- function(shifts) {
  area <- coalesce(shifts[["area"]] %||% NA_character_, shifts$role)
  factor(area, levels = unique(area))
}

area_hue <- function(area) {
  area_hues[(as.integer(area) - 1) %% length(area_hues) + 1]
}

# Fill and text colour of every shift and leave code: hue by area, tone by
# shift type
code_colours <- function(shifts) {
  area <- shift_areas(shifts)
  tone <- type_tones[coalesce(match(shifts$shift_type, type_tones$shift_type), 1L), ]
  bind_rows(
    tibble::tibble(code = shifts$shift_code,
                   fill = grDevices::hcl(area_hue(area), tone$chroma, tone$luminance),
                   area = as.character(area), text = tone$text),
    tibble::tibble(code = unname(leave_codes), fill = rota_colours$fill[rota_colours$style == "leave"],
                   area = NA_character_, text = NA_character_)
  )
}

# Cover slots (by_role columns): on-call first, then normal days, each
# grouped by area in the shifts sheet's order
slot_order <- function(shifts) {
  slots <- unique(shift_covers(shifts)$slot)
  i <- match(slots, shifts$shift_code)
  area <- as.integer(shift_areas(shifts))[i]
  slots[order(!coalesce(as.logical(shifts$is_on_call[i]), FALSE), area, i)]
}

# The band label and colour over each slot: its area and on-call group
slot_groups <- function(slots, shifts) {
  i <- match(slots, shifts$shift_code)
  area <- shift_areas(shifts)[i]
  on_call <- coalesce(as.logical(shifts$is_on_call[i]), FALSE)
  label <- if_else(on_call, as.character(area), paste(area, "normal days"))
  label <- coalesce(label, "Other")
  tibble::tibble(
    label = label,
    colour = if_else(is.na(area), "#7F7F7F", grDevices::hcl(area_hue(area), 50, 45))
  )
}

argb <- function(hex) openxlsx2::wb_color(paste0("FF", sub("#", "", hex)))

code_style_name <- function(code) paste0("code_", code)

add_rota_styles <- function(wb, colours) {
  styles <- bind_rows(
    rota_colours |> filter(style != "leave"),
    transmute(colours, style = code_style_name(code), fill, text)
  )
  for (i in seq_len(nrow(styles))) {
    wb <- openxlsx2::wb_add_dxfs_style(
      wb, name = styles$style[i], bg_fill = argb(styles$fill[i]),
      font_color = if (!is.na(styles$text[i])) argb(styles$text[i])
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

data_dims <- function(x, cols, header_rows = 1) {
  openxlsx2::wb_dims(rows = seq_len(max(nrow(x), 1)) + header_rows, cols = cols)
}

# Shade the date columns of weekend and bank holiday rows. Column B is the
# day name and column C the bank holiday note.
shade_rows <- function(wb, sheet, x, cols, header_rows = 1) {
  dims <- data_dims(x, cols, header_rows)
  row <- header_rows + 1
  wb |>
    openxlsx2::wb_add_conditional_formatting(sheet, dims = dims, rule = sprintf('$C%d<>""', row),
                                             style = "holiday") |>
    openxlsx2::wb_add_conditional_formatting(sheet, dims = dims,
                                             rule = sprintf('OR($B%1$d="Sat",$B%1$d="Sun")', row),
                                             style = "weekend")
}

# Colour each code cell by its code, as a conditional format so that hand
# edits take the right colour
colour_codes <- function(wb, sheet, x, cols, colours, header_rows = 1) {
  dims <- data_dims(x, cols, header_rows)
  first <- paste0(openxlsx2::int2col(min(cols)), header_rows + 1)
  for (code in colours$code) {
    wb <- openxlsx2::wb_add_conditional_formatting(
      wb, sheet, dims = dims, rule = sprintf('%s="%s"', first, code), style = code_style_name(code)
    )
  }
  wb
}

# Fill the cells holding `codes` (a column from row + 1, or a header row
# from first_col) in their code's colours
fill_codes <- function(wb, sheet, codes, colours, row = 1, first_col = NULL) {
  for (k in seq_along(codes)) {
    i <- match(codes[k], colours$code)
    if (is.na(i)) next
    dims <- if (is.null(first_col)) openxlsx2::wb_dims(rows = row + k, cols = 1)
            else openxlsx2::wb_dims(rows = row, cols = first_col - 1 + k)
    wb <- openxlsx2::wb_add_fill(wb, sheet, dims = dims, color = argb(colours$fill[i]))
    if (!is.na(colours$text[i])) {
      wb <- openxlsx2::wb_add_font(wb, sheet, dims = dims, color = argb(colours$text[i]),
                                   bold = !is.null(first_col))
    }
  }
  wb
}

# Landscape, one page wide, with the heading rows on every page
print_setup <- function(wb, sheet, title_rows) {
  openxlsx2::wb_page_setup(wb, sheet, orientation = "landscape", fit_to_width = 1,
                           fit_to_height = 0, print_title_rows = title_rows,
                           print_title_cols = 1:2)
}

#' Write per-doctor calendars
#'
#' Writes one iCalendar (`.ics`) file per doctor with their shifts and
#' approved leave, which they can import into Outlook, Google Calendar or
#' Apple Calendar, and one for the whole department, `All_doctors.ics`, with
#' everyone's shifts titled like `"ICU1 night: Dr Ada Alder"`. Shift times are local wall-clock times (no time zone), so
#' they show as rostered whatever the calendar's time zone.
#'
#' @inheritParams write_rota
#' @param dir Folder to write the files into, created if needed. Files are
#'   named after the doctor, e.g. `D01_Ada_Alder.ics`, and overwritten if
#'   they exist.
#' @param doctors Doctor IDs to write calendars for. Defaults to every doctor
#'   in the staff list with at least one shift or day of leave.
#' @param all_doctors Also write `All_doctors.ics`, with the shifts of every
#'   doctor in `doctors`.
#'
#' @returns The paths of the files written, invisibly.
#' @export
#' @examples
#' inputs <- read_rota_inputs(rota_example())
#' rota <- read_rota(rota_example("residents", "output/rota.xlsx"))
#' write_calendars(rota, inputs, tempfile("calendars"), doctors = "D01")
write_calendars <- function(rota, inputs, dir, doctors = NULL, all_doctors = TRUE) {
  inputs <- as_rota_inputs(inputs)
  rota <- as_rota(rota)
  labels <- doctor_labels(inputs$staff)

  shifts <- shift_instances(rota, inputs) |>
    transmute(
      doctor_id,
      uid = paste(doctor_id, date, code, sep = "-"),
      start = format(start_dt, "%Y%m%dT%H%M%S", tz = "UTC"),
      end = format(end_dt, "%Y%m%dT%H%M%S", tz = "UTC"),
      summary = shift_label(role, shift_type, label),
      name = inputs$staff$name[match(doctor_id, inputs$staff$doctor_id)],
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
    write_ics(path, paste("Rota", labels[[id]]), filter(events, doctor_id == id))
  })
  if (all_doctors) {
    everyone <- shifts |>
      filter(doctor_id %in% doctors) |>
      mutate(uid = paste0("all-", uid), summary = paste0(summary, ": ", name))
    paths <- c(paths, write_ics(file.path(dir, "All_doctors.ics"), "Rota: all doctors", everyone))
  }
  invisible(paths)
}

# Write events (uid, start, end, summary, description, all_day) as an
# iCalendar file
write_ics <- function(path, name, events) {
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  events <- arrange(events, start)
  lines <- c(
    "BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//rota-writer//EN", "CALSCALE:GREGORIAN",
    paste0("X-WR-CALNAME:", ics_text(name)),
    unlist(pmap(events, \(uid, start, end, summary, description, all_day, ...) c(
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
