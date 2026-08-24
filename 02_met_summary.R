# 02_monthly_summary.R
# Run 01_raw_hourly_met2_0_processing.R first
# This does the daily, monthly, and annual summaries. 

library(tidyverse)
library(lubridate)

options(dplyr.summarise.inform = FALSE)


# Paths & checks
out_dir <- file.path(getwd(), "data_output_files")
if (!dir.exists(out_dir)) {
  stop("data_output_files/ not found. Run 01_raw_hourly_met2_0_processing.R first.")
}

hourly_path <- file.path(out_dir, "hourly_raw.csv")
if (!file.exists(hourly_path)) stop("Missing: ", hourly_path)

tz_local <- "America/Denver"

# Read hourly data
# We parse as UTC then convert to local TZ (DST-safe)
hourly <- readr::read_csv(
  hourly_path,
  show_col_types = FALSE,
  col_types = readr::cols(
    hour = readr::col_datetime(),
    .default = readr::col_guess()
  ),
  locale = readr::locale(tz = "UTC")
) %>%
  mutate(
    hour  = with_tz(hour, tz_local),
    year  = year(hour),
    month = month(hour),
    date  = as_date(hour, tz = tz_local)
  )

# Helpers for expected hours (DST-aware)
expected_hours_in_month <- function(y, m, tz = tz_local) {
  start <- ymd(sprintf("%04d-%02d-01", y, m), tz = tz)
  end   <- start %m+% months(1)
  as.numeric(difftime(end, start, units = "hours"))
}

expected_hours_in_day <- function(d, tz = tz_local) {
  start <- as_datetime(d, tz = tz)
  end   <- start + days(1)
  as.numeric(difftime(end, start, units = "hours"))
}

# Column handling
id_cols      <- c("station", "year", "month", "date", "hour")
precip_col   <- "precip_mm_hour"
has_precip   <- precip_col %in% names(hourly)

# Numerical columns to summarise (exclude IDs, calendar, precip)
num_cols_to_summarise <- hourly %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("year", "month", precip_col))) %>%
  names()

# A tiny (but important!) helper to round to 1 decimal place
r1dp <- function(x) round(x, 1)

# Monthly summary
monthly_core <- hourly %>%
  group_by(station, year, month) %>%
  summarise(
    hours_present = n_distinct(hour),
    across(
      all_of(num_cols_to_summarise),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        min  = ~ suppressWarnings(min(.x, na.rm = TRUE)),
        max  = ~ suppressWarnings(max(.x, na.rm = TRUE))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

monthly_precip <- if (has_precip) {
  hourly %>%
    group_by(station, year, month) %>%
    summarise(
      precip_mm_month   = sum(.data[[precip_col]], na.rm = TRUE),
      hours_with_precip = sum(.data[[precip_col]] > 0, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  warning("No 'precip_mm_hour' found in hourly_raw.csv; monthly precip will be NA.")
  tibble(
    station = character(), year = integer(), month = integer(),
    precip_mm_month = numeric(), hours_with_precip = integer()
  )
}

monthly <- monthly_core %>%
  left_join(monthly_precip, by = c("station", "year", "month")) %>%
  rowwise() %>%
  mutate(
    expected_hours = expected_hours_in_month(year, month, tz = tz_local),
    coverage       = round(hours_present / expected_hours, 3),
    quality_flag   = case_when(
      is.na(coverage)      ~ "no_data",
      coverage >= 0.90     ~ "OK (>=90%)",
      coverage >= 0.80     ~ "Warning (80–90%)",
      TRUE                 ~ "Low (<80%)"
    )
  ) %>%
  ungroup() %>%
  arrange(station, year, month) %>%
  mutate(ym = sprintf("%04d-%02d", year, month)) %>%
  relocate(ym, .after = month) %>%
  # Round to 1 decimal place for summaries
  mutate(
    across(matches("_(mean|min|max)$"), r1dp),
    precip_mm_month = if ("precip_mm_month" %in% names(.)) r1dp(precip_mm_month) else precip_mm_month,
    expected_hours = r1dp(expected_hours)
  )

# Write monthly wide
readr::write_csv(monthly, file.path(out_dir, "monthly_summary.csv"))

# Monthly long (for plotting)
monthly_metric_cols <- names(monthly) %>%
  setdiff(c(
    "station","year","month","ym","hours_present","expected_hours",
    "coverage","quality_flag","precip_mm_month","hours_with_precip"
  ))

monthly_long <- monthly %>%
  select(station, year, month, ym, all_of(monthly_metric_cols),
         precip_mm_month, coverage, quality_flag) %>%
  pivot_longer(
    cols = all_of(monthly_metric_cols),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(mean|min|max)$"
  )


readr::write_csv(monthly_long, file.path(out_dir, "monthly_summary_long.csv"))

# Daily summary
daily_core <- hourly %>%
  group_by(station, date) %>%
  summarise(
    hours_present = n_distinct(hour),
    across(
      all_of(num_cols_to_summarise),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        min  = ~ suppressWarnings(min(.x, na.rm = TRUE)),
        max  = ~ suppressWarnings(max(.x, na.rm = TRUE))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

daily_precip <- if (has_precip) {
  hourly %>%
    group_by(station, date) %>%
    summarise(
      precip_mm_day     = sum(.data[[precip_col]], na.rm = TRUE),
      hours_with_precip = sum(.data[[precip_col]] > 0, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  tibble(
    station = character(), date = as_date(character()),
    precip_mm_day = numeric(), hours_with_precip = integer()
  )
}

daily <- daily_core %>%
  left_join(daily_precip, by = c("station", "date")) %>%
  mutate(
    expected_hours = expected_hours_in_day(date, tz = tz_local),
    coverage       = round(hours_present / expected_hours, 3),
    quality_flag   = case_when(
      is.na(coverage)      ~ "no_data",
      coverage >= 0.90     ~ "OK (>=90%)",
      coverage >= 0.80     ~ "Warning (80–90%)",
      TRUE                 ~ "Low (<80%)"
    ),
    year  = year(date),
    month = month(date),
    day   = mday(date)
  ) %>%
  relocate(year, month, day, .after = date) %>%
  arrange(station, date) %>%
  # Round to 1 decimal place
  mutate(
    across(matches("_(mean|min|max)$"), r1dp),
    precip_mm_day = if ("precip_mm_day" %in% names(.)) r1dp(precip_mm_day) else precip_mm_day,
    expected_hours = r1dp(expected_hours)
  )

# Write daily wide
readr::write_csv(daily, file.path(out_dir, "daily_summary.csv"))

# Daily long
daily_metric_cols <- names(daily) %>%
  setdiff(c(
    "station","date","year","month","day",
    "hours_present","expected_hours","coverage","quality_flag",
    "precip_mm_day","hours_with_precip"
  ))

daily_long <- daily %>%
  select(station, date, year, month, day,
         all_of(daily_metric_cols),
         precip_mm_day, coverage, quality_flag) %>%
  pivot_longer(
    cols = all_of(daily_metric_cols),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(mean|min|max)$"
  )

readr::write_csv(daily_long, file.path(out_dir, "daily_summary_long.csv"))

# Annual summary

# DST- and leap-year-aware expected hours in a year
expected_hours_in_year <- function(y, tz = tz_local) {
  start <- ymd(sprintf("%04d-01-01", y), tz = tz)
  end   <- start %m+% years(1)
  as.numeric(difftime(end, start, units = "hours"))
}

annual_core <- hourly %>%
  group_by(station, year) %>%
  summarise(
    hours_present = n_distinct(hour),
    across(
      all_of(num_cols_to_summarise),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        min  = ~ suppressWarnings(min(.x, na.rm = TRUE)),
        max  = ~ suppressWarnings(max(.x, na.rm = TRUE))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

annual_precip <- if (has_precip) {
  hourly %>%
    group_by(station, year) %>%
    summarise(
      precip_mm_year    = sum(.data[[precip_col]], na.rm = TRUE),
      hours_with_precip = sum(.data[[precip_col]] > 0, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  warning("No 'precip_mm_hour' found in hourly_raw.csv; annual precip will be NA.")
  tibble(
    station = character(), year = integer(),
    precip_mm_year = numeric(), hours_with_precip = integer()
  )
}

annual <- annual_core %>%
  left_join(annual_precip, by = c("station", "year")) %>%
  rowwise() %>%
  mutate(
    expected_hours = expected_hours_in_year(year, tz = tz_local),
    coverage       = round(hours_present / expected_hours, 3),
    quality_flag   = case_when(
      is.na(coverage)      ~ "no_data",
      coverage >= 0.90     ~ "OK (>=90%)",
      coverage >= 0.80     ~ "Warning (80–90%)",
      TRUE                 ~ "Low (<80%)"
    )
  ) %>%
  ungroup() %>%
  arrange(station, year) %>%
  # Round to 1 decimal place for summaries
  mutate(
    across(matches("_(mean|min|max)$"), r1dp),
    precip_mm_year = if ("precip_mm_year" %in% names(.)) r1dp(precip_mm_year) else precip_mm_year,
    expected_hours = r1dp(expected_hours)
  )

# Write annual wide
readr::write_csv(annual, file.path(out_dir, "annual_summary.csv"))

# Annual long (for plotting) — optional
annual_metric_cols <- names(annual) %>%
  setdiff(c(
    "station","year",
    "hours_present","expected_hours","coverage","quality_flag",
    "precip_mm_year","hours_with_precip"
  ))

annual_long <- annual %>%
  select(station, year, all_of(annual_metric_cols),
         precip_mm_year, coverage, quality_flag) %>%
  pivot_longer(
    cols = all_of(annual_metric_cols),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(mean|min|max)$"
  )

readr::write_csv(annual_long, file.path(out_dir, "annual_summary_long.csv"))

message(
  "Wrote:\n  - ", file.path(out_dir, "monthly_summary.csv"),
  "\n  - ", file.path(out_dir, "monthly_summary_long.csv"),
  "\n  - ", file.path(out_dir, "daily_summary.csv"),
  "\n  - ", file.path(out_dir, "daily_summary_long.csv"),
  "\n  - ", file.path(out_dir, "annual_summary.csv"),
  "\n  - ", file.path(out_dir, "annual_summary_long.csv")
)

