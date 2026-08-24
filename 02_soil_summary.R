# 02_soil_summary.R
# Run 01_raw_hourly_soil_processing.R first
# Gives the summary of the soil sensors. Daily and monthly. 

library(tidyverse)
library(lubridate)

options(dplyr.summarise.inform = FALSE)

# Paths & checks

out_dir <- file.path(getwd(), "data_output_files")
if (!dir.exists(out_dir)) {
  stop("data_output_files/ not found. Run 01_raw_hourly_soil_processing.R first.")
}

hourly_path <- file.path(out_dir, "soil_hourly_raw.csv")
if (!file.exists(hourly_path)) stop("Missing: ", hourly_path)

tz_local <- "America/Denver"

# Read hourly soil data (parse as UTC then convert to local TZ)

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
# numeric columns safe to summaries (exclude calendar helpers if present)
num_cols_to_summarise <- hourly %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("year","month"))) %>%
  names()

# Monthly summary
monthly <- hourly %>%
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
  ) %>%
  rowwise() %>%
  mutate(
    expected_hours = expected_hours_in_month(year, month, tz = tz_local),
    coverage       = round(hours_present / expected_hours, 3),
    quality_flag   = case_when(
      is.na(coverage)  ~ "no_data",
      coverage >= 0.90 ~ "OK (>=90%)",
      coverage >= 0.80 ~ "Warning (80–90%)",
      TRUE             ~ "Low (<80%)"
    )
  ) %>%
  ungroup() %>%
  arrange(station, year, month) %>%
  mutate(ym = sprintf("%04d-%02d", year, month)) %>%
  relocate(ym, .after = month) %>%
  # ---- Only round mean columns to 1 dp ----
mutate(across(ends_with("_mean"), ~ round(.x, 1)))

readr::write_csv(monthly, file.path(out_dir, "soil_monthly_summary.csv"))

# Monthly long (for plotting)
monthly_metric_cols <- names(monthly) %>%
  setdiff(c(
    "station","year","month","ym",
    "hours_present","expected_hours","coverage","quality_flag"
  ))

monthly_long <- monthly %>%
  select(station, year, month, ym, all_of(monthly_metric_cols),
         coverage, quality_flag) %>%
  pivot_longer(
    cols = all_of(monthly_metric_cols),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(mean|min|max)$"
  )
# means already rounded above; min/max kept raw

readr::write_csv(monthly_long, file.path(out_dir, "soil_monthly_summary_long.csv"))

# Daily summary
daily <- hourly %>%
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
  ) %>%
  mutate(
    expected_hours = expected_hours_in_day(date, tz = tz_local),
    coverage       = round(hours_present / expected_hours, 3),
    quality_flag   = case_when(
      is.na(coverage)  ~ "no_data",
      coverage >= 0.90 ~ "OK (>=90%)",
      coverage >= 0.80 ~ "Warning (80–90%)",
      TRUE             ~ "Low (<80%)"
    ),
    year  = year(date),
    month = month(date),
    day   = mday(date)
  ) %>%
  relocate(year, month, day, .after = date) %>%
  arrange(station, date) %>%
  # Only round mean columns to 1 dp
mutate(across(ends_with("_mean"), ~ round(.x, 1)))

readr::write_csv(daily, file.path(out_dir, "soil_daily_summary.csv"))

# Daily long
daily_metric_cols <- names(daily) %>%
  setdiff(c(
    "station","date","year","month","day",
    "hours_present","expected_hours","coverage","quality_flag"
  ))

daily_long <- daily %>%
  select(station, date, year, month, day,
         all_of(daily_metric_cols),
         coverage, quality_flag) %>%
  pivot_longer(
    cols = all_of(daily_metric_cols),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(mean|min|max)$"
  )
# (means already rounded above; min/max kept raw)

readr::write_csv(daily_long, file.path(out_dir, "soil_daily_summary_long.csv"))

message(
  "Wrote:\n  - ", file.path(out_dir, "soil_monthly_summary.csv"),
  "\n  - ", file.path(out_dir, "soil_monthly_summary_long.csv"),
  "\n  - ", file.path(out_dir, "soil_daily_summary.csv"),
  "\n  - ", file.path(out_dir, "soil_daily_summary_long.csv")
)
