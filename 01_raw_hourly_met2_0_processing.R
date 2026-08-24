# 01_raw_hourly_met2_0_processing.R
# Change: precip is now included in hourly output (no separate rain_raw.csv)
# This processes the hourly data with some QAQC and writes out csv files. 

library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)

# Clears out everything
rm(list = ls(all = TRUE))

message("Your working directory is: ", getwd())
out_dir <- file.path(getwd(), "data_output_files")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
message("Output files will be written to: ", out_dir)

# Grab both .dat and .backup
file_list <- list.files(
  "./data_raw",
  pattern = "SevMET_[A-Za-z0-9]+_air\\.(dat|backup)$",
  full.names = TRUE
)
stopifnot(length(file_list) > 0)

# read TOA5 safely across variants
read_toa5 <- function(file, tz_local = "America/Denver") {
  hdr_lines <- readr::read_lines(file, n_max = 30)
  hdr_idx <- which(str_detect(hdr_lines, '^"?TIMESTAMP"?,'))[1]
  if (is.na(hdr_idx)) stop("Could not find a TIMESTAMP header in: ", basename(file))
  
  col_names <- hdr_lines[hdr_idx] |>
    str_split(",", simplify = TRUE) |>
    as.vector() |>
    str_remove_all('^"|"$') |>
    str_remove_all('"')
  
  # Dump column names, units, and processing types
  # TOA5 row layout: [hdr_idx] = column names,
  #                  [hdr_idx+1] = units  (e.g. "deg C", "W/m2", "mm", …),
  #                  [hdr_idx+2] = processing type (Avg, Min, Max, Smp, Tot, …)
  parse_toa5_row <- function(line) {
    line |>
      str_split(",", simplify = TRUE) |>
      as.vector() |>
      str_remove_all('^"|"$') |>
      str_remove_all('"')
  }
  
  col_units <- if (length(hdr_lines) >= hdr_idx + 1)
    parse_toa5_row(hdr_lines[hdr_idx + 1]) else rep(NA_character_, length(col_names))
  col_proc  <- if (length(hdr_lines) >= hdr_idx + 2)
    parse_toa5_row(hdr_lines[hdr_idx + 2]) else rep(NA_character_, length(col_names))
  
  # Pad to same length (guard against ragged rows)
  n <- length(col_names)
  length(col_units) <- n
  length(col_proc)  <- n
  
  col_schema <- tibble(
    column          = col_names,
    unit            = col_units,
    processing_type = col_proc
  )
  
  station_id_tmp <- str_extract(basename(file), "(?<=SevMET_)[A-Za-z0-9]+(?=_air\\.)")
  cat("\n=== Column schema for station:", station_id_tmp,
      "| file:", basename(file), "===\n")
  print(col_schema, n = Inf)
  cat("\n")
  
  # Write schema to CSV alongside the other output files
  schema_path <- file.path(
    file.path(getwd(), "data_output_files"),
    paste0("column_schema_", station_id_tmp, ".csv")
  )
  dir.create(dirname(schema_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(col_schema, schema_path)
  # End column schema dump
  
  # Data start is typically header + 2 lines (units, processing types)
  skip_rows <- hdr_idx + 2
  
  station_id <- str_extract(basename(file), "(?<=SevMET_)[A-Za-z0-9]+(?=_air\\.)")
  
  dat <- read_csv(
    file,
    skip = skip_rows,
    col_names = col_names,
    na = c("", "NA", "-9999", "-99999", "-999.99"),
    show_col_types = FALSE,
    col_types = cols(.default = col_double(), TIMESTAMP = col_character())
  ) %>%
    mutate(
      TIMESTAMP = parse_date_time(
        TIMESTAMP,
        orders = c("Y-m-d H:M:S", "mdy HMS", "mdy HM", "Ymd HMS"),
        tz = tz_local
      ),
      station = station_id
    )
  
  cat("\n--- NA summary for station:", station_id, "---\n")
  print(dat %>% summarise(across(everything(), ~ sum(is.na(.)))))
  
  dat
}

# Read & combine
all_raw <- map_dfr(file_list, read_toa5) %>%
  arrange(station, TIMESTAMP)
colnames(all_raw)

# QAQC Find suspect overlapping timestamps 
is_close_match <- function(df, tol = 0.1, frac = 0.9) {
  diffs <- df %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(), ~ diff(range(., na.rm = TRUE)))) %>%
    unlist(use.names = FALSE)
  diffs <- diffs[is.finite(diffs)]
  if (!length(diffs)) return(TRUE)
  mean(diffs < tol) >= frac
}

suspect_rows <- all_raw %>%
  group_by(station, TIMESTAMP) %>%
  filter(n() > 1) %>%
  group_modify(~ if (!is_close_match(.x)) .x else tibble()) %>%
  ungroup()

write_csv(suspect_rows, file.path(out_dir, "suspect_timestamp_overlap.csv"))

# QAQC De-duplicate exact duplicate rows
notdeduped <- nrow(all_raw)
all_raw <- distinct(all_raw)
deduped <- nrow(all_raw)
message(
  "There were ", notdeduped, " rows. ",
  (notdeduped - deduped), " duplicate rows were removed. ",
  "The deduplicated dataset contains ", deduped, " rows."
)

# Fill calendar fields & completeness scaffold
check_missing_timestamps <- function(df, station_id, interval = "5 mins") {
  df_station <- df %>% filter(station == station_id)
  
  if (nrow(df_station) == 0 || all(is.na(df_station$TIMESTAMP))) {
    warning(glue::glue("No valid timestamps found for station {station_id}"))
    return(tibble(TIMESTAMP = as.POSIXct(NA), station = station_id, missing = NA))
  }
  
  ts_seq <- tibble(
    TIMESTAMP = seq(min(df_station$TIMESTAMP, na.rm = TRUE),
                    max(df_station$TIMESTAMP, na.rm = TRUE),
                    by = interval)
  )
  
  full_df <- ts_seq %>%
    left_join(df_station, by = "TIMESTAMP") %>%
    mutate(
      station = station_id,
      missing = !TIMESTAMP %in% df_station$TIMESTAMP
    )
  
  full_df
}

all_checked <- unique(all_raw$station) %>%
  map_dfr(~ check_missing_timestamps(all_raw, .x, interval = "5 mins")) %>%
  mutate(
    month = month(TIMESTAMP),
    year  = year(TIMESTAMP),
    day   = day(TIMESTAMP),
    hour  = floor_date(TIMESTAMP, unit = "hour")
  )
all_checked
write_csv(all_checked, file.path(out_dir, "checked_raw.csv"))


# Hourly aggregation (precip kept in same file)
# Columns to exclude from numeric means
exclude_exact <- c("RECORD", "batt_Avg",
                   "CS320_Z_Avg", "missing", "year", "month", "day")
# parDen is coming from the licor sensor that measures Photosynthetic Active Radiation.
# swRad is the pyranometer that measures shortwave radiation.

exclude_exact <- intersect(exclude_exact, names(all_checked))

# Detect precip column (new stations do not need special handling)
precip_candidates <- intersect(names(all_checked),
                               c("precip_Tot", "precip_tot", "precip_total"))
has_precip <- length(precip_candidates) >= 1

# Build hourly means for met variables
hourly_means <- all_checked %>%
  select(-all_of(exclude_exact), -any_of(precip_candidates)) %>%
  group_by(station, hour) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(station, hour)

# Add hourly min/max for air temperature
if ("airTemp_Avg" %in% names(all_checked)) {
  hourly_airtemp_range <- all_checked %>%
    group_by(station, hour) %>%
    summarise(
      airTemp_Min = min(airTemp_Avg, na.rm = TRUE),
      airTemp_Max = max(airTemp_Avg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Replace Inf/-Inf (all-NA hours) with NA
    mutate(
      airTemp_Min = if_else(is.infinite(airTemp_Min), NA_real_, airTemp_Min),
      airTemp_Max = if_else(is.infinite(airTemp_Max), NA_real_, airTemp_Max)
    ) %>%
    arrange(station, hour)
  
  hourly_means <- hourly_means %>%
    left_join(hourly_airtemp_range, by = c("station", "hour"))
} else {
  warning("airTemp_Avg column not found; skipping hourly min/max calculation.")
}

# If present, add hourly precip as a sum into the same table
if (has_precip) {
  # If multiple precip cols slip through, take the first non-NA per row then sum
  precip_col <- precip_candidates[1]
  hourly_precip <- all_checked %>%
    group_by(station, hour) %>%
    summarise(
      precip_mm_hour = sum(.data[[precip_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(station, hour)
  
  hourly_summary <- hourly_means %>%
    left_join(hourly_precip, by = c("station", "hour"))
} else {
  warning("No precip total column found (looked for: precip_Tot / precip_tot / precip_total). Proceeding without precip.")
  hourly_summary <- hourly_means
}

# Optional: bring back calendar fields (year, month, day) computed from hour
hourly_summary <- hourly_summary %>%
  mutate(
    year  = year(hour),
    month = month(hour),
    day   = day(hour)
  ) %>%
  relocate(year, month, day, .after = hour)

# Write single hourly file (now includes precip_mm_hour when available)
write_csv(hourly_summary, file.path(out_dir, "hourly_raw.csv"))

# Sensor health written out
sensor_health <- all_raw %>%
  select(any_of(c("TIMESTAMP", "RECORD", "station", "batt_Avg")))

write_csv(sensor_health, file.path(out_dir, "sensor_health_raw.csv"))

message("Done. Next step: run 02_met_summary.R .")