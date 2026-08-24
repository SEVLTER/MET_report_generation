# 01_raw_hourly_soil_processing.R
# Merge and process SEV MET soil station TOA5 data
# Produces hourly summaries from MET soil data loggers.

library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)
rm(list = ls(all = TRUE))

message("Your working directory is: ", getwd())
out_dir <- file.path(getwd(), "data_output_files")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
message("Output files will be written to: ", out_dir)

# Grab both .dat and .backup
file_list <- list.files(
  "./data_raw/",
  pattern = "SevMET_[A-Za-z0-9]+_soil1\\.(dat|backup)$",
  full.names = TRUE
)
stopifnot(length(file_list) > 0)

# --- TOA5 reader (same logic as air) ---
read_toa5 <- function(file, tz_local = "America/Denver") {
  hdr_lines <- readr::read_lines(file, n_max = 30)
  hdr_idx <- which(str_detect(hdr_lines, '^"?TIMESTAMP"?,'))[1]
  if (is.na(hdr_idx)) stop("Could not find TIMESTAMP header in: ", basename(file))
  
  col_names <- hdr_lines[hdr_idx] |>
    str_split(",", simplify = TRUE) |>
    as.vector() |>
    str_remove_all('^"|"$') |>
    str_remove_all('"')
  
  skip_rows <- hdr_idx + 2
  station_id <- str_extract(basename(file), "(?<=SevMET_)[A-Za-z0-9]+(?=_soil1\\.)")
  
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

# --- Read and combine all soil files ---
all_raw <- map_dfr(file_list, read_toa5) %>%
  arrange(station, TIMESTAMP)

# QAQC same as air script
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

write_csv(suspect_rows, file.path(out_dir, "soil_suspect_timestamp_overlap.csv"))

notdeduped <- nrow(all_raw)
all_raw <- distinct(all_raw)
deduped <- nrow(all_raw)
message("There were ", notdeduped, " rows. ", notdeduped - deduped, " duplicates removed.")

# Fill missing timestamps
check_missing_timestamps <- function(df, station_id, interval = "5 mins") {
  df_station <- df %>% filter(station == station_id)
  
  ts_seq <- tibble(
    TIMESTAMP = seq(min(df_station$TIMESTAMP, na.rm = TRUE),
                    max(df_station$TIMESTAMP, na.rm = TRUE),
                    by = interval)
  )
  
  full_df <- ts_seq %>%
    left_join(df_station, by = "TIMESTAMP") %>%
    mutate(station = station_id,
           missing = !TIMESTAMP %in% df_station$TIMESTAMP)
  
  full_df
}

all_checked <- unique(all_raw$station) %>%
  map_dfr(~ check_missing_timestamps(all_raw, .x)) %>%
  mutate(
    month = month(TIMESTAMP),
    year  = year(TIMESTAMP),
    day   = day(TIMESTAMP),
    hour  = floor_date(TIMESTAMP, "hour")
  )

# Hourly aggregation
exclude_exact <- c("RECORD", "batt_Avg", "missing")
exclude_exact <- intersect(exclude_exact, names(all_checked))

hourly_summary <- all_checked %>%
  select(-all_of(exclude_exact)) %>%
  group_by(station, hour) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(station, hour) %>%
  mutate(year = year(hour), month = month(hour), day = day(hour))

write_csv(hourly_summary, file.path(out_dir, "soil_hourly_raw.csv"))

message("Done. Next step: summarize or plot soil data as needed. Run 02_soil_summary.")
