# 00_move_met_files.R
# aw 2026-01-21
# Move SevMET/*/*_{air|soil1}.dat(.backup) up to ./data_raw

# Working directory should be set using Session -> Set Working Directory. Not hard coded. 
# .
# └── Project name/
#   ├── data_raw
#   ├── data_output_files


library(stringr)
library(tidyverse)
library(purrr)
library(tibble)
library(readr)

move_met_files <- function(base_dir = "./data_raw",
                           sensors = c("air", "soil1")) {
  src_root <- file.path(base_dir, "SevMET")
  if (!dir.exists(src_root)) {
    message("No subfolder 'SevMET' under ", base_dir, ". Nothing to move.")
    return(tibble(sensor = character(), source = character(), dest = character(), status = character()))
  }
  
  normdir <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  
  # Helper: move/copy one list of files for a given sensor suffix ("air" or "soil1")
  move_for_sensor <- function(sensor_suffix) {
    # accept both "*.dat" and "*.dat.backup"
    pat <- sprintf("SevMET_[A-Za-z0-9]+_%s\\.dat(\\.backup)?$", sensor_suffix)
    
    files <- list.files(
      src_root,
      pattern = pat,
      recursive = TRUE,
      full.names = TRUE
    )
    
    if (!length(files)) {
      message("No ", sensor_suffix, " .dat/.backup files found under ", src_root)
      return(tibble(sensor = character(), source = character(), dest = character(), status = character()))
    }
    
    map_dfr(files, function(f) {
      dest <- file.path(base_dir, basename(f))
      status <- NA_character_
      
      if (normdir(dirname(f)) == normdir(base_dir)) {
        status <- "already_in_place"
      } else if (file.exists(dest)) {
        status <- "skipped_exists"
      } else {
        ok <- tryCatch(file.rename(f, dest),
                       warning = function(w) FALSE,
                       error   = function(e) FALSE)
        if (!ok) {
          ok <- file.copy(f, dest, overwrite = FALSE)
          if (ok) unlink(f)
        }
        status <- if (ok) "moved" else "failed"
      }
      
      tibble(sensor = sensor_suffix, source = f, dest = dest, status = status)
    })
  }
  
  # Run for each requested sensor type
  res_list <- lapply(sensors, move_for_sensor)
  res <- bind_rows(res_list)
  
  # Summary
  if (nrow(res)) {
    summary_tbl <- res %>%
      count(sensor, status, name = "n") %>%
      arrange(sensor, desc(n))
    print(summary_tbl)
    message(
      "Files processed: ", nrow(res),
      " | moved: ", sum(res$status == "moved"),
      " | skipped (exists): ", sum(res$status == "skipped_exists"),
      " | already in place: ", sum(res$status == "already_in_place"),
      " | failed: ", sum(res$status == "failed")
    )
  } else {
    message("Nothing processed.")
  }
  
  # Write a single combined log next to your other outputs
  out_dir <- file.path(getwd(), "data_output_files")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write_csv(res, file.path(out_dir, "moved_files_log.csv"))
  
  invisible(res)
}

# Run the movers (AIR + SOIL1)
move_log <- move_met_files("./data_raw", sensors = c("air", "soil1"))

# For convenience, list the files now in place
file_list_air  <- list.files("./data_raw", pattern = "SevMET_[A-Za-z0-9]+_air\\.(dat|backup)$",  full.names = TRUE)
file_list_soil <- list.files("./data_raw", pattern = "SevMET_[A-Za-z0-9]+_soil1\\.(dat|backup)$", full.names = TRUE)

message("Air files now in ./data_raw:  ", length(file_list_air))
message("Soil files now in ./data_raw:", length(file_list_soil))

# Next:
#   - Run 01_raw_hourly_met2_0_processing.R for air
#   - Run 01_raw_hourly_soil_processing.R for soil
