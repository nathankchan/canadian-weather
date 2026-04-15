# pipeline.R
# R replacement for getdata.sh, update.sh, removecorruptfiles.sh, and run.sh.
# Can be sourced by app.R for in-app updates, or run standalone via
# `Rscript pipeline.R` as a drop-in replacement for run.sh.

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(parallel)
  library(curl)
})

# ---------------------------------------------------------------------------
# URL builder
# ---------------------------------------------------------------------------

eccc_csv_url <- function(station_id, year, month) {
  sprintf(
    paste0(
      "https://climate.weather.gc.ca/climate_data/bulk_data_e.html",
      "?format=csv&stationID=%s&Year=%d&Month=%02d",
      "&Day=14&timeframe=1&submit=Download+Data"
    ),
    station_id,
    as.integer(year),
    as.integer(month)
  )
}

# ---------------------------------------------------------------------------
# Download all monthly CSVs for one station (replaces getdata.sh)
# ---------------------------------------------------------------------------

download_station_csvs <- function(
  station_id,
  start_year,
  end_year,
  rawdata_dir = "./rawdata"
) {
  station_dir <- file.path(rawdata_dir, station_id)
  if (!dir.exists(station_dir)) {
    dir.create(station_dir, recursive = TRUE)
  }

  downloaded <- character(0)
  for (year in seq(as.integer(start_year), as.integer(end_year))) {
    for (month in seq_len(12)) {
      filename <- sprintf("%d_%02d.csv", year, month)
      destfile <- file.path(station_dir, filename)

      if (file.exists(destfile)) {
        next
      }

      url <- eccc_csv_url(station_id, year, month)
      tryCatch(
        {
          curl_download(url, destfile, quiet = TRUE)
          downloaded <- c(downloaded, destfile)
        },
        error = function(e) {
          if (file.exists(destfile)) unlink(destfile)
        }
      )
    }
  }
  downloaded
}

# ---------------------------------------------------------------------------
# Remove corrupt (empty / zero-byte) files for one station
# (replaces removecorruptfiles.sh)
# ---------------------------------------------------------------------------

remove_corrupt_files <- function(station_id, rawdata_dir = "./rawdata") {
  station_dir <- file.path(rawdata_dir, station_id)
  if (!dir.exists(station_dir)) {
    return(character(0))
  }

  files <- list.files(station_dir, full.names = TRUE)
  if (length(files) == 0L) {
    return(character(0))
  }

  sizes <- file.size(files)
  bad <- files[is.na(sizes) | sizes == 0L]
  if (length(bad) > 0L) {
    unlink(bad)
  }
  bad
}

# ---------------------------------------------------------------------------
# Remove stale files whose modification time predates their month-end
# (replaces update.sh)
# ---------------------------------------------------------------------------

remove_stale_files <- function(station_id, rawdata_dir = "./rawdata") {
  station_dir <- file.path(rawdata_dir, station_id)
  if (!dir.exists(station_dir)) {
    return(character(0))
  }

  files <- list.files(station_dir, full.names = TRUE)
  if (length(files) == 0L) {
    return(character(0))
  }

  removed <- character(0)
  now <- Sys.time()

  for (f in files) {
    bn <- basename(f)
    parts <- regmatches(bn, regexec("^(\\d{4})_(\\d{2})\\.csv$", bn))[[1]]
    if (length(parts) != 3L) {
      next
    }

    year <- as.integer(parts[2])
    month <- as.integer(parts[3])

    file_date <- as.POSIXct(sprintf("%d-%02d-01", year, month), tz = "UTC")
    month_end <- seq(file_date, by = "1 month", length.out = 2)[2]

    if (now > file_date && file.mtime(f) < month_end) {
      unlink(f)
      removed <- c(removed, f)
    }
  }
  removed
}

# ---------------------------------------------------------------------------
# Combine CSVs, drop empty rows, type-convert, write parquet
# (mirrors helper.R logic)
# ---------------------------------------------------------------------------

combine_csv <- function(dirname) {
  csv_files <- list.files(dirname, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0L) {
    stop("No CSV files found in ", dirname)
  }
  data_list <- lapply(csv_files, function(x) {
    read_csv(
      x,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
  })
  bind_rows(data_list)
}

remove_empty_rows <- function(x, col) {
  x[!is.na(x[[col]]), ]
}

build_parquet <- function(station_id, emptycol, rawdata_dir, outdir) {
  station_dir <- paste0(file.path(rawdata_dir, station_id), "/")
  data <- combine_csv(station_dir) |>
    remove_empty_rows(col = emptycol)
  write_parquet(
    type_convert(data, col_types = cols()),
    file.path(outdir, paste0(station_id, ".parquet"))
  )
}

# ---------------------------------------------------------------------------
# Full per-station update pipeline
# ---------------------------------------------------------------------------

update_station <- function(
  station_id,
  start_year,
  end_year,
  rawdata_dir = "./rawdata",
  outdir = "./data/",
  emptycol = "Temp (\u00b0C)"
) {
  tryCatch(
    {
      remove_stale_files(station_id, rawdata_dir)
      remove_corrupt_files(station_id, rawdata_dir)
      download_station_csvs(station_id, start_year, end_year, rawdata_dir)
      build_parquet(station_id, emptycol, rawdata_dir, outdir)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
}

# ---------------------------------------------------------------------------
# Convenience: update a single station by ID using stationlist metadata
# ---------------------------------------------------------------------------

update_selected_station <- function(
  station_id,
  stationlist_path = "./stationlist.csv",
  rawdata_dir = "./rawdata",
  outdir = "./data/",
  emptycol = "Temp (\u00b0C)"
) {
  sl <- read_csv(stationlist_path, show_col_types = FALSE)
  row <- sl[sl$`Station ID` == as.integer(station_id), ]
  if (nrow(row) == 0L) {
    stop("Station ID not found in stationlist: ", station_id)
  }

  start_year <- row$`HLY First Year`[1]
  end_year <- row$`HLY Last Year`[1]
  if (is.na(start_year) || is.na(end_year)) {
    stop("Missing HLY year range for station ", station_id)
  }

  update_station(
    station_id,
    start_year,
    end_year,
    rawdata_dir,
    outdir,
    emptycol
  )
}

# ---------------------------------------------------------------------------
# Update stationlist.csv from ECCC inventory
# (replaces update_stationlist.R when called as a function)
# ---------------------------------------------------------------------------

refresh_stationlist <- function(stationlist_path = "./stationlist.csv") {
  inventory_url <- paste0(
    "https://collaboration.cmc.ec.gc.ca/cmc/climate/",
    "Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv"
  )

  year_cols <- c(
    "First Year",
    "Last Year",
    "HLY First Year",
    "HLY Last Year",
    "DLY First Year",
    "DLY Last Year",
    "MLY First Year",
    "MLY Last Year"
  )

  sl <- read_csv(stationlist_path, show_col_types = FALSE)
  inventory_tmp <- tempfile(fileext = ".csv")
  download.file(inventory_url, inventory_tmp, quiet = TRUE)
  inventory <- read_csv(inventory_tmp, skip = 3, show_col_types = FALSE)

  updates <- inventory |>
    filter(`Station ID` %in% sl$`Station ID`) |>
    select(`Station ID`, all_of(year_cols))

  updated <- sl |>
    select(-all_of(year_cols)) |>
    left_join(updates, by = "Station ID") |>
    select(all_of(colnames(sl)))

  write_csv(updated, stationlist_path)
  cat("stationlist.csv updated:", nrow(updates), "station(s) refreshed\n")
}

# ---------------------------------------------------------------------------
# Full pipeline: update all stations (replaces run.sh + helper.R)
# ---------------------------------------------------------------------------

update_all_stations <- function(
  stationlist_path = "./stationlist.csv",
  rawdata_dir = "./rawdata",
  outdir = "./data/",
  emptycol = "Temp (\u00b0C)",
  n_cores = max(1L, detectCores() - 1L),
  refresh_metadata = TRUE,
  progress_file = NULL
) {
  if (refresh_metadata) {
    cat("Refreshing station metadata...\n")
    tryCatch(
      refresh_stationlist(stationlist_path),
      error = function(e) {
        cat(
          "WARNING: Could not refresh stationlist:",
          conditionMessage(e),
          "\n"
        )
      }
    )
  }

  sl <- read_csv(stationlist_path, show_col_types = FALSE)
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }

  total <- nrow(sl)
  cat(sprintf("Updating %d stations using %d cores...\n", total, n_cores))

  # Write initial progress (0 / total)
  if (!is.null(progress_file)) {
    writeLines(paste0("0/", total), progress_file)
  }

  # File-based counter for progress reporting across parallel workers
  counter_file <- tempfile("pipeline_counter_")
  file.create(counter_file)

  errors <- mclapply(
    seq_len(total),
    function(i) {
      result <- update_station(
        station_id = sl$`Station ID`[i],
        start_year = sl$`HLY First Year`[i],
        end_year = sl$`HLY Last Year`[i],
        rawdata_dir = rawdata_dir,
        outdir = outdir,
        emptycol = emptycol
      )
      # Append a byte to counter file and report progress
      cat(".", file = counter_file, append = TRUE)
      completed <- file.size(counter_file)
      pct <- round(100 * completed / total, 1)
      cat(sprintf(
        "%d/%d (%.1f%%) stations updated\n",
        completed,
        total,
        pct
      ))
      if (!is.null(progress_file)) {
        cat(".", file = progress_file, append = TRUE)
      }
      result
    },
    mc.cores = n_cores,
    mc.preschedule = FALSE
  )

  unlink(counter_file)

  failed_idx <- which(!vapply(errors, is.null, logical(1)))
  if (length(failed_idx) > 0L) {
    cat(sprintf("WARNING: %d station(s) failed:\n", length(failed_idx)))
    for (i in failed_idx) {
      cat(sprintf("  %s: %s\n", sl$`Station ID`[i], errors[[i]]))
    }
  } else {
    cat(sprintf("Done. %d parquet files written to %s\n", total, outdir))
  }

  invisible(failed_idx)
}

# ---------------------------------------------------------------------------
# Standalone entry point — only runs when executed directly, not when sourced.
# Usage: Rscript pipeline.R
# ---------------------------------------------------------------------------

if (
  !interactive() &&
    identical(sys.nframe(), 0L)
) {
  update_all_stations()
}
