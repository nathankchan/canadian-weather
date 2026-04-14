# helper.R

library(tidyverse)
library(arrow)
library(parallel)

combine_csv <- function(dirname) {
  data_list <- mapply(
    function(x) {
      read_csv(
        x,
        col_types = cols(.default = col_character()),
        show_col_types = FALSE
      )
    },
    paste0(dirname, list.files(dirname)),
    SIMPLIFY = FALSE
  )
  bind_rows(data_list)
}

remove_empty_rows <- function(x, col) {
  x[-which(is.na(x[, col])), ]
}

get_station_data <- function(id, emptycol) {
  combine_csv(dirname = paste0("./rawdata/", id, "/")) |>
    remove_empty_rows(x = _, col = emptycol)
}

process_and_write <- function(id, emptycol, outdir) {
  tryCatch(
    {
      data <- get_station_data(id, emptycol)
      write_parquet(
        x = type_convert(data),
        sink = paste0(outdir, id, ".parquet")
      )
      NULL
    },
    error = function(e) conditionMessage(e)
  )
}

process_all_stations <- function(station_ids, emptycol, outdir, n_cores) {
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }

  cat(sprintf(
    "Processing %d stations using %d cores...\n",
    length(station_ids), n_cores
  ))

  errors <- mclapply(
    station_ids,
    process_and_write,
    emptycol = emptycol,
    outdir = outdir,
    mc.cores = n_cores,
    mc.preschedule = FALSE
  )

  failed <- station_ids[!vapply(errors, is.null, logical(1))]
  if (length(failed) > 0) {
    cat(sprintf("WARNING: %d station(s) failed:\n", length(failed)))
    for (id in failed) {
      cat(sprintf("  %s: %s\n", id, errors[[which(station_ids == id)]]))
    }
  } else {
    cat(sprintf(
      "Done. %d parquet files written to %s\n",
      length(station_ids), outdir
    ))
  }
}

stationlist <- read_csv(file = "./stationlist.csv", show_col_types = FALSE)
station_ids <- stationlist$`Station ID`
n_cores <- max(1L, detectCores() - 1L)

process_all_stations(
  station_ids = station_ids,
  emptycol = "Temp (°C)",
  outdir = "./data/",
  n_cores = n_cores
)
