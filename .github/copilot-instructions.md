# Copilot Instructions — Canadian Weather Shiny App

## Project Overview

Interactive Shiny web application for visualizing historical hourly weather data across all 13 Canadian provinces and territories. Data is sourced from Environment and Climate Change Canada (ECCC).

**Stack:** R 4.5.3, Shiny, tidyverse (incl. ggplot2), plotly, arrow, leaflet, DT, shinycssloaders, htmlwidgets, processx
**Environment:** renv (locked dependencies — use `renv::restore()` before running)

## Entry Points

- `app.R` — Shiny UI + server logic (reactive pipelines, plot rendering, interactive map, statistics table, data export, help toggle, source-data refresh)
- `pipeline.R` — Data pipeline: downloads raw CSVs, cleans corrupt/stale files, converts to Parquet. Can be sourced by `app.R` for in-app updates or run standalone via `Rscript pipeline.R`
- `project_libraries.R` — Declares dev-time dependencies (`lintr`, `languageserver`, `rsconnect`) for renv to track
- `stationlist.csv` — Metadata for all 960 stations; `"Station ID"` maps to `rawdata/` subdirectory names and output parquet filenames

## Data Flow

```
ECCC API → pipeline.R → rawdata/{StationID}/*.csv
    → pipeline.R (combine CSVs, remove empty rows, type-convert, write Parquet)
    → data/{StationID}.parquet
    → app.R (lazy Arrow dataset opened on station selection)
    → Shiny UI (Map / Surface Plot / Line Chart / Heat Map / Table)
```

One parquet file per station. `pipeline.R` processes all stations in parallel via `mclapply` (`update_all_stations()` → `update_station()` with `tryCatch`). Within each station, `combine_csv()` merges monthly CSVs, `remove_empty_rows()` drops rows with no temperature reading, and `type_convert()` coerces string columns before writing.

## Architecture Notes

### Data reactives

- `selected_data` reactive opens a lazy Arrow dataset (`open_dataset()`) for the active station; it does not load data into memory. `filtered_data` derives from it, applying date-range predicates before collecting — all plot and table reactives derive from `filtered_data`.
- `station_numeric_cols` reactive derives from `selected_data` and returns the names of all numeric columns (excluding those in `exclude_cols`: `Longitude (x)`, `Latitude (y)`, `Climate ID`, `Year`). It drives both the column selector and the statistics table.
- `full_stats_data` collects all numeric columns from `selected_data` (no date filter) for use in the statistics table full-dataset comparison.
- `full_date_range` reactive computes min/max `Date/Time (LST)` from the full station dataset; used by preset buttons, date input initialization, and export filenames.
- `station_id` reactive extracts the Station ID from the selected parquet filename (strips `.parquet`).
- `station_info` reactive returns a list with `province` and `name` for the selected station; used by export filename generation and plot titles.
- `stats_table` computes summary statistics (N, Missing, Min, Max, Range, Mean, Median, SD, IQR) for both the selected date range and the full dataset. `displaystats` supports two sort orders controlled by the `stats_grouping` radio button, highlights the currently selected variable in bold, and is visible only on plot tabs (hidden on Map and Table tabs via `conditionalPanel`).

### State management

- `current_dates` (`reactiveVal`) and `current_column` (`reactiveVal`) are the authoritative sources of the active date range and column. All plot/table reactives read from these rather than directly from `input$dates` or `input$selectedcolumn`, which avoids cascading invalidation when multiple inputs update simultaneously (e.g. on station change).
- Date range filtering is centralised in `filtered_data`. If extending, derive new reactives from `filtered_data` rather than re-filtering `selected_data` directly.
- On station change, the app **preserves** the current column if it exists in the new station and the current date range if it overlaps the new station's range. It falls back to `"Temp (°C)"` and a 2-year date range ending at the station's latest date only when preservation is not possible. `freezeReactiveValue()` is used to suppress cascading updates when resetting `selectedcolumn`, `dates`, and `date_slider` simultaneously.

### Plot data reactives

- `plot_columns` selects `Date/Time (LST)`, `Time (LST)`, and the active column from `filtered_data`.
- `plot_3dinput` pivots `plot_columns` into a wide matrix (rows = hours, columns = dates) for the 3D surface plot.
- `plot_lineinput` reformats `plot_columns` into a two-column `DateTime`/`Value` frame for the line chart.
- `plot_heatinput` reformats `plot_columns` into `Date`/`Hour`/`Value` for the heat map tile plot.

### UI features

- **Help text toggle** — `actionButton("toggle_help")` toggles a hidden `checkboxInput("showhelp")`; five `conditionalPanel` blocks key off `input.showhelp == true` to show/hide help text, z-axis hints, and the source attribution block.
- **Update source data** — Two buttons: `actionButton("update_station")` (single station) and `actionButton("update_all")` (all stations). Both launch `pipeline.R` via `processx::process$new()` asynchronously. `update_proc`, `update_result`, `update_target`, `update_progress_file`, and `update_total` reactiveVals track progress; `output$update_status` renders a spinner, progress count, or result checkmark/error. Both buttons are disabled during execution via a custom JS message handler (`setButtonDisabled`).
- **Preset date buttons** — Six buttons (2 wks, 2 mos, 2 yrs, 10 yrs, 30 yrs, Max) set the date range relative to the station's latest date. Buttons are dynamically disabled when the preset exceeds the station's date range. A collapsible "Custom" `<details>` section houses the `dateRangeInput` and `sliderInput` for manual entry; the two inputs sync bidirectionally.
- **Z-axis controls** — `autozaxis` radio button toggles manual z-axis limits (`zaxislimits` slider). `output$zaxistable` shows the actual min/max for the selected column and date range.

### Map

- The **Map tab** (`displaymap`) uses `leaflet` to show all stations colour-coded by province. Clicking a marker triggers a two-step selection: if the province dropdown already matches, `updateSelectInput` updates the station directly; if not, `pending_station` (a `reactiveVal`) stashes the intended parquet filename and the province observer applies it once the station choices have been repopulated.
- Province-zoom buttons (`map_zoom_{province}`) are generated dynamically via `lapply`. Additional buttons zoom to all stations or the selected station. Changing station also auto-zooms the map to the new station's coordinates.

### Exports and helpers

- Export handlers (`export_filtered_csv`, `export_filtered_parquet`, `export_full_csv`, `export_full_parquet`) produce filenames via `export_filename()` using a `to_camel_case()` transform on the province and station name. Filtered exports use `filtered_data()`; full exports collect from `selected_data()`.
- `expand_station_name()` normalises raw ECCC station name strings (e.g. expands abbreviations like `Int'l` → `International`, `A` → `Airport`, `Aut` → `Automatic`) and is applied once at startup when building `stationlist`.
- Helper functions in `app.R`: `compute_stats()`, `build_stats_group()`, `to_camel_case()`, `export_filename()`, `compute_xaxis()`, `expand_station_name()`. (`format_date_range()` and `format_plot_title()` are defined but currently unused.)

### Offline pipeline

- `pipeline.R` handles the full data pipeline (download, clean, convert to Parquet). It can be sourced by `app.R` for in-app updates or run standalone via `Rscript pipeline.R`.

## Code Style

- Use **tidyverse** idioms (`dplyr`, `readr`, `tidyr`, `purrr`) and **arrow** throughout.
- Pipe with `|>` (native pipe).
- Use `mclapply()` for parallel station processing in `pipeline.R` (see `update_all_stations`).
- Keep data transformations in `pipeline.R`; keep reactive/UI logic in `app.R`.
- Format R code with the **air** formatter (configured via `.vscode/settings.json`).
- Lint with **lintr** (configured via `.lintr`; `object_usage_linter` is disabled to suppress tidyverse/NSE false positives; line length limit is 100).

## SOLID Principles

These apply whenever writing or modifying R code in this project. R is primarily functional, so adapt each principle accordingly.

### Single Responsibility

Each function does exactly one thing. Do not combine data loading, filtering, and formatting in one function.

```r
# Bad — loads, filters, and formats in one block
get_plot_data <- function(station, col, dates) {
  data <- read_csv(...)
  data <- data[data$date >= dates[1], ]
  colnames(data) <- c("Date", "Value")
  data
}

# Good — each concern is separate
load_station_data <- function(station) read_csv(...)
filter_date_range <- function(data, dates) data[data$date >= dates[1] & data$date <= dates[2], ]
rename_for_plot <- function(data) { colnames(data) <- c("Date", "Value"); data }
```

### Open/Closed

Functions should be open for extension via parameters, not modified every time a new case arises. Avoid hardcoding values that are likely to vary (column names, file paths, thresholds).

```r
# Bad — hardcodes the temperature column
remove_empty_rows <- function(x) x[-which(is.na(x[, "Temp (°C)"])), ]

# Good — parameterised (as it already is in pipeline.R)
remove_empty_rows <- function(x, col) x[-which(is.na(x[, col])), ]
```

### Liskov Substitution

Applies when using R's OOP systems (S3, R6). A subclass or specialised implementation must be usable wherever the base type is expected, without breaking callers.

If you introduce an S3 class (e.g., a `WeatherStation` object), any function accepting a `WeatherStation` must work with all objects of that class — not just specific subtypes.

### Interface Segregation

Functions should not require arguments they don't use. Keep function signatures narrow. If a function only needs a column name, don't pass the entire data frame config.

```r
# Bad — forces callers to pass a full config list even when only one field is needed
get_station_id <- function(config) config$stationlist[[config$idcol]][...]

# Good — accepts only what it needs
get_station_id <- function(stationlist, namecol, idcol, nameval) ...
```

### Dependency Inversion

High-level orchestration functions should not depend on low-level details. Pass dependencies (file paths, column names, data sources) as arguments rather than hardcoding them inside orchestrators.

```r
# Bad — process_all_data hardcodes the column name
process_all_data <- function(station_ids) {
  mclapply(station_ids, get_station_data,
           emptycol = "Temp (°C)")
}

# Good — column names injected by the caller
process_all_data <- function(station_ids, emptycol) {
  mclapply(station_ids, get_station_data,
           emptycol = emptycol)
}
```

## When to Use OOP in R

R is functional by default. Prefer plain functions and data frames unless state management or polymorphism genuinely simplifies the design.

**Use OOP (R6) when:**

- An object needs to carry mutable state across multiple operations (e.g., a data pipeline object that accumulates results and tracks progress)
- You are modelling an entity with distinct lifecycle phases (initialise → load → process → export)
- You need method dispatch based on object type in a class hierarchy

**Use S3 when:**

- You need lightweight polymorphism on existing data structures (e.g., a `print.WeatherData` method, a `summary.StationReport` method)
- Extending generic functions (`print`, `summary`, `plot`) for custom classes

**Stick with plain functions when:**

- The operation is a pure transformation (input in, output out, no side effects)
- The function is used in a pipeline (`|>`) — this is most of `pipeline.R` and is correct
- The Shiny reactive graph handles all state (most of `app.R`)

### R6 Example Pattern (for reference if needed)

```r
library(R6)

WeatherDataLoader <- R6Class("WeatherDataLoader",
  public = list(
    station_ids = NULL,
    data        = NULL,

    initialize = function(station_ids) {
      self$station_ids <- station_ids
    },

    load = function(emptycol) {
      self$data <- lapply(self$station_ids, get_station_data, emptycol = emptycol)
      names(self$data) <- self$station_ids
      invisible(self)
    },

    export = function(outdir) {
      mapply(function(x, name) write_parquet(x, file.path(outdir, paste0(name, ".parquet"))),
             self$data, names(self$data))
      invisible(self)
    }
  )
)
```

Only introduce this pattern if the project grows to require it (e.g., multiple data sources, pluggable loaders, or caching strategies). The current functional design in `pipeline.R` is appropriate for the project's current scope.

## Key Caveats

- `pipeline.R` is sourced at the top level of `app.R` (`source("pipeline.R", local = TRUE)`) so its functions are available for in-app station updates.
- Remove commented-out test/debug code before committing.
- The `zaxislimits` slider range is computed from the full station data (not just the selected date range) — this is intentional for stable comparisons.
- `stationlist.csv` lists all stations; `"Station ID"` maps to `rawdata/` subdirectory names and `data/*.parquet` filenames.
