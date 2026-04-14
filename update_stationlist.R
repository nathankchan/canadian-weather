# update_stationlist.R
# Downloads the ECCC Station Inventory and updates stationlist.csv in-place,
# refreshing year-range columns for the stations already listed.

library(tidyverse)

INVENTORY_URL <- "https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv"
STATIONLIST_PATH <- "./stationlist.csv"

year_cols <- c(
  "First Year", "Last Year",
  "HLY First Year", "HLY Last Year",
  "DLY First Year", "DLY Last Year",
  "MLY First Year", "MLY Last Year"
)

stationlist <- read_csv(STATIONLIST_PATH, show_col_types = FALSE)

inventory_tmp <- tempfile(fileext = ".csv")
download.file(INVENTORY_URL, inventory_tmp, quiet = TRUE)
inventory <- read_csv(inventory_tmp, skip = 3, show_col_types = FALSE)

updates <- inventory %>%
  filter(`Station ID` %in% stationlist$`Station ID`) %>%
  select(`Station ID`, all_of(year_cols))

updated <- stationlist %>%
  select(-all_of(year_cols)) %>%
  left_join(updates, by = "Station ID") %>%
  select(all_of(colnames(stationlist)))

write_csv(updated, STATIONLIST_PATH)
cat("stationlist.csv updated:", nrow(updates), "station(s) refreshed\n")
