# helper.R

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(shiny)

combine_csv <- function(dirname) {
  data_list <- mapply(
    function(x) {
      read_csv(x, col_types = cols(.default = col_character()), show_col_types = FALSE)
    },
    paste0(dirname, list.files(dirname)),
    SIMPLIFY = FALSE
  )
  data_combined <- bind_rows(data_list)
  return(data_combined)
}

remove_empty_rows <- function(x, col) {
  data_in <- x[-which(is.na(x[, col])),]
  return(data_in)
}

combine_stations <- function(x, y, col) {
  
  if ( as.POSIXct(as.character(x[[col]][1])) > as.POSIXct(as.character(y[[col]][1])) ) {
    oldindex <- y[[col]]
    olddata <- y
    newindex <- x[[col]]
    newdata <- x
  } else {
    oldindex <- x[[col]]
    olddata <- x
    newindex <- y[[col]]
    newdata <- y
  }
  
  shareddata <- intersect(oldindex, newindex)
  oldindex_drop <- which(oldindex %in% shareddata)
  
  olddata <- olddata[-oldindex_drop, ]
  
  out <- bind_rows(olddata, newdata)
  
  return(out)
  
}

get_station_data <- function(ids, emptycol, combinecol) {
  
  station1 <- 
    combine_csv(dirname = paste0("./rawdata/", ids[1], "/")) %>% 
    remove_empty_rows(x = ., col = emptycol)
  
  station2 <- 
    combine_csv(dirname = paste0("./rawdata/", ids[2], "/")) %>% 
    remove_empty_rows(x = ., col = emptycol)
  
  out <- 
    combine_stations(
      x = station1, 
      y = station2, 
      col = combinecol)
  
  return(out)
  
}

get_station_id <- function(stationlist, namecol, idcol, nameval) {
  out <- stationlist[[idcol]][which(stationlist[, namecol] == nameval)]
  return(out)
}
get_station_ids <- 
  Vectorize(get_station_id, vectorize.args = "nameval", SIMPLIFY = FALSE)


process_all_data <- function(station_ids, emptycol, combinecol) {
  
  out <-
    mapply(
      get_station_data,
      station_ids,
      MoreArgs = list(
        emptycol = emptycol,
        combinecol = combinecol)
      ,
      SIMPLIFY = FALSE)
  
  return(out)
}

stationlist <- read_csv(file = "./stationlist.csv", show_col_types = FALSE)
station_ids <- get_station_ids(stationlist, "Province", "Station ID", unique(stationlist$Province))

station_data <- 
  process_all_data(
    station_ids = station_ids,
    emptycol = "Temp (°C)",
    combinecol = "Date/Time (LST)"
  )

if (!dir.exists("data")) {dir.create("data")}
invisible(mapply(
  FUN = function(x, filename) {
    write_csv(x = x,
              file = paste0("data/", filename, ".csv.gz"))
  },
  x = station_data,
  filename = names(station_data),
  SIMPLIFY = FALSE
))

rm(station_data)
station_data <- mapply(
  read_csv,
  paste0("./data/", list.files(path = "./data/", pattern = "*.csv.gz")),
  MoreArgs = list(show_col_types = FALSE),
  SIMPLIFY = FALSE
)
names(station_data) <- names(station_data) %>% basename %>% substr(., 1, nchar(.) - 7)

saveRDS(object = station_data, file = "data/station_data.rds")


# Old code for testing purposes...

# data1 <- combine_csv(
#   dirname = paste0("./rawdata/", stationlist$Station.ID[15], "/")
# )
# data2 <- combine_csv(
#   dirname = paste0("./rawdata/", stationlist$Station.ID[16], "/")
# )
# 
# data3 <- remove_empty_rows(data1, "Temp (°C)")
# data4 <- remove_empty_rows(data2, "Temp (°C)")
# 
# data5 <- combine_stations(
#   x = data3,
#   y = data4,
#   col = "Date/Time (LST)"
# )
# data5


