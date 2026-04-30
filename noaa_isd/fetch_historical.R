## fetch_historical.R
## One-time bulk pull of NOAA GHCN-Hourly for both Bangalore airport stations.
## Run once. For ongoing updates use update.R.
##
## Source: https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/
## Note: GHCN-H precipitation is empty for these METAR stations - temp/wind only.

library(tidyverse)
library(lubridate)

options(timeout = 600)  # NOAA PSVs can be 10+ MB

script_dir <- tryCatch({
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
}, error = function(e) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    getwd()
  }
})

stations <- list(
  HAL  = list(id = "INM00043296", first_year = 2005),
  VOBL = list(id = "INI0000VOBL", first_year = 2008)
)
this_year <- year(Sys.Date())

base_url <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year"

fetch_year <- function(station_label, station_id, yr) {
  url <- sprintf("%s/%d/psv/GHCNh_%s_%d.psv", base_url, yr, station_id, yr)
  message("  ", station_label, " ", yr, " ...")
  tmp <- tempfile(fileext = ".psv")
  ok <- tryCatch({
    download.file(url, tmp, quiet = TRUE, mode = "wb")
    TRUE
  }, error = function(e) { message("    skip (", e$message, ")"); FALSE })
  if (!ok || !file.exists(tmp) || file.size(tmp) < 1000) return(NULL)

  d <- read_delim(
    tmp, delim = "|", show_col_types = FALSE,
    col_select = c("STATION", "DATE", "temperature",
                   "wind_speed", "wind_direction", "relative_humidity"),
    col_types = cols(.default = col_guess(), DATE = col_datetime())
  )
  unlink(tmp)
  d %>%
    transmute(
      DT      = DATE,
      Station = station_label,
      Temp    = temperature,
      WindSpd = wind_speed,
      WindDir = wind_direction,
      RH      = relative_humidity
    )
}

all_rows <- map_dfr(names(stations), function(lab) {
  st <- stations[[lab]]
  message(lab, ": years ", st$first_year, "-", this_year)
  map_dfr(st$first_year:this_year, ~ fetch_year(lab, st$id, .x))
})

blrISD <- all_rows %>%
  filter(!is.na(DT)) %>%
  distinct(Station, DT, .keep_all = TRUE) %>%
  arrange(Station, DT)

save(blrISD, file = file.path(script_dir, "data", "bangaloreISD.RData"))

blrISD %>%
  summarise(
    n        = n(),
    first    = min(DT),
    last     = max(DT),
    pct_temp = round(mean(!is.na(Temp))   * 100, 1),
    pct_wind = round(mean(!is.na(WindSpd)) * 100, 1),
    .by = Station
  ) %>%
  print()

message("Saved: ", file.path(script_dir, "data", "bangaloreISD.RData"))
