## update.R
## Delta refresh: re-pull current year (and prior year for safety)
## for both stations, dedupe, append.

library(tidyverse)
library(lubridate)

options(timeout = 600)

script_dir <- tryCatch({
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
}, error = function(e) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    getwd()
  }
})

data_file <- file.path(script_dir, "data", "bangaloreISD.RData")
if (!file.exists(data_file)) stop("No data file. Run fetch_historical.R first.")
load(data_file)

stations <- list(
  HAL  = "INM00043296",
  VOBL = "INI0000VOBL"
)
this_year <- year(Sys.Date())
last_year <- this_year - 1L
years <- c(last_year, this_year)

base_url <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year"

fetch_year <- function(station_label, station_id, yr) {
  url <- sprintf("%s/%d/psv/GHCNh_%s_%d.psv", base_url, yr, station_id, yr)
  message("  ", station_label, " ", yr)
  tmp <- tempfile(fileext = ".psv")
  ok <- tryCatch({
    download.file(url, tmp, quiet = TRUE, mode = "wb"); TRUE
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

new_rows <- map_dfr(names(stations), function(lab) {
  map_dfr(years, ~ fetch_year(lab, stations[[lab]], .x))
}) %>% filter(!is.na(DT))

before_n <- nrow(blrISD)
blrISD <- blrISD %>%
  filter(year(DT) < last_year) %>%
  bind_rows(new_rows) %>%
  distinct(Station, DT, .keep_all = TRUE) %>%
  arrange(Station, DT)
after_n <- nrow(blrISD)

save(blrISD, file = data_file)

message("Rows before: ", before_n, "  after: ", after_n,
        "  delta: ", after_n - before_n)
blrISD %>%
  summarise(last = max(DT), .by = Station) %>%
  mutate(s = paste0(Station, "=", format(last))) %>%
  pull(s) %>% paste(collapse = ", ") %>% message("Latest obs: ", .)
