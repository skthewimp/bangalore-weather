library(tidytable)
library(tidyverse)
library(lubridate)
library(httr2)
library(jsonlite)

script_dir <- tryCatch({
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
}, error = function(e) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(sub("--file=", "", file_arg))
  } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    getwd()
  }
})

primKey <- Sys.getenv("OIKOLAB_PRIMARY")
if (primKey == "") {
  message("OIKOLAB_PRIMARY not found in env, using secondary or stopping")
  primKey <- Sys.getenv("OIKOLAB_SECONDARY")
}

startDate <- "1981-01-01T00:00:00"
endDate <- paste0(Sys.Date(), 'T00:00:00')
bloreLat <- 12.9716
bloreLon <- 77.5946

message("Downloading historical wind data in chunks to avoid API limits...")

fetch_chunk <- function(start, end) {
  message(sprintf("Fetching from %s to %s...", start, end))
  url <- paste0("https://api.oikolab.com/weather?start=", start, "&end=",end,"&param=wind_speed&freq=H&lat=",bloreLat,"&lon=",bloreLon,"&api-key=",primKey)
  tmp <- tempfile()
  res <- tryCatch({
    download.file(url, tmp, quiet = TRUE)
    w1 <- jsonlite::fromJSON(tmp)
    w2 <- jsonlite::fromJSON(w1$data)
    
    w2$data %>%
      as_tibble() %>%
      set_names(c("Latlong", "Source", "Something", "SomethingElse", "Wind")) %>%
      mutate(
        Index = w2$index,
        DT = as.POSIXct(Index, origin='1970-01-01')
      )
  }, error = function(e) {
    message("Error fetching chunk: ", e$message)
    NULL
  })
  res
}

# 1981-01-01 to 2000-12-31
blrWind1 <- fetch_chunk("1981-01-01T00:00:00", "2000-12-31T23:59:59")
# 2001-01-01 to 2020-12-31
blrWind2 <- fetch_chunk("2001-01-01T00:00:00", "2020-12-31T23:59:59")
# 2021-01-01 to present
blrWind3 <- fetch_chunk("2021-01-01T00:00:00", paste0(Sys.Date(), 'T00:00:00'))

blrWind <- bind_rows(blrWind1, blrWind2, blrWind3) %>% distinct(DT, .keep_all = TRUE)

save(blrWind, file=file.path(script_dir, 'data', 'bangaloreWind.RData'))

message("Successfully saved historical wind data to ", file.path(script_dir, 'data', 'bangaloreWind.RData'))
message("Total rows: ", nrow(blrWind))
