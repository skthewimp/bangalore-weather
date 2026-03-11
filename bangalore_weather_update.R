library(tidytable)
library(tidyverse)
library(patchwork)
library(yaml)
library(rvest)
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

source(file.path(script_dir, 'bangalore_weather_historical.R'))

primKey <- Sys.getenv("OIKOLAB_PRIMARY")
secKey <- Sys.getenv("OIKOLAB_SECONDARY")
load(file.path(script_dir, 'data', 'bangaloreRainfall.RData'))
load(file.path(script_dir, 'data', 'bangaloreTemperature.RData'))

blrTemp %>%
  filter(!is.na(Temp)) ->
  blrTemp

blrRain %>%
  filter(!is.na(Rain)) -> 
  blrRain

startDate <-  str_sub(max(blrTemp$DT), 1, 19) %>% str_replace_all(" ", "T")
endDate <- paste0(Sys.Date(), 'T00:00:00')

bloreLat <- 12.9716
bloreLon <- 77.5946

if (as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%S") < as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%S")) {
  url <- paste0("https://api.oikolab.com/weather?start=", startDate, "&end=",endDate,"&param=temperature&freq=H&lat=",bloreLat,"&lon=",bloreLon,"&api-key=",primKey)

  tmp <- tempfile()
  download.file(url, tmp)

  b1 <- jsonlite::fromJSON(tmp)
  b2 <- jsonlite::fromJSON(b1$data)
  b2$data %>%
    as_tibble() %>%
    set_names(c("Latlong", "Source", "Something", "SomethingElse", "Temp")) %>%
    mutate(
      Index = b2$index,
      DT= as.POSIXct(Index, origin='1970-01-01')
    ) ->
    blrTempNew

  url <- paste0("https://api.oikolab.com/weather?start=", startDate, "&end=",endDate,"&param=total_precipitation&freq=H&lat=",bloreLat,"&lon=",bloreLon,"&api-key=",primKey)
  tmp <- tempfile()
  download.file(url, tmp)
  r1 <- jsonlite::fromJSON(tmp)
  r2 <- jsonlite::fromJSON(r1$data)
  r2$data %>%
    as_tibble() %>%
    set_names(c("Latlong", "Source", "Something", "SomethingElse", "Rain")) %>%
    mutate(
      Index = r2$index,
      DT=as.POSIXct(Index, origin='1970-01-01')
    ) ->
    blrRainNew

  blrRain %>%
    bind_rows(blrRainNew) ->
    blrRain
  blrTemp %>%
    bind_rows(blrTempNew) ->
    blrTemp

  save(blrTemp, file=file.path(script_dir, 'data', 'bangaloreTemperature.RData'))
  save(blrRain, file=file.path(script_dir, 'data', 'bangaloreRainfall.RData'))
} else {
  message("Data already up to date, skipping fetch.")
}

curr_year <- max(year(blrTemp$DT))

# --- Recent 2-week commentary ---
recent_window <- 14
cutoff_date <- Sys.Date() - recent_window

temp_daily <- blrTemp %>%
  mutate(DT = as.Date(DT), Temp = as.numeric(Temp)) %>%
  summarise(High = max(Temp), Low = min(Temp), .by = DT) %>%
  mutate(Month = month(DT), Day = day(DT))

rain_daily <- blrRain %>%
  mutate(DT = as.Date(DT), Rain = as.numeric(Rain)) %>%
  summarise(Rain = sum(Rain), .by = DT) %>%
  mutate(Month = month(DT), Day = day(DT))

recent_temp <- temp_daily %>% filter(DT >= cutoff_date)
recent_rain <- rain_daily %>% filter(DT >= cutoff_date)
hist_temp <- temp_daily %>% filter(DT < as.Date(paste0(curr_year, "-01-01")))
hist_rain <- rain_daily %>% filter(DT < as.Date(paste0(curr_year, "-01-01")))

# Normal ranges for the same calendar days
recent_cal <- recent_temp %>% distinct(Month, Day)
normal_temp <- hist_temp %>%
  inner_join(recent_cal, by = c("Month", "Day")) %>%
  summarise(NormalHigh = mean(High), NormalLow = mean(Low))
normal_rain <- hist_rain %>%
  inner_join(recent_cal, by = c("Month", "Day")) %>%
  summarise(NormalRain = sum(Rain) / n_distinct(year(DT)))

hottest <- recent_temp %>% slice_max(High, n = 1, with_ties = FALSE)
coldest <- recent_temp %>% slice_min(Low, n = 1, with_ties = FALSE)
total_rain <- sum(recent_rain$Rain)
rainy_days <- sum(recent_rain$Rain >= 0.1)

# Record-breaking days in the window
hist_records <- hist_temp %>%
  summarise(RecHigh = max(High), RecLow = min(Low), .by = c(Month, Day))
record_days <- recent_temp %>%
  left_join(hist_records, by = c("Month", "Day")) %>%
  filter(!is.na(RecHigh) & (High > RecHigh | Low < RecLow)) %>%
  nrow()

recent_facts <- paste0(
  "Period: last ", recent_window, " days ending ", format(Sys.Date(), "%b %d, %Y"), "\n",
  "Avg daily high: ", round(mean(recent_temp$High), 1), "\u00B0C (normal for these dates: ", round(normal_temp$NormalHigh, 1), "\u00B0C)\n",
  "Avg daily low: ", round(mean(recent_temp$Low), 1), "\u00B0C (normal: ", round(normal_temp$NormalLow, 1), "\u00B0C)\n",
  "Hottest day: ", round(hottest$High, 1), "\u00B0C on ", format(hottest$DT, "%b %d"), "\n",
  "Coldest night: ", round(coldest$Low, 1), "\u00B0C on ", format(coldest$DT, "%b %d"), "\n",
  "Total rainfall: ", round(total_rain, 0), "mm (normal for these dates: ~", round(normal_rain$NormalRain, 0), "mm)\n",
  "Rainy days: ", rainy_days, "\n",
  if (record_days > 0) paste0(record_days, " days broke all-time records (since 1981) for their calendar date\n") else ""
)

commentary <- tryCatch({
  body <- list(
    model = "claude-haiku-4-5-20251001",
    max_tokens = 200,
    system = paste0(
      "You are writing a terse chart subtitle for a Bangalore weather visualization. ",
      "You receive weather stats for the last ~2 weeks compared to 40-year historical norms. ",
      "Write exactly 3 bullets that a Bangalore resident would find interesting. ",
      "Focus on what is unusual or noteworthy - not just restating numbers. ",
      "Compare to what is normal for this time of year. Mention if it has been warmer/cooler/drier/wetter than usual and by how much. ",
      "Each bullet must be under 15 words, start with '\u2022 ', and use \u00B0C. ",
      "Plain, calm tone. No hyperbole. Output only the 3 bullets, nothing else."
    ),
    messages = list(
      list(role = "user", content = paste0("Bangalore weather stats:\n", recent_facts))
    )
  )
  resp <- request("https://api.anthropic.com/v1/messages") %>%
    req_headers(
      `x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"),
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json"
    ) %>%
    req_body_json(body) %>%
    req_timeout(30) %>%
    req_error(is_error = ~ FALSE) %>%
    req_perform()
  if (resp_status(resp) != 200) {
    message("Claude API returned status ", resp_status(resp), ". Skipping commentary.")
    NULL
  } else {
    raw <- resp %>% resp_body_json() %>% .$content %>% .[[1]] %>% .$text
    lines <- strsplit(raw, "\n")[[1]]
    bullet_lines <- lines[grepl("^\u2022", trimws(lines))]
    if (length(bullet_lines) == 0) NULL
    else paste(trimws(bullet_lines), collapse = "\n")
  }
}, error = function(e) { message("Claude API call failed: ", e$message); NULL })


generate_weather_chart(curr_year, commentary = commentary)

