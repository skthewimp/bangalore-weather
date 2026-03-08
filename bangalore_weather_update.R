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


blrTemp %>%
  mutate(
    DT = as.Date(DT), 
    Month = month(DT),
    Day = day(DT),
    Temp = as.numeric(Temp)
  ) %>%
  mutate(
    High = max(Temp),
    Low = min(Temp),
    .by = DT
  ) %>%
  summarise(
    RecordHigh = max(Temp),
    RecordLow = min(Temp),
    NormalHigh = median(High),
    NormalLow = median(Low), 
    High2022 = max(Temp[year(DT) == curr_year]),
    Low2022 = min(Temp[year(DT) == curr_year]),
    .by = c(Month,Day)
  ) %>%
  mutate(
    Date = make_date(curr_year, Month, Day),
    High2022 = ifelse(is.infinite(High2022),NormalHigh, High2022),
    Low2022 = ifelse(is.infinite(Low2022),NormalHigh, Low2022), 
    Special = case_when(
      High2022 >= RecordHigh ~ paste("Hottest", format(Date, "%b-%d"), "since 1980"),
      Low2022 <= RecordLow ~ paste("Coldest", format(Date, "%b-%d"), "since 1980"),
      .default = ""
    )
  ) %>%
  ggplot() + 
  geom_segment(aes(x = Date, xend = Date, y = RecordLow, yend = RecordHigh), linewidth = 1, col = "#d4cbaa")+ 
  geom_segment(aes(x = Date, xend = Date, y = NormalLow, yend = NormalHigh), linewidth = 1, col = '#888888') + 
  geom_segment(aes(x = Date, xend = Date, y = Low2022, yend = High2022), linewidth = 1, col = "#490000",alpha = 0.9) + 
  ggrepel::geom_text_repel(aes(x = Date, y = NormalHigh, label = str_wrap(Special, 10)), size = 2.5, direction = 'y')  +
  scale_x_date("", lim = c(floor_date(Sys.Date(), '1 year'), ceiling_date(Sys.Date(), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), date_labels = '%B', position = 'top', expand = expansion(mult = 0)) + 
  scale_y_continuous("", breaks = seq(10, 42, 4) )  +
  ggthemes::theme_tufte()  + 
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold'), axis.line.y = element_line(colour = 'black', linewidth = 0.2),  panel.background = element_rect(fill = rgb(0.85, 0.85, 0.75), linewidth = 0), plot.background = element_rect(fill = rgb(0.85, 0.85, 0.75)) ) +
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 38, label = "Temperature", hjust = 0, fontface = 'bold', size = 3) + 
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 37.5, label = str_wrap("Brown bars represent range between the daily high and low", 40), hjust = 0, vjust = 1, size = 2.5) + 
  annotate("text", x = as.Date(paste0(curr_year, '-09-05')), y = 14, label = str_wrap("Dark grey bars show normal range; Beige show record range", 40), size = 2.5) ->
  tempPlot


blrRain %>%
  mutate(
    Rain = as.numeric(Rain),
    DT = as.Date(DT)
  ) %>%
  summarise(
    Rain = sum(Rain),
    .by = DT
  ) %>%
  mutate(
    Month = month(DT),
    Day = day(DT), 
    MonthYear = floor_date(DT, '1 month')
  ) %>%
  mutate(
    MonthlyRain = sum(Rain),
    .by = MonthYear
  ) %>%
  mutate(
    DailyAvg = mean(Rain),
    DailyMax = max(Rain),
    .by = c(Month, Day)
  ) %>%
  mutate(
    MonthlyAvg = mean(MonthlyRain),
    MonthlyMax = max(MonthlyRain), 
    .by = Month
  ) %>%
  filter(year(DT) == curr_year) %>%
  arrange(DT) %>%
  mutate(
    CumulRain = cumsum(Rain), 
    CumulAvg = cumsum(DailyAvg),
    .by = Month
  ) %>%
  mutate(
    normalLabel = case_when(
      Day == 1 & Month == 10 ~ paste("Normal", round(MonthlyAvg, 0), sep = '\n'), 
      Day == 1 ~ as.character(round(MonthlyAvg, 0)),
      .default = ""
    ),
    actualLabel = case_when(
      Day == max(Day) & Month == 10 ~ paste("Actual", round(CumulRain, 0), sep = '\n'), 
      Day == max(Day) ~ as.character(round(CumulRain, 0)),
      .default = ""
    ),
    MonthLabel = ifelse(CumulRain == MonthlyMax & Day == 15, paste("Wettest", month.name[Month], "since 1981"), ""),
    .by = Month
  ) %>%
  mutate(
    DayLabel = ifelse(Rain == DailyMax, paste0("Record ", month.name[Month], "-", Day, "; ", round(Rain, 0), " mm"), ""),
    .by = c(Month, Day)
  ) %>%
  mutate(
    Label = case_when(
      DayLabel != "" ~ DayLabel, 
      MonthLabel != "" ~ MonthLabel, 
      .default = ""
    )
  ) %>%
  ggplot(aes(x = DT)) + 
  geom_segment(aes(x = DT, xend = DT, y = CumulRain - Rain, yend = CumulRain), linewidth = 1, col = '#490000') + 
  #geom_area(aes(y = CumulRain, group = Month), lwd = 1, col = '#005566', fill = '#d4cbaa', alpha = 0.4) +
  geom_step(aes(y = MonthlyAvg, group = Month), lwd = 1, col = 'darkgreen') + 
  geom_text(aes(y = MonthlyAvg, label = normalLabel), vjust = -0.05, hjust = 0, size = 2.5, fontface = 'bold') + 
  geom_text(aes(y = CumulRain, label = actualLabel), vjust = -0.05, hjust = 1, size = 2.5, fontface = 'bold') + 
  ggrepel::geom_text_repel(aes(y = CumulRain, label = str_wrap(Label, 10)), size = 2,  fontface = 'bold') +
  scale_x_date("", lim = c(floor_date(Sys.Date(), '1 year'), ceiling_date(Sys.Date(), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), date_labels = '%B', expand = expansion(mult = 0)) + 
  ggthemes::theme_tufte() + 
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold'), axis.line.y = element_line(colour = 'black', linewidth = 0.2), panel.background = element_rect(fill = rgb(0.85, 0.85, 0.75), linewidth = 0), plot.background = element_rect(fill = rgb(0.85, 0.85, 0.75)) ) + 
  scale_y_continuous("", breaks = seq(0,500, 50)) + 
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 260, label = "Precipitation", hjust = 0, fontface = 'bold', size = 3) + 
  annotate("text", x = as.Date(paste0(curr_year, '-02-05')), y = 260, label = str_wrap("Cumulative monthly precipitation in mm compared to normal monthly precipitation", 1000), hjust = 0,  size = 2.5)  ->
  rainPlot

tempPlot + rainPlot +
  plot_layout(ncol = 1, heights = c(70, 30)) +
  plot_annotation(
    title = paste("Bangalore's Weather in", curr_year),
    subtitle = commentary,
    caption = "Data source: Oikokab"
  ) &
  theme(
    plot.title = element_text(face = 'bold', hjust = 0),
    plot.subtitle = element_text(size = 8, color = "#490000", hjust = 0, margin = margin(t = 2, b = 4)),
    panel.background = element_rect(fill = "#eae4db"),
    plot.background = element_rect(fill = "#eae4db", linewidth = 0)
  ) 



