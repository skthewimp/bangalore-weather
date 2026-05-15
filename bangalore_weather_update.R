library(tidytable)
library(tidyverse)
library(shadowtext)
library(ggtext)
library(yaml)
library(rvest)
library(lubridate)
library(httr2)
library(jsonlite)
library(patchwork)

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
load(file.path(script_dir, 'data', 'bangaloreWind.RData'))

blrTemp %>%
  filter(!is.na(Temp)) ->
  blrTemp

blrRain %>%
  filter(!is.na(Rain)) -> 
  blrRain

blrWind %>%
  filter(!is.na(Wind)) ->
  blrWind

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

  url <- paste0("https://api.oikolab.com/weather?start=", startDate, "&end=",endDate,"&param=wind_speed&freq=H&lat=",bloreLat,"&lon=",bloreLon,"&api-key=",primKey)
  tmp <- tempfile()
  download.file(url, tmp)
  w1 <- jsonlite::fromJSON(tmp)
  w2 <- jsonlite::fromJSON(w1$data)
  w2$data %>%
    as_tibble() %>%
    set_names(c("Latlong", "Source", "Something", "SomethingElse", "Wind")) %>%
    mutate(
      Index = w2$index,
      DT=as.POSIXct(Index, origin='1970-01-01')
    ) ->
    blrWindNew

  url <- paste0("https://api.oikolab.com/weather?start=", startDate, "&end=",endDate,"&param=wind_direction&freq=H&lat=",bloreLat,"&lon=",bloreLon,"&api-key=",primKey)
  tmp <- tempfile()
  download.file(url, tmp)
  wd1 <- jsonlite::fromJSON(tmp)
  wd2 <- jsonlite::fromJSON(wd1$data)
  wd2$data %>%
    as_tibble() %>%
    set_names(c("Latlong", "Source", "Something", "SomethingElse", "WindDir")) %>%
    mutate(
      Index = wd2$index,
      DT=as.POSIXct(Index, origin='1970-01-01')
    ) %>%
    select(DT, WindDir) ->
    blrWindDirNew

  blrWindNew <- blrWindNew %>% left_join(blrWindDirNew, by = "DT")

  blrWind %>%
    bind_rows(blrWindNew) ->
    blrWind

  save(blrTemp, file=file.path(script_dir, 'data', 'bangaloreTemperature.RData'))
  save(blrRain, file=file.path(script_dir, 'data', 'bangaloreRainfall.RData'))
  save(blrWind, file=file.path(script_dir, 'data', 'bangaloreWind.RData'))
} else {
  message("Data already up to date, skipping fetch.")
}

curr_year <- max(year(blrTemp$DT))

temp_daily <- blrTemp %>%
  mutate(DT = as.Date(DT), Temp = as.numeric(Temp)) %>%
  filter(DT < Sys.Date()) %>%
  summarise(High = max(Temp), Low = min(Temp), .by = DT) %>%
  mutate(Month = month(DT), Day = day(DT))

rain_daily <- blrRain %>%
  mutate(DT = as.Date(DT), Rain = as.numeric(Rain)) %>%
  filter(DT < Sys.Date()) %>%
  summarise(Rain = sum(Rain), .by = DT) %>%
  mutate(Month = month(DT), Day = day(DT))

longest_run <- function(flags, dates) {
  flags <- replace(flags, is.na(flags), FALSE)
  if (!any(flags)) return(NULL)
  r <- rle(flags)
  is_t <- which(r$values)
  best <- is_t[which.max(r$lengths[is_t])]
  end_pos <- sum(r$lengths[seq_len(best)])
  start_pos <- end_pos - r$lengths[best] + 1
  list(len = r$lengths[best], start = dates[start_pos], end = dates[end_pos])
}

fmt_run <- function(label, run) {
  if (is.null(run) || run$len < 2) return(NULL)
  paste0(label, ": ", run$len, " days (", format(run$start, "%b %d"), " - ",
         format(run$end, "%b %d"), ")")
}

build_weather_window_text <- function(start_date, end_date, temp_daily, rain_daily) {
  anchor_year <- year(end_date)
  hist_cutoff <- as.Date(paste0(anchor_year, "-01-01"))
  recent_temp <- temp_daily %>% filter(DT >= start_date, DT <= end_date)
  recent_rain <- rain_daily %>% filter(DT >= start_date, DT <= end_date)
  hist_temp <- temp_daily %>% filter(DT < hist_cutoff)
  hist_rain <- rain_daily %>% filter(DT < hist_cutoff)

  if (nrow(recent_temp) == 0 || nrow(hist_temp) == 0 || nrow(hist_rain) == 0) {
    return(NULL)
  }

  norms_temp_by_day <- hist_temp %>%
    summarise(NormalHigh = mean(High), NormalLow = mean(Low), .by = c(Month, Day))
  norms_rain_by_day <- hist_rain %>%
    summarise(NormalRain = sum(Rain) / n_distinct(year(DT)), .by = c(Month, Day))

  window_daily <- recent_temp %>%
    left_join(recent_rain %>% select(DT, Rain), by = "DT") %>%
    mutate(Rain = coalesce(Rain, 0)) %>%
    left_join(norms_temp_by_day, by = c("Month", "Day")) %>%
    left_join(norms_rain_by_day, by = c("Month", "Day")) %>%
    arrange(DT) %>%
    mutate(
      HighDev = High - NormalHigh,
      LowDev = Low - NormalLow,
      RainDev = Rain - NormalRain
    )

  window_n <- nrow(window_daily)
  total_rain <- sum(window_daily$Rain)
  rainy_days <- sum(window_daily$Rain >= 0.1)
  normal_total_rain <- sum(window_daily$NormalRain)
  normal_rainy_days <- mean(window_daily$NormalRain >= 0.1) * window_n

  daily_table <- window_daily %>%
    mutate(line = paste0(
      format(DT, "%b %d"),
      "  high ", sprintf("%.1f", High), "°C (norm ", sprintf("%.1f", NormalHigh),
      ", ", ifelse(HighDev >= 0, "+", ""), sprintf("%.1f", HighDev), ")",
      "  low ", sprintf("%.1f", Low), "°C (norm ", sprintf("%.1f", NormalLow),
      ", ", ifelse(LowDev >= 0, "+", ""), sprintf("%.1f", LowDev), ")",
      "  rain ", sprintf("%.1f", Rain), "mm (norm ", sprintf("%.1f", NormalRain), ")"
    )) %>%
    pull(line) %>%
    paste(collapse = "\n")

  roll_max_rain <- function(k) {
    n <- nrow(window_daily)
    if (n < k) return(NULL)
    sums <- map_dbl(seq_len(n - k + 1), ~ sum(window_daily$Rain[.x:(.x + k - 1)]))
    i <- which.max(sums)
    if (sums[i] < 0.1) return(NULL)
    paste0("Wettest ", k, "-day stretch: ", sprintf("%.1f", sums[i]), "mm (",
           format(window_daily$DT[i], "%b %d"), " - ",
           format(window_daily$DT[i + k - 1], "%b %d"), ")")
  }

  rolling_text <- c(roll_max_rain(2), roll_max_rain(3), roll_max_rain(5)) %>%
    discard(is.null) %>%
    paste(collapse = "\n")

  streaks_text <- c(
    fmt_run("Dry streak in window (rain <0.1mm)",             longest_run(window_daily$Rain < 0.1, window_daily$DT)),
    fmt_run("Wet streak in window (rain >=0.1mm)",            longest_run(window_daily$Rain >= 0.1, window_daily$DT)),
    fmt_run("Hot-day streak (high >=2°C above normal)",       longest_run(window_daily$HighDev >= 2, window_daily$DT)),
    fmt_run("Cool-day streak (high >=2°C below normal)",      longest_run(window_daily$HighDev <= -2, window_daily$DT)),
    fmt_run("Warm-night streak (low >=2°C above normal)",     longest_run(window_daily$LowDev >= 2, window_daily$DT)),
    fmt_run("Cool-night streak (low >=2°C below normal)",     longest_run(window_daily$LowDev <= -2, window_daily$DT))
  ) %>% discard(is.null) %>% paste(collapse = "\n")

  ante_streak <- function(opposite_in_window, threshold_fn, label) {
    hits <- which(opposite_in_window)
    antecedent_end <- if (length(hits)) window_daily$DT[hits[1]] - 1 else end_date
    run_n <- rain_daily %>%
      filter(DT <= antecedent_end) %>% arrange(desc(DT)) %>%
      mutate(stop = !threshold_fn(Rain), cum = cumsum(stop)) %>%
      filter(cum == 0) %>% nrow()
    if (run_n < 3) return(NULL)
    paste0(label, ": ", run_n, " days ending ", format(antecedent_end, "%b %d"))
  }

  antecedent_text <- c(
    ante_streak(window_daily$Rain >= 0.1, function(r) r < 0.1,  "Dry streak preceding first rain in window"),
    ante_streak(window_daily$Rain < 0.1,  function(r) r >= 0.1, "Wet streak preceding first dry day in window")
  ) %>% discard(is.null) %>% paste(collapse = "\n")

  hist_records <- hist_temp %>%
    summarise(RecHigh = max(High), RecLow = min(Low), .by = c(Month, Day))

  record_detail <- recent_temp %>%
    left_join(hist_records, by = c("Month", "Day")) %>%
    filter(!is.na(RecHigh) & (High > RecHigh | Low < RecLow)) %>%
    mutate(
      detail = case_when(
        High > RecHigh ~ paste0(format(DT, "%b %d"), ": high of ", round(High, 1), "\u00B0C broke record of ", round(RecHigh, 1), "\u00B0C"),
        Low < RecLow ~ paste0(format(DT, "%b %d"), ": low of ", round(Low, 1), "\u00B0C broke record of ", round(RecLow, 1), "\u00B0C")
      )
    )

  record_text <- if (nrow(record_detail) > 0) {
    paste0("Record-breaking days (since 1981):\n", paste(record_detail$detail, collapse = "\n"), "\n")
  } else ""

  facts_text <- paste0(
    "Period: ", format(start_date, "%b %d"), " - ", format(end_date, "%b %d, %Y"), "\n",
    "\nWINDOW SUMMARY\n",
    "Avg daily high: ", sprintf("%.1f", mean(window_daily$High)), "\u00B0C (normal ", sprintf("%.1f", mean(window_daily$NormalHigh)), "\u00B0C)\n",
    "Avg daily low: ",  sprintf("%.1f", mean(window_daily$Low)),  "\u00B0C (normal ", sprintf("%.1f", mean(window_daily$NormalLow)),  "\u00B0C)\n",
    "Total rainfall: ", sprintf("%.1f", total_rain), "mm (normal ~", sprintf("%.1f", normal_total_rain), "mm)\n",
    "Rainy days: ", rainy_days, " of ", window_n, " (normal ~", sprintf("%.1f", normal_rainy_days), ")\n",
    "\nDAILY DETAIL (actual vs climatological normal)\n", daily_table, "\n",
    if (nzchar(rolling_text))    paste0("\nWETTEST MULTI-DAY STRETCHES\n", rolling_text, "\n") else "",
    if (nzchar(streaks_text))    paste0("\nSTREAKS WITHIN WINDOW\n", streaks_text, "\n") else "",
    if (nzchar(antecedent_text)) paste0("\nANTECEDENT STREAKS (extending before window)\n", antecedent_text, "\n") else "",
    if (nzchar(record_text))     paste0("\n", record_text) else ""
  )

  list(
    facts = facts_text,
    start_date = start_date,
    end_date = end_date,
    window_n = window_n
  )
}

build_few_shot_messages <- function(script_dir, temp_daily, rain_daily) {
  examples_path <- file.path(script_dir, "fewshot_annotations", "fewshot_examples.csv")
  if (!file.exists(examples_path)) return(list())

  examples <- read_csv(examples_path, show_col_types = FALSE) %>%
    mutate(
      start_date = as.Date(start_date),
      end_date = as.Date(end_date)
    )

  n_examples <- min(4, nrow(examples))
  if (n_examples == 0) return(list())
  examples <- examples %>% slice_sample(n = n_examples)

  example_messages <- map(seq_len(nrow(examples)), function(i) {
    ex <- examples[i, ]
    ex_context <- build_weather_window_text(ex$start_date[[1]], ex$end_date[[1]], temp_daily, rain_daily)
    if (is.null(ex_context)) return(NULL)
    list(
      list(
        role = "user",
        content = paste0(
          "Reviewed example input weather stats:\n", ex_context$facts,
          "\nEDITOR NOTE\n", ex$lesson[[1]]
        )
      ),
      list(
        role = "assistant",
        content = paste0("• ", ex$preferred_lead[[1]])
      )
    )
  }) %>% discard(is.null)

  unlist(example_messages, recursive = FALSE)
}

normalize_commentary_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(text)
  text %>%
    str_replace_all("\u2022", "*") %>%
    str_replace_all("\u2013|\u2014|\u2212", "-") %>%
    str_replace_all("[\u2018\u2019]", "'") %>%
    str_replace_all("[\u201C\u201D]", "\"")
}

# --- Recent commentary ---
recent_window <- 14
live_end_date <- Sys.Date() - 1
live_start_date <- live_end_date - (recent_window - 1)
live_context <- build_weather_window_text(live_start_date, live_end_date, temp_daily, rain_daily)
recent_facts <- live_context$facts
few_shot_messages <- build_few_shot_messages(script_dir, temp_daily, rain_daily)

commentary <- tryCatch({
  body <- list(
    model = "claude-haiku-4-5-20251001",
    max_tokens = 200,
    system = paste0(
      "You write a terse chart subtitle for a Bangalore weather visualization. ",
      "You receive: a window summary, a per-day actual-vs-normal table, the wettest 2/3/5-day stretches, every notable streak inside the window (dry, wet, hot, cool, warm-night, cool-night), antecedent dry/wet streaks extending before the window, and any records broken since 1981. The numbers are pre-computed; your job is to pick the most striking signals and phrase them. ",
      "You will first see reviewed historical examples. In those examples, the assistant gives only a single lead bullet showing the preferred framing. Use those as few-shot demonstrations of what deserves emphasis, but do not copy their wording and do not limit yourself to one bullet for the live task. ",
      "Write exactly 3 bullets that a Bangalore resident would find interesting. ",
      "Choose the 2-3 most noteworthy signals from the data. The interesting story might be a multi-day rain stretch, a long dry spell broken by sudden rain, a heat or cool streak, a record day, an unusually warm night pattern, a sustained departure from normal, or something else entirely. Pick whatever is most striking; do not default to any one frame. ",
      "A reviewed example may overturn the naive label suggested by totals alone: if most rain came in one burst, lead with that burst; if rain triggered a sharp cool-down, mention the temperature effect; if records dominate most days, lead with records; if a dry spell is the story, pair duration with shortfall; if rain fell on nearly every day, say that directly. ",
      "Use the per-day deviations to judge what is unusual. If a signal exists in the data (e.g. a 30-day antecedent dry streak, or 5 consecutive nights >=2\u00B0C above normal), you must surface it - do not blur it into a window average. Do not call a window 'trace rainfall' if a single day or short stretch carried most of it; describe what actually happened. ",
      "Each bullet must be under 15 words, start with '\u2022 ', and use \u00B0C. ",
      "Never say 'this period' or 'the period'. Use specific dates like '", format(live_start_date, "%b %d"), " - ", format(live_end_date, "%b %d"), "' or 'late March' etc. ",
      "Plain, calm tone. No hyperbole. Output only the 3 bullets, nothing else."
    ),
    messages = c(
      few_shot_messages,
      list(list(role = "user", content = paste0("Bangalore weather stats:\n", recent_facts)))
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
    else normalize_commentary_text(paste(trimws(bullet_lines), collapse = "\n"))
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
  scale_x_date("", lim = c(floor_date(as.Date(paste0(curr_year, '-01-01')), '1 year'), ceiling_date(as.Date(paste0(curr_year, '-12-31')), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), labels = ~ toupper(format(.x, "%B")), position = 'top', expand = expansion(mult = 0)) +
  scale_y_continuous("", breaks = seq(10, 42, 4) )  +
  ggthemes::theme_tufte()  +
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold', size = 7), axis.line.y = element_line(colour = 'black', linewidth = 0.2),  panel.background = element_rect(fill = "#eae4db", linewidth = 0), plot.background = element_rect(fill = "#eae4db") ) +
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 38, label = "Temperature", hjust = 0, fontface = 'bold', size = 3) +
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 37.5, label = str_wrap("Bars represent range between the daily high and low", 40), hjust = 0, vjust = 1, size = 2.5) +
  annotate("text", x = Sys.Date() - 14, y = 37, label = "RECORD HIGH", hjust = 0, size = 2, fontface = 'bold', color = "#b0a882") +
  annotate("text", x = Sys.Date() - 14, y = 34, label = "ACTUAL HIGH", hjust = 0, size = 2, fontface = 'bold', color = "#490000") +
  annotate("text", x = Sys.Date() - 14, y = 27, label = "NORMAL RANGE", hjust = 0, size = 2, fontface = 'bold', color = "#888888") +
  annotate("text", x = Sys.Date() - 14, y = 20, label = "ACTUAL LOW", hjust = 0, size = 2, fontface = 'bold', color = "#490000") +
  annotate("text", x = Sys.Date() - 14, y = 15, label = "RECORD LOW", hjust = 0, size = 2, fontface = 'bold', color = "#b0a882") ->
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
    Year = year(DT),
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
  arrange(DT) %>%
  mutate(
    CumulRainYM = cumsum(Rain),
    .by = c(Year, Month)
  ) %>%
  mutate(
    CumulMin = quantile(CumulRainYM, 0.10),
    CumulMax = quantile(CumulRainYM, 0.90),
    .by = c(Month, Day)
  ) %>%
  filter(Year == curr_year) %>%
  mutate(
    CumulRain = CumulRainYM,
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
  geom_ribbon(aes(ymin = CumulMin, ymax = CumulMax, group = Month), fill = "#6699CC", alpha = 0.2) +
  geom_segment(aes(x = DT, xend = DT, y = CumulRain - Rain, yend = CumulRain), linewidth = 1, col = '#2255AA') +
  geom_step(aes(y = MonthlyAvg, group = Month), lwd = 1, col = '#4477BB') +
  geom_text(aes(y = MonthlyAvg, label = normalLabel), vjust = -0.05, hjust = 0, size = 2.5, fontface = 'bold') +
  geom_text(aes(y = CumulRain, label = actualLabel), vjust = -0.05, hjust = 1, size = 2.5, fontface = 'bold') +
  ggrepel::geom_text_repel(aes(y = CumulRain, label = str_wrap(Label, 10)), size = 2,  fontface = 'bold') +
  scale_x_date("", lim = c(floor_date(as.Date(paste0(curr_year, '-01-01')), '1 year'), ceiling_date(as.Date(paste0(curr_year, '-12-31')), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), labels = ~ toupper(format(.x, "%B")), expand = expansion(mult = 0)) +
  ggthemes::theme_tufte() +
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold', size = 7), axis.line.y = element_line(colour = 'black', linewidth = 0.2), panel.background = element_rect(fill = "#eae4db", linewidth = 0), plot.background = element_rect(fill = "#eae4db") ) +
  scale_y_continuous("", breaks = seq(0,500, 50)) +
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 260, label = "Precipitation", hjust = 0, fontface = 'bold', size = 3) +
  annotate("text", x = as.Date(paste0(curr_year, '-02-05')), y = 260, label = str_wrap("Cumulative monthly precipitation in mm compared with normal monthly precipitation. Blue band shows historical range.", 1000), hjust = 0,  size = 2.5)  ->
  rainPlot

combined <- tempPlot + rainPlot +
  plot_layout(ncol = 1, heights = c(70, 30)) +
  plot_annotation(
    title = paste0("Bangalore's Weather in ", curr_year, ", as of ", format(Sys.Date(), "%B %d")),
    subtitle = commentary,
    caption = "Data source: Oikolab"
  ) &
  theme(
    plot.title = element_text(face = 'bold', hjust = 0),
    plot.subtitle = element_text(size = 8, color = "#490000", hjust = 0, margin = margin(t = 2, b = 4)),
    panel.background = element_rect(fill = "#eae4db"),
    plot.background = element_rect(fill = "#eae4db", linewidth = 0)
  )

outfile <- file.path(script_dir, 'charts', paste0('bangalore_weather_', format(Sys.Date(), '%Y%m%d'), '.png'))
ggsave(outfile, combined, width = 12, height = 6)
message("Saved: ", outfile)
combined
