require(tidytable)
require(tidyverse)
require(ggtext)
require(lubridate)
require(httr2)
require(jsonlite)
require(patchwork)

.script_dir <- tryCatch({
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

source(file.path(.script_dir, "weather_chart_common.R"))

compute_weather_stats <- function(blrTemp, blrRain, target_year) {

  # --- Daily temperature summaries ---
  temp_daily <- blrTemp %>%
    mutate(DT = as.Date(DT), Temp = as.numeric(Temp)) %>%
    summarise(High = max(Temp), Low = min(Temp), .by = DT) %>%
    mutate(Year = year(DT), Month = month(DT), Day = day(DT))

  curr <- temp_daily %>% filter(Year == target_year)
  hist <- temp_daily %>% filter(Year < target_year)

  # Data-driven thresholds from historical percentiles
  hot_day_thresh <- round(quantile(hist$High, 0.95, na.rm = TRUE), 1)
  cold_day_thresh <- round(quantile(hist$Low, 0.05, na.rm = TRUE), 1)
  hot_streak_thresh <- round(quantile(hist$High, 0.90, na.rm = TRUE), 1)
  cold_streak_thresh <- round(quantile(hist$Low, 0.10, na.rm = TRUE), 1)

  # Historical baselines
  hist_avg_hot_days <- hist %>%
    summarise(n = sum(High > hot_day_thresh), .by = Year) %>% pull(n) %>% mean(na.rm = TRUE)
  hist_avg_cold_days <- hist %>%
    summarise(n = sum(Low < cold_day_thresh), .by = Year) %>% pull(n) %>% mean(na.rm = TRUE)
  hist_avg_high <- mean(hist$High, na.rm = TRUE)
  hist_avg_low  <- mean(hist$Low, na.rm = TRUE)

  # Current year temperature stats
  hottest <- curr %>% slice_max(High, n = 1, with_ties = FALSE)
  coldest <- curr %>% slice_min(Low, n = 1, with_ties = FALSE)
  hot_days_count <- sum(curr$High > hot_day_thresh)
  cold_days_count <- sum(curr$Low < cold_day_thresh)

  # Streaks (with date ranges)
  hot_rle <- rle(curr$High > hot_streak_thresh)
  cold_rle <- rle(curr$Low < cold_streak_thresh)
  hot_streak <- max(c(hot_rle$lengths[hot_rle$values], 0))
  cold_streak <- max(c(cold_rle$lengths[cold_rle$values], 0))

  streak_period <- function(rle_obj, dates, val = TRUE) {
    if (!any(rle_obj$values == val)) return("")
    idx <- which(rle_obj$values == val)
    best <- idx[which.max(rle_obj$lengths[idx])]
    end_pos <- sum(rle_obj$lengths[1:best])
    start_pos <- end_pos - rle_obj$lengths[best] + 1
    paste(format(dates[start_pos], "%b %d"), "to", format(dates[end_pos], "%b %d"))
  }
  hot_streak_period <- streak_period(hot_rle, curr$DT)
  cold_streak_period <- streak_period(cold_rle, curr$DT)

  # Record-breaking days
  hist_records <- hist %>%
    summarise(RecHigh = max(High), RecLow = min(Low), .by = c(Month, Day))
  record_days <- curr %>%
    left_join(hist_records, by = c("Month", "Day")) %>%
    filter(!is.na(RecHigh) & (High > RecHigh | Low < RecLow)) %>%
    nrow()

  # --- Daily rainfall summaries ---
  rain_daily <- blrRain %>%
    mutate(DT = as.Date(DT), Rain = as.numeric(Rain)) %>%
    summarise(Rain = sum(Rain), .by = DT) %>%
    mutate(Year = year(DT), Month = month(DT))

  curr_rain <- rain_daily %>% filter(Year == target_year)
  hist_rain <- rain_daily %>% filter(Year < target_year)

  hist_avg_annual <- hist_rain %>%
    summarise(total = sum(Rain), .by = Year) %>% pull(total) %>% mean(na.rm = TRUE)
  curr_annual <- sum(curr_rain$Rain)

  wettest_day <- curr_rain %>% slice_max(Rain, n = 1, with_ties = FALSE)

  # Monthly comparison
  curr_monthly <- curr_rain %>% summarise(total = sum(Rain), .by = Month)
  hist_monthly_avg <- hist_rain %>%
    summarise(total = sum(Rain), .by = c(Year, Month)) %>%
    summarise(avg = mean(total), .by = Month)
  monthly_compare <- curr_monthly %>% left_join(hist_monthly_avg, by = "Month")

  wettest_month <- monthly_compare %>% slice_max(total, n = 1, with_ties = FALSE)
  driest_month <- monthly_compare %>% filter(avg > 20) %>% slice_min(total, n = 1, with_ties = FALSE)

  # Dry and wet spells (with date ranges)
  dry_rle <- rle(curr_rain$Rain < 0.1)
  wet_rle <- rle(curr_rain$Rain >= 0.1)
  dry_spell <- max(c(dry_rle$lengths[dry_rle$values], 0))
  wet_spell <- max(c(wet_rle$lengths[wet_rle$values], 0))
  dry_spell_period <- streak_period(dry_rle, curr_rain$DT)
  wet_spell_period <- streak_period(wet_rle, curr_rain$DT)

  rainy_days <- sum(curr_rain$Rain >= 0.1)
  hist_avg_rainy <- hist_rain %>%
    summarise(n = sum(Rain >= 0.1), .by = Year) %>% pull(n) %>% mean(na.rm = TRUE)

  # Monsoon (Jun-Sep)
  monsoon_total <- curr_rain %>% filter(Month %in% 6:9) %>% pull(Rain) %>% sum()
  hist_monsoon_avg <- hist_rain %>%
    filter(Month %in% 6:9) %>%
    summarise(total = sum(Rain), .by = Year) %>% pull(total) %>% mean(na.rm = TRUE)

  list(
    year = target_year,
    hot_day_thresh = hot_day_thresh,
    cold_day_thresh = cold_day_thresh,
    hot_streak_thresh = hot_streak_thresh,
    cold_streak_thresh = cold_streak_thresh,
    hottest_day = format(hottest$DT, "%b %d"),
    hottest_temp = round(hottest$High, 1),
    coldest_day = format(coldest$DT, "%b %d"),
    coldest_temp = round(coldest$Low, 1),
    hot_days = hot_days_count,
    hot_days_avg = round(hist_avg_hot_days, 1),
    cold_days = cold_days_count,
    cold_days_avg = round(hist_avg_cold_days, 1),
    mean_high = round(mean(curr$High), 1),
    mean_high_avg = round(hist_avg_high, 1),
    mean_low = round(mean(curr$Low), 1),
    mean_low_avg = round(hist_avg_low, 1),
    hot_streak_days = hot_streak,
    hot_streak_period = hot_streak_period,
    cold_streak_days = cold_streak,
    cold_streak_period = cold_streak_period,
    record_breaking_days = record_days,
    annual_rain_mm = round(curr_annual, 0),
    annual_rain_avg = round(hist_avg_annual, 0),
    wettest_day_date = format(wettest_day$DT, "%b %d"),
    wettest_day_mm = round(wettest_day$Rain, 1),
    wettest_month = month.name[wettest_month$Month],
    wettest_month_mm = round(wettest_month$total, 0),
    wettest_month_avg = round(wettest_month$avg, 0),
    driest_month = month.name[driest_month$Month],
    driest_month_mm = round(driest_month$total, 0),
    driest_month_avg = round(driest_month$avg, 0),
    longest_dry_spell = dry_spell,
    dry_spell_period = dry_spell_period,
    longest_wet_spell = wet_spell,
    wet_spell_period = wet_spell_period,
    rainy_days = rainy_days,
    rainy_days_avg = round(hist_avg_rainy, 0),
    monsoon_mm = round(monsoon_total, 0),
    monsoon_avg = round(hist_monsoon_avg, 0)
  )
}

normalize_commentary_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(text)
  text %>%
    str_replace_all("\u2022", "*") %>%
    str_replace_all("\u2013|\u2014|\u2212", "-") %>%
    str_replace_all("[\u2018\u2019]", "'") %>%
    str_replace_all("[\u201C\u201D]", "\"")
}

safe_dev <- function(actual, avg) {
  if (is.null(actual) || is.null(avg) || is.na(actual) || is.na(avg) || avg == 0) return(0)
  abs(actual - avg) / abs(avg)
}

generate_commentary <- function(stats) {
  # R pre-ranks facts by how far they deviate from average
  facts <- list(
    list(dev = safe_dev(stats$hot_days, stats$hot_days_avg),
         text = paste0(stats$hot_days, " days above ", stats$hot_day_thresh, "\u00B0C vs the usual ", stats$hot_days_avg)),
    list(dev = safe_dev(stats$cold_days, stats$cold_days_avg),
         text = paste0(stats$cold_days, " days below ", stats$cold_day_thresh, "\u00B0C vs the usual ", stats$cold_days_avg)),
    list(dev = safe_dev(stats$annual_rain_mm, stats$annual_rain_avg),
         text = paste0("Annual rainfall ", stats$annual_rain_mm, "mm vs the usual ", stats$annual_rain_avg, "mm")),
    list(dev = safe_dev(stats$monsoon_mm, stats$monsoon_avg),
         text = paste0("Monsoon (Jun-Sep) rainfall ", stats$monsoon_mm, "mm vs the usual ", stats$monsoon_avg, "mm")),
    list(dev = safe_dev(stats$wettest_month_mm, stats$wettest_month_avg),
         text = paste0(stats$wettest_month, " got ", stats$wettest_month_mm, "mm of rain vs the usual ", stats$wettest_month_avg, "mm")),
    list(dev = safe_dev(stats$driest_month_mm, stats$driest_month_avg),
         text = paste0(stats$driest_month, " got only ", stats$driest_month_mm, "mm of rain vs the usual ", stats$driest_month_avg, "mm")),
    list(dev = safe_dev(stats$rainy_days, stats$rainy_days_avg),
         text = paste0(stats$rainy_days, " rainy days vs the usual ", stats$rainy_days_avg)),
    list(dev = ifelse(stats$hot_streak_days > 10, 1.0, stats$hot_streak_days / 20),
         text = paste0("A ", stats$hot_streak_days, "-day hot streak above ", stats$hot_streak_thresh, "\u00B0C from ", stats$hot_streak_period)),
    list(dev = ifelse(stats$cold_streak_days > 10, 0.8, stats$cold_streak_days / 20),
         text = paste0("A ", stats$cold_streak_days, "-day cold streak below ", stats$cold_streak_thresh, "\u00B0C from ", stats$cold_streak_period)),
    list(dev = ifelse(stats$longest_dry_spell > 30, 1.0, stats$longest_dry_spell / 50),
         text = paste0("A ", stats$longest_dry_spell, "-day dry spell from ", stats$dry_spell_period)),
    list(dev = ifelse(stats$longest_wet_spell > 10, 0.8, stats$longest_wet_spell / 20),
         text = paste0("A ", stats$longest_wet_spell, "-day wet spell from ", stats$wet_spell_period)),
    list(dev = ifelse(stats$record_breaking_days > 5, 0.9, stats$record_breaking_days / 10),
         text = paste0(stats$record_breaking_days, " days broke all-time records for their calendar date"))
  )

  # Sort by deviation, take top 5
  devs <- sapply(facts, function(f) f$dev)
  top_idx <- order(devs, decreasing = TRUE)[1:5]
  top_facts <- paste(sapply(facts[top_idx], function(f) f$text), collapse = "\n")

  base_body <- list(
    max_tokens = 300,
    system = paste0(
      "You are writing a terse chart subtitle for a Bangalore weather visualization for the year ", stats$year, ". ",
      "You receive weather stats ranked by how unusual they are compared to 40-year historical norms. ",
      "Write exactly 4 bullets that a Bangalore resident would find interesting. ",
      "Focus on what stands out - comparisons to normal, streaks, records. Not just restating numbers. ",
      "Each bullet must be under 15 words, start with '\u2022 ', and use \u00B0C. ",
      "Write like an observant Bangalore resident, not a lab report. ",
      "Plain, calm tone. No hyperbole. Output only the 4 bullets, nothing else."
    ),
    messages = list(
      list(role = "user", content = paste0("Top 5 most unusual Bangalore weather facts for ", stats$year, ":\n", top_facts))
    )
  )

  max_attempts <- as.integer(Sys.getenv("HISTORICAL_COMMENTARY_MAX_ATTEMPTS", "6"))
  if (is.na(max_attempts) || max_attempts < 1) max_attempts <- 1
  initial_sleep <- as.numeric(Sys.getenv("HISTORICAL_COMMENTARY_INITIAL_SLEEP", "8"))
  if (is.na(initial_sleep) || initial_sleep < 1) initial_sleep <- 1
  model_names <- strsplit(
    Sys.getenv(
      "HISTORICAL_COMMENTARY_MODELS",
      "claude-haiku-4-5-20251001,claude-sonnet-4-5-20250929"
    ),
    ","
  )[[1]] %>%
    trimws() %>%
    discard(~ !nzchar(.x))

  if (length(model_names) == 0) {
    stop("No Claude models configured for historical commentary.", call. = FALSE)
  }

  resp <- NULL
  last_status <- NULL
  last_error <- NULL

  for (model_name in model_names) {
    body <- c(list(model = model_name), base_body)
    message("Generating ", stats$year, " commentary with ", model_name)

    resp <- tryCatch({
      request("https://api.anthropic.com/v1/messages") %>%
        req_headers(
          `x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"),
          `anthropic-version` = "2023-06-01",
          `content-type` = "application/json"
        ) %>%
        req_body_json(body) %>%
        req_timeout(30) %>%
        perform_claude_request_with_retries(max_attempts = max_attempts, initial_sleep = initial_sleep)
    }, error = function(e) {
      last_error <<- e$message
      return(NULL)
    })

    if (is.null(resp)) next

    last_status <- resp_status(resp)
    if (last_status == 200) break

    message("Claude model ", model_name, " returned status ", last_status, ". Trying next model if available.")
    resp <- NULL
  }

  if (is.null(resp)) {
    if (!is.null(last_error)) {
      stop("Claude API call failed for ", stats$year, ": ", last_error, call. = FALSE)
    }
    stop("Claude API returned no successful response for ", stats$year, call. = FALSE)
  }

  if (resp_status(resp) != 200) {
    stop("Claude API returned status ", resp_status(resp), " for ", stats$year, call. = FALSE)
  }

  raw <- resp %>% resp_body_json() %>% .$content %>% .[[1]] %>% .$text
  lines <- strsplit(raw, "\n")[[1]]
  bullet_lines <- lines[grepl("^\u2022", trimws(lines))]
  if (length(bullet_lines) == 0) {
    stop("Claude returned no bullet commentary for ", stats$year, call. = FALSE)
  }
  normalize_commentary_text(paste(trimws(bullet_lines), collapse = "\n"))
}

generate_weather_chart <- function(target_year, save_path = NULL, width = 13.5, height = 7.5, commentary = NULL) {

  load(file.path(.script_dir, 'data', 'bangaloreTemperature.RData'))
  load(file.path(.script_dir, 'data', 'bangaloreRainfall.RData'))

  blrTemp %>%
    filter(!is.na(Temp)) %>%
    filter(year(DT) <= target_year) ->
    blrTemp

  blrRain %>%
    filter(!is.na(Rain)) %>%
    filter(year(DT) <= target_year) ->
    blrRain

  curr_year <- target_year

  # Compute stats and generate AI commentary (skip if commentary was passed in)
  if (is.null(commentary)) {
    weather_stats <- compute_weather_stats(blrTemp, blrRain, target_year)
    commentary <- generate_commentary(weather_stats)
  }

  temp_data <- build_temperature_plot_data(blrTemp, curr_year)
  rain_data <- build_rain_plot_data(blrRain, curr_year)

  combined <- render_weather_chart(
    temp_data = temp_data,
    rain_data = rain_data,
    curr_year = curr_year,
    title = paste("Bangalore's Weather in", curr_year),
    subtitle = commentary,
    caption = "Data source: Oikolab"
  )

  if (!is.null(save_path)) {
    ggsave(save_path, combined, width = width, height = height)
    message("Saved: ", save_path)
  }

  combined
}

# Generate charts for specified years
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  years <- as.integer(args)
  for (yr in years) {
    outfile <- file.path(.script_dir, 'charts', paste0('bangalore_weather_', yr, '.png'))
    generate_weather_chart(yr, save_path = outfile)
    delay <- as.numeric(Sys.getenv("HISTORICAL_COMMENTARY_YEAR_DELAY", "12"))
    if (!is.na(delay) && delay > 0) Sys.sleep(delay)
  }
}
