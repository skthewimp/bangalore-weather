require(tidytable)
require(tidyverse)
require(patchwork)
require(lubridate)
require(httr2)
require(jsonlite)

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

generate_commentary <- function(stats) {

  # R pre-ranks facts by how far they deviate from average
  safe_dev <- function(actual, avg) {
    if (is.null(avg) || is.na(avg) || avg == 0) return(0)
    abs(actual - avg) / abs(avg)
  }

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

  system_prompt <- paste0(
    "Rewrite these 5 weather facts as exactly 4 chart subtitle bullets.\n",
    "Rules:\n",
    "- One fact per bullet, under 15 words. Drop the least interesting one.\n",
    "- Keep all numbers, dates, and periods exactly as given.\n",
    "- Plain calm tone. No 'unprecedented', 'dramatic', 'remarkable', 'significant', 'notably', 'surpassing'.\n",
    "- Use \u00B0C for temperatures.\n",
    "- Format: 4 lines starting with '\u2022 '. Nothing else.\n\n",
    "Example input:\n",
    "42 days above 35\u00B0C vs the usual 18\n",
    "A 47-day dry spell from Jan 03 to Feb 18\n\n",
    "Example output:\n",
    "\u2022 42 days crossed 35\u00B0C, more than double the usual 18\n",
    "\u2022 A 47-day dry spell stretched from early Jan to mid-Feb"
  )

  body <- list(
    model = "gemma3:4b",
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = paste0("Bangalore ", stats$year, " weather facts to rewrite:\n", top_facts))
    ),
    max_tokens = 300,
    temperature = 0.3
  )

  resp <- tryCatch({
    request("http://localhost:11434/v1/chat/completions") %>%
      req_body_json(body) %>%
      req_timeout(120) %>%
      req_error(is_error = ~ FALSE) %>%
      req_perform()
  }, error = function(e) {
    message("Ollama call failed: ", e$message, ". Skipping commentary.")
    return(NULL)
  })

  if (is.null(resp)) return(NULL)

  if (resp_status(resp) != 200) {
    message("Ollama returned status ", resp_status(resp), ". Skipping commentary.")
    return(NULL)
  }

  result <- resp %>% resp_body_json()
  raw <- result$choices[[1]]$message$content
  # Keep only lines starting with bullet character, drop any preamble
  lines <- strsplit(raw, "\n")[[1]]
  bullet_lines <- lines[grepl("^\u2022", trimws(lines))]
  if (length(bullet_lines) == 0) return(NULL)
  # Strip banned words the model sometimes sneaks in
  cleaned <- gsub("\\s*(significantly|notably|remarkably|dramatically|unprecedented)\\s*", " ",
                   paste(trimws(bullet_lines), collapse = "\n"), ignore.case = TRUE)
  trimws(gsub("  +", " ", cleaned))
}

generate_weather_chart <- function(target_year, save_path = NULL, width = 12, height = 6) {

  load('data/bangaloreTemperature.RData')
  load('data/bangaloreRainfall.RData')

  blrTemp %>%
    filter(!is.na(Temp)) %>%
    filter(year(DT) <= target_year) ->
    blrTemp

  blrRain %>%
    filter(!is.na(Rain)) %>%
    filter(year(DT) <= target_year) ->
    blrRain

  curr_year <- target_year

  # Compute stats and generate AI commentary
  weather_stats <- compute_weather_stats(blrTemp, blrRain, target_year)
  commentary <- generate_commentary(weather_stats)

  # Month boundary dates for vertical separator lines
  month_bounds <- as.Date(paste0(curr_year, '-', 2:12, '-01'))

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
      RecHigh = High2022 >= RecordHigh,
      RecLow = Low2022 <= RecordLow
    ) -> temp_data

    # Inline visual legend position (upper area, Oct-Nov where temps are lower)
    leg_x <- as.Date(paste0(curr_year, '-10-20'))
    leg_lo <- 33
    leg_nlo <- 34.5
    leg_alo <- 35.5
    leg_ahi <- 37
    leg_nhi <- 38
    leg_hi <- 39.5

    ggplot(temp_data) +
    # Month boundary lines
    geom_vline(xintercept = month_bounds, colour = '#c8c0aa', linewidth = 0.3) +
    # Data layers
    geom_segment(aes(x = Date, xend = Date, y = RecordLow, yend = RecordHigh), linewidth = 1, col = "#c8c0aa") +
    geom_segment(aes(x = Date, xend = Date, y = NormalLow, yend = NormalHigh), linewidth = 1, col = '#9c9280') +
    geom_segment(aes(x = Date, xend = Date, y = Low2022, yend = High2022), linewidth = 1, col = "#5f3946") +
    # Record high days (firebrick dots)
    geom_point(data = temp_data %>% filter(RecHigh), aes(x = Date, y = High2022), col = 'firebrick3', size = 1.5) +
    # Record low days (blue dots)
    geom_point(data = temp_data %>% filter(RecLow), aes(x = Date, y = Low2022), col = 'blue3', size = 1.5) +
    # Inline visual legend — example bar in lower area
    annotate("segment", x = leg_x, xend = leg_x, y = leg_lo, yend = leg_hi, linewidth = 4, col = "#c8c0aa") +
    annotate("segment", x = leg_x, xend = leg_x, y = leg_nlo, yend = leg_nhi, linewidth = 4, col = "#9c9280") +
    annotate("segment", x = leg_x, xend = leg_x, y = leg_alo, yend = leg_ahi, linewidth = 4, col = "#5f3946") +
    # Legend labels — right side
    annotate("segment", x = leg_x + 1, xend = leg_x + 12, y = leg_hi, yend = leg_hi, linewidth = 0.3, col = '#3C3C3C') +
    annotate("text", x = leg_x + 13, y = leg_hi, label = "RECORD HIGH", hjust = 0, size = 2, col = '#3C3C3C') +
    annotate("segment", x = leg_x + 1, xend = leg_x + 12, y = leg_nhi, yend = leg_nhi, linewidth = 0.3, col = '#3C3C3C') +
    annotate("text", x = leg_x + 13, y = leg_nhi, label = "NORMAL HIGH", hjust = 0, size = 2, col = '#3C3C3C') +
    annotate("segment", x = leg_x + 1, xend = leg_x + 12, y = leg_nlo, yend = leg_nlo, linewidth = 0.3, col = '#3C3C3C') +
    annotate("text", x = leg_x + 13, y = leg_nlo, label = "NORMAL LOW", hjust = 0, size = 2, col = '#3C3C3C') +
    annotate("segment", x = leg_x + 1, xend = leg_x + 12, y = leg_lo, yend = leg_lo, linewidth = 0.3, col = '#3C3C3C') +
    annotate("text", x = leg_x + 13, y = leg_lo, label = "RECORD LOW", hjust = 0, size = 2, col = '#3C3C3C') +
    # Legend label — left side (actual year)
    annotate("segment", x = leg_x - 1, xend = leg_x - 12, y = (leg_alo + leg_ahi) / 2, yend = (leg_alo + leg_ahi) / 2, linewidth = 0.3, col = '#3C3C3C') +
    annotate("text", x = leg_x - 13, y = (leg_alo + leg_ahi) / 2, label = paste0(curr_year, "\nTEMPERATURE"), hjust = 1, size = 2, col = '#3C3C3C', lineheight = 0.9) +
    # Axes
    scale_x_date("", lim = c(floor_date(as.Date(paste0(curr_year, '-01-01')), '1 year'), ceiling_date(as.Date(paste0(curr_year, '-12-31')), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), date_labels = '%B', position = 'top', expand = expansion(mult = 0)) +
    scale_y_continuous("", breaks = seq(10, 42, 4), labels = function(x) paste0(x, "\u00B0")) +
    ggthemes::theme_tufte() +
    theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold'), axis.line.y = element_line(colour = 'black', linewidth = 0.2), panel.background = element_rect(fill = '#e5e1d8', linewidth = 0), plot.background = element_rect(fill = '#e5e1d8')) +
    annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 40, label = "Temperature", hjust = 0, fontface = 'bold', size = 3, col = '#3C3C3C') ->
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
    geom_vline(xintercept = month_bounds, colour = '#c8c0aa', linewidth = 0.3) +
    geom_segment(aes(x = DT, xend = DT, y = CumulRain - Rain, yend = CumulRain), linewidth = 1, col = '#5f3946') +
    geom_step(aes(y = MonthlyAvg, group = Month), lwd = 1, col = '#9c9280') +
    geom_text(aes(y = MonthlyAvg, label = normalLabel), vjust = -0.05, hjust = 0, size = 2.5, fontface = 'bold', col = '#3C3C3C') +
    geom_text(aes(y = CumulRain, label = actualLabel), vjust = -0.05, hjust = 1, size = 2.5, fontface = 'bold', col = '#3C3C3C') +
    ggrepel::geom_text_repel(aes(y = CumulRain, label = str_wrap(Label, 10)), size = 2, fontface = 'bold', col = '#3C3C3C') +
    scale_x_date("", lim = c(floor_date(as.Date(paste0(curr_year, '-01-01')), '1 year'), ceiling_date(as.Date(paste0(curr_year, '-12-31')), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), date_labels = '%B', expand = expansion(mult = 0)) +
    ggthemes::theme_tufte() +
    theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold'), axis.line.y = element_line(colour = 'black', linewidth = 0.2), panel.background = element_rect(fill = '#e5e1d8', linewidth = 0), plot.background = element_rect(fill = '#e5e1d8')) +
    scale_y_continuous("", breaks = seq(0,500, 50)) +
    annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 260, label = "Precipitation", hjust = 0, fontface = 'bold', size = 3, col = '#3C3C3C') +
    annotate("text", x = as.Date(paste0(curr_year, '-02-05')), y = 260, label = str_wrap("Cumulative monthly precipitation in mm compared to normal monthly precipitation", 1000), hjust = 0, size = 2.5, col = '#3C3C3C')  ->
    rainPlot

  combined <- tempPlot + rainPlot +
    plot_layout(ncol = 1, heights = c(70, 30)) +
    plot_annotation(
      title = paste("Bangalore's Weather in", curr_year),
      subtitle = commentary,
      caption = "Data source: Oikokab"
    ) &
    theme(
      plot.title = element_text(face = 'bold', hjust = 0, colour = '#3C3C3C'),
      plot.subtitle = element_text(size = 8, color = "#5f3946", hjust = 0, margin = margin(t = 2, b = 4)),
      plot.caption = element_text(colour = '#3C3C3C'),
      panel.background = element_rect(fill = '#e5e1d8'),
      plot.background = element_rect(fill = '#e5e1d8', linewidth = 0)
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
    outfile <- paste0('charts/bangalore_weather_', yr, '.png')
    generate_weather_chart(yr, save_path = outfile)
  }
}
