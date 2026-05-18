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

source(file.path(script_dir, "weather_chart_common.R"))

primKey <- Sys.getenv("OIKOLAB_PRIMARY")
secKey <- Sys.getenv("OIKOLAB_SECONDARY")
load(file.path(script_dir, 'data', 'bangaloreRainfall.RData'))
load(file.path(script_dir, 'data', 'bangaloreTemperature.RData'))
load(file.path(script_dir, 'data', 'bangaloreWind.RData'))

blrTemp %>%
  mutate(Temp = as.numeric(Temp)) %>%
  filter(!is.na(Temp)) ->
  blrTemp

blrRain %>%
  mutate(Rain = as.numeric(Rain)) %>%
  filter(!is.na(Rain)) -> 
  blrRain

blrWind %>%
  mutate(
    Wind = as.numeric(Wind),
    WindDir = as.numeric(WindDir)
  ) %>%
  filter(!is.na(Wind)) ->
  blrWind

bloreLat <- 12.9716
bloreLon <- 77.5946
tz_local <- Sys.timezone()
backfill_days <- 10
reanalysis_lag_days <- 7

fmt_oiko_time <- function(x) {
  format(as.POSIXct(x, tz = tz_local), "%Y-%m-%dT%H:%M:%S")
}

month_starts_between <- function(start_dt, end_dt) {
  seq(floor_date(as.Date(start_dt), "month"),
      floor_date(as.Date(end_dt - hours(1)), "month"),
      by = "1 month")
}

oiko_units_needed <- function(start_dt, end_dt, n_params) {
  length(month_starts_between(start_dt, end_dt)) * n_params
}

fetch_oiko_payload <- function(start_dt, end_dt, param, model = NULL) {
  req <- request("https://api.oikolab.com/weather") %>%
    req_headers(`api-key` = primKey) %>%
    req_url_query(
      start = fmt_oiko_time(start_dt),
      end = fmt_oiko_time(end_dt),
      param = param,
      freq = "H",
      lat = bloreLat,
      lon = bloreLon
    )

  if (!is.null(model)) {
    req <- req %>% req_url_query(model = model)
  }

  resp <- req %>% req_perform() %>% resp_body_json(simplifyVector = TRUE)
  jsonlite::fromJSON(resp$data)
}

fetch_oiko_series <- function(start_dt, end_dt, param, value_name, model = NULL) {
  payload <- fetch_oiko_payload(start_dt, end_dt, param, model = model)

  payload$data %>%
    as.data.frame() %>%
    as_tibble() %>%
    set_names(c("Latlong", "Source", "Something", "SomethingElse", value_name)) %>%
    mutate(
      Index = payload$index,
      DT = as.POSIXct(Index, origin = "1970-01-01", tz = tz_local),
      !!value_name := as.numeric(.data[[value_name]])
    )
}

replace_window <- function(existing, replacement, start_dt, end_dt) {
  existing %>%
    filter(DT < start_dt | DT >= end_dt) %>%
    bind_rows(replacement)
}

dedupe_on_dt <- function(df) {
  df %>%
    mutate(.source_rank = case_when(
      Source == "era5" ~ 2L,
      Source == "gfs" ~ 1L,
      TRUE ~ 0L
    )) %>%
    arrange(DT, desc(.source_rank), desc(Index)) %>%
    distinct(DT, .keep_all = TRUE) %>%
    select(-.source_rank)
}

fetch_oiko_account <- function() {
  tryCatch(
    request("https://api.oikolab.com/account") %>%
      req_headers(`api-key` = primKey) %>%
      req_perform() %>%
      resp_body_json(simplifyVector = TRUE),
    error = function(e) NULL
  )
}

repair_month_starts <- function(df, cutoff_date) {
  df %>%
    mutate(Date = as.Date(DT)) %>%
    filter(Source == "gfs", Date <= cutoff_date) %>%
    transmute(MonthStart = floor_date(Date, "month")) %>%
    distinct() %>%
    pull(MonthStart) %>%
    sort()
}

repair_month_windows <- function(month_starts) {
  tibble(
    start_dt = as.POSIXct(month_starts, tz = tz_local),
    end_dt = as.POSIXct(month_starts %m+% months(1), tz = tz_local)
  )
}

reanalysis_ready_date <- Sys.Date() - reanalysis_lag_days
fetch_end <- as.POSIXct(paste0(Sys.Date(), " 00:00:00"), tz = tz_local)
backfill_start <- as.POSIXct(paste0(Sys.Date() - backfill_days, " 00:00:00"), tz = tz_local)
backfill_start <- max(backfill_start, as.POSIXct(min(blrTemp$DT), tz = tz_local))

repair_months <- union(
  repair_month_starts(blrTemp, reanalysis_ready_date),
  repair_month_starts(blrRain, reanalysis_ready_date)
)
repair_windows <- repair_month_windows(repair_months)

backfill_units <- oiko_units_needed(backfill_start, fetch_end, n_params = 4)
repair_units <- if (nrow(repair_windows) > 0) nrow(repair_windows) * 2 else 0
estimated_units <- backfill_units + repair_units

acct <- fetch_oiko_account()
remaining_units <- if (is.null(acct)) Inf else acct$incl_units_per_billing_period - acct$current_usage
do_historical_repair <- repair_units > 0 && estimated_units <= remaining_units

message(
  "Oikolab refresh plan: backfill ", backfill_days, " days (", backfill_units,
  " units) + historical repair ", repair_units, " units; remaining allowance ",
  ifelse(is.finite(remaining_units), remaining_units, "unknown"), "."
)

if (repair_units > 0 && !do_historical_repair) {
  message("Skipping historical gfs->era5 repair to stay within remaining Oikolab allowance.")
}

blrTempBackfill <- fetch_oiko_series(backfill_start, fetch_end, "temperature", "Temp")
blrRainBackfill <- fetch_oiko_series(backfill_start, fetch_end, "total_precipitation", "Rain")
blrWindBackfill <- fetch_oiko_series(backfill_start, fetch_end, "wind_speed", "Wind")
blrWindDirBackfill <- fetch_oiko_series(backfill_start, fetch_end, "wind_direction", "WindDir") %>%
  select(DT, WindDir)
blrWindBackfill <- blrWindBackfill %>% left_join(blrWindDirBackfill, by = "DT")

blrTemp <- replace_window(blrTemp, blrTempBackfill, backfill_start, fetch_end)
blrRain <- replace_window(blrRain, blrRainBackfill, backfill_start, fetch_end)
blrWind <- replace_window(blrWind, blrWindBackfill, backfill_start, fetch_end)

if (do_historical_repair) {
  for (i in seq_len(nrow(repair_windows))) {
    win_start <- repair_windows$start_dt[i]
    win_end <- repair_windows$end_dt[i]
    message(
      "Refreshing historical ERA5 month ",
      format(win_start, "%Y-%m"),
      " (", i, "/", nrow(repair_windows), ")."
    )

    blrTempMonth <- fetch_oiko_series(win_start, win_end, "temperature", "Temp", model = "era5")
    blrRainMonth <- fetch_oiko_series(win_start, win_end, "total_precipitation", "Rain", model = "era5")

    blrTemp <- replace_window(blrTemp, blrTempMonth, win_start, win_end)
    blrRain <- replace_window(blrRain, blrRainMonth, win_start, win_end)
  }
}

blrTemp <- dedupe_on_dt(blrTemp)
blrRain <- dedupe_on_dt(blrRain)
blrWind <- dedupe_on_dt(blrWind)

save(blrTemp, file=file.path(script_dir, 'data', 'bangaloreTemperature.RData'))
save(blrRain, file=file.path(script_dir, 'data', 'bangaloreRainfall.RData'))
save(blrWind, file=file.path(script_dir, 'data', 'bangaloreWind.RData'))

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

fmt_short_range <- function(start, end) {
  if (month(start) == month(end)) {
    paste0(format(start, "%b %d"), "-", format(end, "%d"))
  } else {
    paste0(format(start, "%b %d"), "-", format(end, "%b %d"))
  }
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

  build_daily_context <- function(temp_scope, rain_scope) {
    temp_scope %>%
      left_join(rain_scope %>% select(DT, Rain), by = "DT") %>%
      mutate(Rain = coalesce(Rain, 0)) %>%
      left_join(norms_temp_by_day, by = c("Month", "Day")) %>%
      left_join(norms_rain_by_day, by = c("Month", "Day")) %>%
      arrange(DT) %>%
      mutate(
        HighDev = High - NormalHigh,
        LowDev = Low - NormalLow,
        RainDev = Rain - NormalRain
      )
  }

  window_daily <- build_daily_context(recent_temp, recent_rain)

  window_n <- nrow(window_daily)
  total_rain <- sum(window_daily$Rain)
  rainy_days <- sum(window_daily$Rain >= 0.1)
  normal_total_rain <- sum(window_daily$NormalRain)
  normal_rainy_days <- mean(window_daily$NormalRain >= 0.1) * window_n

  label_window <- function(label, start, end, df = window_daily) {
    scoped <- df %>% filter(DT >= start, DT <= end)
    if (nrow(scoped) == 0) return(NULL)
    actual_rain <- sum(scoped$Rain)
    normal_rain <- sum(scoped$NormalRain)
    rain_pct_text <- if (normal_rain > 0) {
      paste0(
        "; ", sprintf("%.0f", 100 * (1 - actual_rain / normal_rain)),
        "% below normal; ", sprintf("%.0f", 100 * actual_rain / normal_rain),
        "% of normal"
      )
    } else {
      ""
    }
    paste0(
      label, " (", format(start, "%b %d"), " - ", format(end, "%b %d"), "): ",
      sprintf("%.1f", actual_rain), "mm rain vs normal ~",
      sprintf("%.1f", normal_rain), "mm", rain_pct_text, "; ",
      sum(scoped$Rain >= 0.1), " rainy days of ", nrow(scoped)
    )
  }

  month_start <- floor_date(end_date, "month")
  month_temp <- temp_daily %>% filter(DT >= month_start, DT <= end_date)
  month_rain <- rain_daily %>% filter(DT >= month_start, DT <= end_date)
  month_daily <- build_daily_context(month_temp, month_rain)

  named_window_text <- c(
    label_window(paste0("Last ", window_n, " days"), start_date, end_date),
    if (nrow(month_daily) > 0) {
      label_window(paste0(format(end_date, "%B"), " so far"), month_start, end_date, month_daily)
    } else {
      NULL
    }
  ) %>%
    discard(is.null) %>%
    paste(collapse = "\n")

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

  hist_temp_records <- hist_temp %>%
    summarise(RecHigh = max(High), RecLow = min(Low), .by = c(Month, Day))
  hist_rain_records <- hist_rain %>%
    summarise(RecRain = max(Rain), .by = c(Month, Day))

  ytd_temp <- temp_daily %>%
    filter(year(DT) == anchor_year, DT <= end_date)
  ytd_rain <- rain_daily %>%
    filter(year(DT) == anchor_year, DT <= end_date)

  temp_record_detail <- ytd_temp %>%
    left_join(hist_temp_records, by = c("Month", "Day")) %>%
    filter(!is.na(RecHigh) & (High > RecHigh | Low < RecLow)) %>%
    mutate(
      record_date = DT,
      record_type = case_when(
        High > RecHigh ~ "high",
        Low < RecLow ~ "low"
      ),
      value = case_when(
        record_type == "high" ~ High,
        record_type == "low" ~ Low
      ),
      detail = case_when(
        record_type == "high" ~ paste0(format(DT, "%b %d"), ": high of ", round(High, 1), "\u00B0C broke record of ", round(RecHigh, 1), "\u00B0C"),
        record_type == "low" ~ paste0(format(DT, "%b %d"), ": low of ", round(Low, 1), "\u00B0C broke record of ", round(RecLow, 1), "\u00B0C")
      )
    ) %>%
    select(record_date, record_type, value, detail)

  rain_record_detail <- ytd_rain %>%
    left_join(hist_rain_records, by = c("Month", "Day")) %>%
    filter(!is.na(RecRain) & Rain > RecRain & Rain > 0) %>%
    mutate(
      record_date = DT,
      record_type = "rain",
      value = Rain,
      detail = paste0(format(DT, "%b %d"), ": rain of ", round(Rain, 1), "mm broke record of ", round(RecRain, 1), "mm")
    ) %>%
    select(record_date, record_type, value, detail)

  record_detail <- bind_rows(temp_record_detail, rain_record_detail) %>%
    arrange(record_date, record_type)

  record_text <- if (nrow(record_detail) > 0) {
    paste0("Year-to-date record-breaking days (since 1981):\n", paste(record_detail$detail, collapse = "\n"), "\n")
  } else ""

  rain_pct_below <- if (normal_total_rain > 0) {
    sprintf("%.0f", 100 * (1 - total_rain / normal_total_rain))
  } else {
    NA_character_
  }
  rain_bullet <- if (!is.na(rain_pct_below)) {
    paste0(
      "* ", fmt_short_range(start_date, end_date), " was almost dry: ",
      sprintf("%.1f", total_rain), "mm vs ", sprintf("%.0f", normal_total_rain), "mm normal"
    )
  } else {
    paste0(
      "* ", fmt_short_range(start_date, end_date), " had ",
      sprintf("%.1f", total_rain), "mm rain"
    )
  }

  streak_bullet <- function(run, values, label) {
    if (is.null(run) || run$len < 3) return(NULL)
    scoped_values <- values[window_daily$DT >= run$start & window_daily$DT <= run$end]
    paste0(
      "* ", fmt_short_range(run$start, run$end), " ", label, " stayed warm: ",
      sprintf("%.1f", mean(scoped_values, na.rm = TRUE)), "\u00B0C above normal"
    )
  }

  warm_night_bullet <- streak_bullet(
    longest_run(window_daily$LowDev >= 2, window_daily$DT),
    window_daily$LowDev,
    "nights"
  )
  hot_day_bullet <- streak_bullet(
    longest_run(window_daily$HighDev >= 2, window_daily$DT),
    window_daily$HighDev,
    "highs"
  )
  record_bullet <- if (nrow(record_detail) > 0) {
    first_record <- record_detail %>%
      arrange(desc(record_date)) %>%
      slice(1)
    if (first_record$record_type[[1]] == "rain") {
      paste0(
        "* ", format(first_record$record_date[[1]], "%b %d"), " set a rain record: ",
        sprintf("%.1f", first_record$value[[1]]), "mm"
      )
    } else {
      paste0(
        "* ", format(first_record$record_date[[1]], "%b %d"), " set a date record: ",
        sprintf("%.1f", first_record$value[[1]]), "\u00B0C"
      )
    }
  } else {
    NULL
  }
  fallback_commentary <- c(rain_bullet, warm_night_bullet, hot_day_bullet, record_bullet) %>%
    discard(is.null) %>%
    head(3) %>%
    paste(collapse = "\n")

  facts_text <- paste0(
    "Period: ", format(start_date, "%b %d"), " - ", format(end_date, "%b %d, %Y"), "\n",
    "\nNAMED WINDOWS FOR WORDING\n",
    named_window_text, "\n",
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
    window_n = window_n,
    fallback_commentary = fallback_commentary
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

has_ambiguous_window_text <- function(text) {
  str_detect(
    str_to_lower(text),
    "\\b(this|that|the) window\\b|\\b(this|that|the) period\\b"
  )
}

has_overstated_weather_text <- function(text) {
  str_detect(
    str_to_lower(text),
    "\\bdrought\\b|\\bgripped\\b|\\bsevere\\b|\\btrapping heat\\b|\\bmonsoon\\b|\\bpre-?monsoon\\b|\\bparched\\b"
  )
}

has_muddled_subtitle_text <- function(text) {
  str_detect(
    str_to_lower(text),
    "\\botherwise\\b|\\binterrupted\\b|\\bfortnight\\b|;"
  )
}

# --- Recent commentary ---
recent_window <- 14
temp_end_date <- max(temp_daily$DT[temp_daily$DT < Sys.Date()])
rain_end_date <- max(rain_daily$DT[rain_daily$DT < Sys.Date()])
available_daily_dates <- intersect(temp_daily$DT, rain_daily$DT)
live_end_date <- max(available_daily_dates[available_daily_dates < Sys.Date()])
live_start_date <- live_end_date - (recent_window - 1)
live_context <- build_weather_window_text(live_start_date, live_end_date, temp_daily, rain_daily)
recent_facts <- live_context$facts
few_shot_messages <- build_few_shot_messages(script_dir, temp_daily, rain_daily)

chart_date_text <- if (identical(temp_end_date, rain_end_date)) {
  paste0("as of ", format(temp_end_date, "%B %d"))
} else {
  paste0(
    "temperature as of ", format(temp_end_date, "%B %d"),
    "; rain as of ", format(rain_end_date, "%B %d")
  )
}

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
      "Record-breaking high, low, and rain facts are year-to-date for the chart year, not just the recent commentary window. ",
      "Each bullet must be under 18 words, start with '\u2022 ', and use \u00B0C. ",
      "Never say 'this window', 'that window', 'the window', 'this period', or 'the period'. Use only named windows supplied in the facts, such as 'last 14 days', '", format(live_end_date, "%B"), " so far', exact dates like '", format(live_start_date, "%b %d"), " - ", format(live_end_date, "%b %d"), "', or logical phrases like 'late March'. ",
      "Any rainfall total or percentage you mention must match the named window in the same bullet. Do not calculate percentages yourself; use only the percentages supplied in NAMED WINDOWS FOR WORDING. ",
      "Do not infer named climate seasons such as monsoon or pre-monsoon unless the facts explicitly provide that label; use calendar wording instead. ",
      "Do not use dramatic labels such as drought, severe, gripped, or trapping heat; say dry, rain shortfall, or warmer nights instead. ",
      "Keep each bullet as one simple readable claim. Avoid semicolons, nested clauses, 'otherwise', 'interrupted', and 'fortnight'. ",
      "Write like an observant Bangalore resident, not a lab report. Prefer concrete phrasing like 'May is still almost dry', 'warm nights are sticking around', or 'May 14 set a heat record' when supported by the data. ",
      "Avoid bureaucratic phrasing such as 'rain shortfall of', 'against normal', 'totaled just', and 'the period saw'. ",
      "It is okay to use mild color like 'almost dry', 'barely any rain', 'warm nights', 'record heat', or 'rain missed Bangalore' when the numbers support it. ",
      "Plain but interesting tone. No hyperbole. Output only the 3 bullets, nothing else."
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
    else {
      candidate <- normalize_commentary_text(paste(trimws(bullet_lines), collapse = "\n"))
      if (has_ambiguous_window_text(candidate)) {
        message("Claude commentary used ambiguous window wording. Skipping commentary.")
        live_context$fallback_commentary
      } else if (has_overstated_weather_text(candidate)) {
        message("Claude commentary used overstated or inferred climate wording. Skipping commentary.")
        live_context$fallback_commentary
      } else if (has_muddled_subtitle_text(candidate)) {
        message("Claude commentary used muddled subtitle wording. Skipping commentary.")
        live_context$fallback_commentary
      } else {
        candidate
      }
    }
  }
}, error = function(e) { message("Claude API call failed: ", e$message); NULL })


temp_data <- build_temperature_plot_data(blrTemp, curr_year)
rain_data <- build_rain_plot_data(blrRain, curr_year)

combined <- render_weather_chart(
  temp_data = temp_data,
  rain_data = rain_data,
  curr_year = curr_year,
  title = paste0("Bangalore's Weather in ", curr_year, ", ", chart_date_text),
  subtitle = commentary,
  caption = "Data source: Oikolab"
)

outfile <- file.path(script_dir, 'charts', paste0('bangalore_weather_', format(Sys.Date(), '%Y%m%d'), '.png'))
ggsave(outfile, combined, width = 13.5, height = 7.5)
message("Saved: ", outfile)
combined
