library(tidyverse)
library(lubridate)
library(patchwork)
library(ggthemes)
library(scales)

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

root_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
out_dir <- file.path(script_dir, "data")
chart_dir <- file.path(script_dir, "charts")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(chart_dir, showWarnings = FALSE, recursive = TRUE)

load(file.path(root_dir, "data", "bangaloreTemperature.RData"))
load(file.path(root_dir, "data", "bangaloreRainfall.RData"))

temp_daily <- blrTemp %>%
  mutate(DT = as.Date(DT), Temp = as.numeric(Temp)) %>%
  filter(!is.na(Temp)) %>%
  filter(DT < Sys.Date()) %>%
  summarise(High = max(Temp), Low = min(Temp), .by = DT) %>%
  mutate(Year = year(DT), Month = month(DT), Day = day(DT))

rain_daily <- blrRain %>%
  mutate(DT = as.Date(DT), Rain = as.numeric(Rain)) %>%
  filter(!is.na(Rain)) %>%
  filter(DT < Sys.Date()) %>%
  summarise(Rain = sum(Rain), .by = DT) %>%
  mutate(Year = year(DT), Month = month(DT), Day = day(DT))

all_daily <- temp_daily %>%
  left_join(rain_daily %>% select(DT, Rain), by = "DT") %>%
  mutate(Rain = coalesce(Rain, 0))

rain_runs <- rain_daily %>%
  arrange(DT) %>%
  mutate(
    dry_group = cumsum(Rain >= 0.1),
    wet_group = cumsum(Rain < 0.1)
  ) %>%
  group_by(dry_group) %>%
  mutate(dry_run_end = row_number() * as.integer(Rain < 0.1)) %>%
  ungroup() %>%
  group_by(wet_group) %>%
  mutate(wet_run_end = row_number() * as.integer(Rain >= 0.1)) %>%
  ungroup() %>%
  select(DT, dry_run_end, wet_run_end)

dry_lookup <- setNames(rain_runs$dry_run_end, as.character(rain_runs$DT))
wet_lookup <- setNames(rain_runs$wet_run_end, as.character(rain_runs$DT))

longest_run_info <- function(flags, dates) {
  flags <- replace(flags, is.na(flags), FALSE)
  if (!any(flags)) {
    return(list(len = 0L, start = as.Date(NA), end = as.Date(NA)))
  }
  rr <- rle(flags)
  idx <- which(rr$values)
  best <- idx[which.max(rr$lengths[idx])]
  end_pos <- sum(rr$lengths[seq_len(best)])
  start_pos <- end_pos - rr$lengths[best] + 1
  list(len = rr$lengths[best], start = dates[start_pos], end = dates[end_pos])
}

max_roll_sum <- function(x, k) {
  if (length(x) < k) return(NA_real_)
  max(vapply(seq_len(length(x) - k + 1), function(i) sum(x[i:(i + k - 1)]), numeric(1)))
}

year_windows <- function(target_year, horizons = c(7L, 14L, 30L)) {
  message("Scoring ", target_year)
  hist_temp <- temp_daily %>% filter(Year < target_year)
  hist_rain <- rain_daily %>% filter(Year < target_year)
  curr_temp <- temp_daily %>% filter(Year == target_year)
  curr_rain <- rain_daily %>% filter(Year == target_year)

  if (nrow(hist_temp) == 0 || nrow(hist_rain) == 0 || nrow(curr_temp) == 0) {
    return(tibble())
  }

  norms_temp <- hist_temp %>%
    summarise(NormalHigh = mean(High), NormalLow = mean(Low), .by = c(Month, Day))
  norms_rain <- hist_rain %>%
    summarise(NormalRain = sum(Rain) / n_distinct(Year), .by = c(Month, Day))
  hist_records <- hist_temp %>%
    summarise(RecHigh = max(High), RecLow = min(Low), .by = c(Month, Day))

  curr_daily <- curr_temp %>%
    left_join(curr_rain %>% select(DT, Rain), by = "DT") %>%
    mutate(Rain = coalesce(Rain, 0)) %>%
    left_join(norms_temp, by = c("Month", "Day")) %>%
    left_join(norms_rain, by = c("Month", "Day")) %>%
    left_join(hist_records, by = c("Month", "Day")) %>%
    arrange(DT) %>%
    mutate(
      HighDev = High - NormalHigh,
      LowDev = Low - NormalLow,
      RecordBreak = High > RecHigh | Low < RecLow
    )

  map_dfr(horizons, function(h) {
    if (nrow(curr_daily) < h) return(tibble())
    map_dfr(seq.int(h, nrow(curr_daily)), function(end_idx) {
      wd <- curr_daily[(end_idx - h + 1):end_idx, ]
      first_rain_idx <- which(wd$Rain >= 0.1)[1]
      first_dry_idx <- which(wd$Rain < 0.1)[1]
      dry_cutoff <- if (is.na(first_rain_idx)) wd$DT[nrow(wd)] else wd$DT[first_rain_idx] - 1
      wet_cutoff <- if (is.na(first_dry_idx)) wd$DT[nrow(wd)] else wd$DT[first_dry_idx] - 1
      ante_dry <- unname(dry_lookup[as.character(dry_cutoff)])
      ante_wet <- unname(wet_lookup[as.character(wet_cutoff)])
      ante_dry <- ifelse(is.na(ante_dry), 0L, ante_dry)
      ante_wet <- ifelse(is.na(ante_wet), 0L, ante_wet)
      dry_run <- longest_run_info(wd$Rain < 0.1, wd$DT)
      wet_run <- longest_run_info(wd$Rain >= 0.1, wd$DT)
      hot_run <- longest_run_info(wd$HighDev >= 2, wd$DT)
      cool_run <- longest_run_info(wd$HighDev <= -2, wd$DT)
      warm_night_run <- longest_run_info(wd$LowDev >= 2, wd$DT)
      cool_night_run <- longest_run_info(wd$LowDev <= -2, wd$DT)

      tibble(
        year = target_year,
        horizon = h,
        start_date = wd$DT[1],
        end_date = wd$DT[nrow(wd)],
        avg_high = mean(wd$High),
        avg_low = mean(wd$Low),
        avg_high_dev = mean(wd$HighDev),
        avg_low_dev = mean(wd$LowDev),
        total_rain = sum(wd$Rain),
        normal_total_rain = sum(wd$NormalRain),
        rain_excess = sum(wd$Rain) - sum(wd$NormalRain),
        rainy_days = sum(wd$Rain >= 0.1),
        normal_rainy_days = sum(wd$NormalRain >= 0.1),
        wettest_2day = max_roll_sum(wd$Rain, 2),
        wettest_3day = max_roll_sum(wd$Rain, 3),
        wettest_5day = max_roll_sum(wd$Rain, min(5, h)),
        concentrated_rain_share = ifelse(sum(wd$Rain) > 0, max_roll_sum(wd$Rain, min(3, h)) / sum(wd$Rain), 0),
        dry_streak = dry_run$len,
        wet_streak = wet_run$len,
        hot_streak = hot_run$len,
        cool_streak = cool_run$len,
        warm_night_streak = warm_night_run$len,
        cool_night_streak = cool_night_run$len,
        ante_dry = ante_dry,
        ante_wet = ante_wet,
        record_days = sum(wd$RecordBreak),
        max_high_dev = max(wd$HighDev),
        min_high_dev = min(wd$HighDev),
        max_low_dev = max(wd$LowDev),
        min_low_dev = min(wd$LowDev)
      )
    })
  })
}

candidate_years <- 2000:max(temp_daily$Year)

candidate_windows <- map_dfr(candidate_years, year_windows) %>%
  mutate(
    rain_ratio = ifelse(normal_total_rain > 0.1, total_rain / normal_total_rain, NA_real_),
    month_label = format(start_date, "%b"),
    season = case_when(
      month(start_date) %in% 3:5 ~ "pre-monsoon",
      month(start_date) %in% 6:9 ~ "monsoon",
      month(start_date) %in% c(10, 11) ~ "post-monsoon",
      TRUE ~ "winter"
    )
  )

families <- tribble(
  ~family, ~label, ~description, ~window_note,
  "rain_burst_after_dry", "Dry spell broken by a sharp burst", "One short rain episode arrives after a long preceding dry run.", "The story is the break, not the total.",
  "sustained_wet", "Sustained wet stretch", "Rain is distributed across many days, not concentrated in one spike.", "A longer window is needed so the persistence is visible.",
  "concentrated_rain", "Rain concentrated into one short spell", "A period looks wet on total rainfall, but nearly all of it comes from one brief burst.", "Shorter framing prevents one burst from being misread as a wet fortnight.",
  "persistent_dry", "Persistently dry window", "Dryness itself is the main signal, relative to what is normal for the season.", "A wider window is useful because absence is cumulative.",
  "hot_spell", "Daytime heat spell", "High temperatures sit well above normal for several consecutive days.", "The run-length matters more than any single hottest day.",
  "warm_nights", "Warm-night run", "Night temperatures stay unusually elevated even if daytime highs are not extreme.", "This needs enough days for the night pattern to be unmistakable.",
  "cool_spell", "Cool daytime spell", "Highs run meaningfully below normal for a sustained stretch.", "This is about persistent daytime coolness, not a one-day dip.",
  "cool_nights", "Cool-night run", "Night temperatures stay below normal for several days.", "The window is chosen to preserve the night-time pattern.",
  "record_cluster", "Record-heavy patch", "Several days in the window broke prior calendar-date records.", "The record cluster is the editorial hook.",
  "wet_month_build", "Wet month building through accumulation", "The signal is cumulative rainfall excess over a month, not one isolated day.", "A 30-day view is needed because the anomaly accumulates.",
  "dry_month_build", "Dry month building through accumulation", "The signal is a running monthly rainfall shortfall.", "A 30-day view is needed because the deficit builds gradually.",
  "mixed_whiplash", "Heat-to-rain whiplash", "The same window contains meaningful heat and then a clear rain break.", "The chosen span keeps both parts of the story visible."
)

family_candidates <- list(
  rain_burst_after_dry = candidate_windows %>%
    filter(horizon %in% c(7, 14), ante_dry >= 12, total_rain >= 8, concentrated_rain_share >= 0.6) %>%
    mutate(score = ante_dry + total_rain + 10 * concentrated_rain_share),
  sustained_wet = candidate_windows %>%
    filter(horizon %in% c(14, 30), wet_streak >= pmax(4, horizon * 0.5), total_rain >= normal_total_rain * 1.5) %>%
    mutate(score = wet_streak + total_rain / 10),
  concentrated_rain = candidate_windows %>%
    filter(horizon %in% c(7, 14), total_rain >= 12, rainy_days <= 3, concentrated_rain_share >= 0.75) %>%
    mutate(score = total_rain + 15 * concentrated_rain_share - rainy_days),
  persistent_dry = candidate_windows %>%
    filter(horizon %in% c(14, 30), dry_streak >= pmax(8, horizon * 0.7), total_rain <= pmax(1, normal_total_rain * 0.25)) %>%
    mutate(score = dry_streak + pmax(normal_total_rain - total_rain, 0)),
  hot_spell = candidate_windows %>%
    filter(horizon %in% c(7, 14), hot_streak >= 4, avg_high_dev >= 1.5) %>%
    mutate(score = hot_streak + 2 * avg_high_dev + pmax(max_high_dev, 0)),
  warm_nights = candidate_windows %>%
    filter(horizon %in% c(7, 14), warm_night_streak >= 4, avg_low_dev >= 1.5) %>%
    mutate(score = warm_night_streak + 2 * avg_low_dev + pmax(max_low_dev, 0)),
  cool_spell = candidate_windows %>%
    filter(horizon %in% c(7, 14), cool_streak >= 4, avg_high_dev <= -1.5) %>%
    mutate(score = cool_streak + 2 * abs(avg_high_dev) + abs(min_high_dev)),
  cool_nights = candidate_windows %>%
    filter(horizon %in% c(7, 14), cool_night_streak >= 4, avg_low_dev <= -1.5) %>%
    mutate(score = cool_night_streak + 2 * abs(avg_low_dev) + abs(min_low_dev)),
  record_cluster = candidate_windows %>%
    filter(horizon %in% c(7, 14), record_days >= 2) %>%
    mutate(score = 4 * record_days + pmax(abs(avg_high_dev), abs(avg_low_dev))),
  wet_month_build = candidate_windows %>%
    filter(horizon == 30, total_rain >= normal_total_rain * 2, wet_streak >= 5) %>%
    mutate(score = total_rain / 10 + wet_streak + 2 * rain_ratio),
  dry_month_build = candidate_windows %>%
    filter(horizon == 30, total_rain <= pmax(2, normal_total_rain * 0.2), dry_streak >= 15) %>%
    mutate(score = dry_streak + pmax(normal_total_rain - total_rain, 0) / 5),
  mixed_whiplash = candidate_windows %>%
    filter(horizon %in% c(7, 14), hot_streak >= 3, total_rain >= 8, ante_dry >= 7, concentrated_rain_share >= 0.5) %>%
    mutate(score = hot_streak + ante_dry / 2 + total_rain / 5 + 5 * concentrated_rain_share)
)

date_overlap_ratio <- function(a_start, a_end, b_start, b_end) {
  overlap <- as.numeric(min(a_end, b_end) - max(a_start, b_start) + 1)
  if (overlap <= 0) return(0)
  span <- min(as.numeric(a_end - a_start + 1), as.numeric(b_end - b_start + 1))
  overlap / span
}

selected <- list()
selected_tbl <- tibble()

for (family_name in names(family_candidates)) {
  fam_tbl <- family_candidates[[family_name]] %>%
    arrange(desc(score), desc(end_date))
  if (nrow(fam_tbl) == 0) next
  pick_idx <- NA_integer_
  for (i in seq_len(nrow(fam_tbl))) {
    cand <- fam_tbl[i, ]
    overlaps <- if (nrow(selected_tbl) == 0) {
      FALSE
    } else {
      map_lgl(seq_len(nrow(selected_tbl)), function(j) {
        other <- selected_tbl[j, ]
        date_overlap_ratio(cand$start_date, cand$end_date, other$start_date, other$end_date) > 0.6
      })
    }
    if (!any(overlaps)) {
      pick_idx <- i
      break
    }
  }
  if (is.na(pick_idx)) next
  selected_tbl <- bind_rows(selected_tbl, fam_tbl[pick_idx, ] %>% mutate(family = family_name))
}

selected_tbl <- selected_tbl %>%
  left_join(families, by = "family") %>%
  arrange(match(family, families$family)) %>%
  mutate(
    example_id = sprintf("Example %02d", row_number()),
    chart_file = file.path("charts", paste0(str_replace_all(family, "_", "-"), "_", format(start_date, "%Y%m%d"), "_", horizon, "d.png")),
    why_useful = case_when(
      family == "rain_burst_after_dry" ~ "Totals alone are misleading; the key is the sudden break after dryness.",
      family == "sustained_wet" ~ "This teaches the model to recognise persistence rather than a single heavy day.",
      family == "concentrated_rain" ~ "This teaches the model not to call a window broadly wet when the rain is concentrated.",
      family == "persistent_dry" ~ "This teaches the model to treat absence of rain as a real signal.",
      family == "hot_spell" ~ "This teaches the model to prioritise a clear daytime heat run.",
      family == "warm_nights" ~ "This teaches the model not to miss elevated night temperatures when highs are less dramatic.",
      family == "cool_spell" ~ "This teaches the model to spot sustained cool highs relative to normal.",
      family == "cool_nights" ~ "This teaches the model to separate cool nights from cool days.",
      family == "record_cluster" ~ "This teaches the model to foreground records when they cluster.",
      family == "wet_month_build" ~ "This teaches the model that some stories are cumulative and need a month-long frame.",
      family == "dry_month_build" ~ "This teaches the model that rainfall deficits often need a month-long frame.",
      family == "mixed_whiplash" ~ "This teaches the model to keep two linked signals in view instead of flattening them.",
      TRUE ~ "This teaches the model to choose the right frame for the signal."
    ),
    key_facts = pmap_chr(
      list(horizon, total_rain, normal_total_rain, rainy_days, ante_dry, wet_streak, dry_streak,
           hot_streak, warm_night_streak, cool_streak, cool_night_streak, record_days),
      function(horizon, total_rain, normal_total_rain, rainy_days, ante_dry, wet_streak, dry_streak,
               hot_streak, warm_night_streak, cool_streak, cool_night_streak, record_days) {
        facts <- c(
          sprintf("Window length: %s days", horizon),
          sprintf("Rain: %.1fmm vs normal %.1fmm", total_rain, normal_total_rain),
          sprintf("Rainy days: %s", rainy_days)
        )
        extras <- c(
          if (ante_dry >= 7) sprintf("Antecedent dry streak: %s days", ante_dry),
          if (wet_streak >= 4) sprintf("Wet streak in window: %s days", wet_streak),
          if (dry_streak >= 7) sprintf("Dry streak in window: %s days", dry_streak),
          if (hot_streak >= 4) sprintf("Hot-day streak: %s days", hot_streak),
          if (warm_night_streak >= 4) sprintf("Warm-night streak: %s days", warm_night_streak),
          if (cool_streak >= 4) sprintf("Cool-day streak: %s days", cool_streak),
          if (cool_night_streak >= 4) sprintf("Cool-night streak: %s days", cool_night_streak),
          if (record_days >= 1) sprintf("Record days: %s", record_days)
        )
        paste(c(facts, extras), collapse = " | ")
      }
    )
  )

render_window_card <- function(example_row) {
  wd <- all_daily %>%
    filter(DT >= example_row$start_date, DT <= example_row$end_date) %>%
    mutate(Month = month(DT), Day = day(DT))

  hist_temp <- temp_daily %>% filter(Year < year(example_row$start_date))
  hist_rain <- rain_daily %>% filter(Year < year(example_row$start_date))
  norms_temp <- hist_temp %>%
    summarise(NormalHigh = mean(High), NormalLow = mean(Low), .by = c(Month, Day))
  norms_rain <- hist_rain %>%
    summarise(NormalRain = sum(Rain) / n_distinct(Year), .by = c(Month, Day))

  wd <- wd %>%
    left_join(norms_temp, by = c("Month", "Day")) %>%
    left_join(norms_rain, by = c("Month", "Day")) %>%
    mutate(day_lab = format(DT, "%b %d"))

  temp_plot <- ggplot(wd, aes(x = DT)) +
    geom_linerange(aes(ymin = NormalLow, ymax = NormalHigh), linewidth = 5, color = "#d4cbaa", alpha = 0.95) +
    geom_linerange(aes(ymin = Low, ymax = High), linewidth = 2.1, color = "#490000") +
    geom_point(aes(y = High), color = "#490000", size = 1.4) +
    geom_point(aes(y = Low), color = "#490000", size = 1.4) +
    scale_x_date("", date_labels = "%b %d", breaks = wd$DT) +
    scale_y_continuous("Temperature (°C)") +
    theme_tufte(base_size = 11) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(face = "bold", color = "#3C3C3C"),
      axis.line = element_line(color = "#3C3C3C", linewidth = 0.2),
      plot.margin = margin(12, 12, 4, 12)
    )

  rain_plot <- ggplot(wd, aes(x = DT)) +
    geom_col(aes(y = Rain), fill = "#005566", width = 0.8) +
    geom_line(aes(y = NormalRain), color = "#888888", linewidth = 0.7) +
    geom_point(aes(y = NormalRain), color = "#888888", size = 1) +
    scale_x_date("", date_labels = "%b %d", breaks = wd$DT) +
    scale_y_continuous("Rain (mm)", labels = label_number(accuracy = 1)) +
    theme_tufte(base_size = 11) +
    theme(
      axis.text.x = element_text(face = "bold", color = "#3C3C3C", angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold", color = "#3C3C3C"),
      axis.line = element_line(color = "#3C3C3C", linewidth = 0.2),
      plot.margin = margin(4, 12, 12, 12)
    )

  subtitle <- paste0(
    format(example_row$start_date, "%b %d, %Y"), " - ", format(example_row$end_date, "%b %d, %Y"),
    "  |  ", example_row$horizon, "-day window  |  ", example_row$window_note
  )

  caption <- paste(
    sprintf("Avg high dev: %+0.1f°C", example_row$avg_high_dev),
    sprintf("Avg low dev: %+0.1f°C", example_row$avg_low_dev),
    sprintf("Rain: %.1fmm vs %.1fmm normal", example_row$total_rain, example_row$normal_total_rain),
    sep = "   "
  )

  card <- temp_plot / rain_plot +
    plot_layout(heights = c(2.2, 1.4)) +
    plot_annotation(
      title = paste0(example_row$example_id, ": ", example_row$label),
      subtitle = subtitle,
      caption = caption,
      theme = theme(
        plot.title = element_text(face = "bold", color = "#3C3C3C", size = 14),
        plot.subtitle = element_text(color = "#5f3946", size = 10),
        plot.caption = element_text(color = "#3C3C3C", size = 9),
        plot.background = element_rect(fill = "#e5e1d8", color = NA)
      )
    ) &
    theme(
      plot.background = element_rect(fill = "#e5e1d8", color = NA),
      panel.background = element_rect(fill = "#e5e1d8", color = NA)
    )

  ggsave(
    filename = file.path(script_dir, example_row$chart_file),
    plot = card,
    width = 11,
    height = 6.8,
    dpi = 180
  )
}

if (nrow(selected_tbl) > 0) {
  walk(split(selected_tbl, seq_len(nrow(selected_tbl))), ~ render_window_card(.x))
}

write_csv(candidate_windows, file.path(out_dir, "candidate_windows.csv"))
write_csv(selected_tbl, file.path(out_dir, "selected_situations.csv"))

feedback_sheet <- selected_tbl %>%
  transmute(
    example_id,
    family,
    label,
    chart_file,
    start_date,
    end_date,
    horizon,
    why_this_case_matters = why_useful,
    why_this_window_length = window_note,
    facts_to_read = sprintf(
      "Rain %.1fmm vs %.1fmm normal | avg high dev %+0.1f°C | avg low dev %+0.1f°C | rainy days %s | wettest 3-day stretch %.1fmm | antecedent dry streak %s days | record days %s",
      total_rain, normal_total_rain, avg_high_dev, avg_low_dev, rainy_days, wettest_3day, ante_dry, record_days
    ),
    your_headline = "",
    what_claude_should_learn = "",
    extra_notes = ""
  )

write_csv(feedback_sheet, file.path(out_dir, "feedback_sheet.csv"))

md_lines <- c(
  "# Few-Shot Weather Situation Cards",
  "",
  "These are candidate training examples for improving the Bangalore weather subtitle annotations.",
  "Each example is a real historical window chosen to teach a different editorial decision, and the window length varies with the signal.",
  "",
  "## How To Use This",
  "",
  "For each example, fill in:",
  "- `Your headline`",
  "- `What Claude should learn`",
  "",
  "Keep the headline to one line. The purpose is to teach framing, not to draft all three live bullets.",
  "",
  "## Situation Families",
  ""
)

for (i in seq_len(nrow(families))) {
  fam <- families[i, ]
  md_lines <- c(
    md_lines,
    paste0("- **", fam$label, "**: ", fam$description, " ", fam$window_note)
  )
}

md_lines <- c(md_lines, "", "## Example Cards", "")

for (i in seq_len(nrow(selected_tbl))) {
  ex <- selected_tbl[i, ]
  facts <- c(
    sprintf("- Window: %s to %s", format(ex$start_date, "%b %d, %Y"), format(ex$end_date, "%b %d, %Y")),
    sprintf("- Situation: %s", ex$label),
    sprintf("- Why this case matters: %s", ex$why_useful),
    sprintf("- Why this window length: %s", ex$window_note),
    sprintf("- Facts to read: Rain %.1fmm vs %.1fmm normal; avg high dev %+0.1f°C; avg low dev %+0.1f°C; rainy days %s; wettest 3-day stretch %.1fmm; antecedent dry streak %s days; record days %s",
            ex$total_rain, ex$normal_total_rain, ex$avg_high_dev, ex$avg_low_dev,
            ex$rainy_days, ex$wettest_3day, ex$ante_dry, ex$record_days),
    "- Your headline:",
    "",
    "- What Claude should learn:",
    ""
  )
  md_lines <- c(
    md_lines,
    paste0("### ", ex$example_id, " — ", ex$label),
    "",
    paste0("![](", ex$chart_file, ")"),
    "",
    facts,
    ""
  )
}

writeLines(md_lines, file.path(script_dir, "situation_cards.md"))

html_lines <- c(
  "<!doctype html>",
  "<html>",
  "<head>",
  "<meta charset='utf-8'>",
  "<title>Bangalore weather few-shot review</title>",
  "<style>",
  "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 32px auto; max-width: 1100px; color: #222; background: #f7f4ed; }",
  "h1, h2, h3 { color: #3c3c3c; }",
  ".card { background: white; padding: 20px; margin: 0 0 28px 0; border: 1px solid #ddd6c8; box-shadow: 0 1px 4px rgba(0,0,0,0.04); }",
  ".meta { color: #6b4651; margin-bottom: 12px; }",
  "img { width: 100%; height: auto; border: 1px solid #e3dccd; }",
  "ul { margin-top: 10px; }",
  "li { margin: 6px 0; }",
  ".prompt { font-weight: 600; margin-top: 12px; }",
  ".small { color: #666; font-size: 14px; }",
  "</style>",
  "</head>",
  "<body>",
  "<h1>Bangalore weather few-shot review</h1>",
  "<p class='small'>See the charts here. Enter your responses in <code>fewshot_annotations/data/feedback_sheet.csv</code> using the matching <code>example_id</code>.</p>"
)

for (i in seq_len(nrow(selected_tbl))) {
  ex <- selected_tbl[i, ]
  html_lines <- c(
    html_lines,
    "<div class='card'>",
    paste0("<h2>", ex$example_id, " — ", ex$label, "</h2>"),
    paste0("<div class='meta'>", format(ex$start_date, "%b %d, %Y"), " to ", format(ex$end_date, "%b %d, %Y"),
           " | ", ex$horizon, "-day window</div>"),
    paste0("<img src='", ex$chart_file, "' alt='", ex$example_id, "'>"),
    "<ul>",
    paste0("<li><strong>Why this case matters:</strong> ", ex$why_useful, "</li>"),
    paste0("<li><strong>Why this window length:</strong> ", ex$window_note, "</li>"),
    sprintf("<li><strong>Facts to read:</strong> Rain %.1fmm vs %.1fmm normal; avg high dev %+0.1f°C; avg low dev %+0.1f°C; rainy days %s; wettest 3-day stretch %.1fmm; antecedent dry streak %s days; record days %s</li>",
            ex$total_rain, ex$normal_total_rain, ex$avg_high_dev, ex$avg_low_dev,
            ex$rainy_days, ex$wettest_3day, ex$ante_dry, ex$record_days),
    "</ul>",
    "<div class='prompt'>Fill these in inside <code>feedback_sheet.csv</code>:</div>",
    "<ul>",
    "<li><strong>your_headline</strong></li>",
    "<li><strong>what_claude_should_learn</strong></li>",
    "<li><strong>extra_notes</strong> (optional)</li>",
    "</ul>",
    "</div>"
  )
}

html_lines <- c(html_lines, "</body>", "</html>")
writeLines(html_lines, file.path(script_dir, "review_cards.html"))
