weather_palette <- list(
  background = "#e5e1d8",
  actual = "#5f3946",
  record = "#c8c0aa",
  normal = "#9c9280",
  text = "#3C3C3C",
  band_outer = "#d6cfbd",
  band_inner = "#b9b09d"
)

perform_claude_request_with_retries <- function(req, max_attempts = 4, initial_sleep = 5) {
  transient_statuses <- c(408, 409, 429, 500, 502, 503, 504, 529)

  for (attempt in seq_len(max_attempts)) {
    resp <- tryCatch(
      req %>%
        httr2::req_error(is_error = ~ FALSE) %>%
        httr2::req_perform(),
      error = function(e) {
        if (attempt == max_attempts) stop(e)
        message(
          "Claude API call failed on attempt ", attempt, "/", max_attempts,
          ": ", e$message, ". Retrying."
        )
        NULL
      }
    )

    if (is.null(resp)) {
      Sys.sleep(initial_sleep * attempt)
      next
    }

    status <- httr2::resp_status(resp)
    if (!(status %in% transient_statuses) || attempt == max_attempts) {
      return(resp)
    }

    message(
      "Claude API returned transient status ", status,
      " on attempt ", attempt, "/", max_attempts, ". Retrying."
    )
    Sys.sleep(initial_sleep * attempt)
  }
}

weather_month_midpoints <- function(curr_year) {
  tibble::tibble(
    x = seq(as.Date(paste0(curr_year, "-01-15")), by = "1 month", length.out = 12),
    label = toupper(month.name)
  )
}

weather_month_starts <- function(curr_year) {
  seq(as.Date(paste0(curr_year, "-02-01")), by = "1 month", length.out = 11)
}

weather_chart_theme <- function(base_size = 10, x_position = "bottom") {
  ggthemes::theme_tufte(base_family = "serif", base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_line(color = weather_palette$text, linewidth = 0.2),
      axis.text = ggplot2::element_text(color = weather_palette$text),
      axis.text.x = ggplot2::element_text(face = "bold", size = rel(0.95)),
      axis.text.y = ggplot2::element_text(size = rel(0.95)),
      axis.line.y.left = ggplot2::element_line(color = weather_palette$normal, linewidth = 1),
      axis.line.y.right = ggplot2::element_line(color = weather_palette$normal, linewidth = 1),
      axis.line.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = weather_palette$background, color = NA),
      plot.background = ggplot2::element_rect(fill = weather_palette$background, color = NA),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

build_temperature_plot_data <- function(blrTemp, curr_year) {
  year_max_or_na <- function(values, years, target_year) {
    vals <- values[years == target_year]
    if (length(vals) == 0 || all(is.na(vals))) return(NA_real_)
    max(vals, na.rm = TRUE)
  }

  year_min_or_na <- function(values, years, target_year) {
    vals <- values[years == target_year]
    if (length(vals) == 0 || all(is.na(vals))) return(NA_real_)
    min(vals, na.rm = TRUE)
  }

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
      ActualHigh = year_max_or_na(Temp, year(DT), curr_year),
      ActualLow = year_min_or_na(Temp, year(DT), curr_year),
      .by = c(Month, Day)
    ) %>%
    mutate(
      Date = make_date(curr_year, Month, Day),
      ActualHigh = ifelse(is.infinite(ActualHigh), NormalHigh, ActualHigh),
      ActualLow = ifelse(is.infinite(ActualLow), NormalLow, ActualLow),
      RecordType = case_when(
        ActualHigh >= RecordHigh ~ "high",
        ActualLow <= RecordLow ~ "low",
        .default = NA_character_
      ),
      Special = case_when(
        ActualHigh >= RecordHigh ~ paste("Hottest", format(Date, "%b %d"), "since 1981"),
        ActualLow <= RecordLow ~ paste("Coldest", format(Date, "%b %d"), "since 1981"),
        .default = ""
      ),
      xmin = Date - 0.48,
      xmax = Date + 0.48
    )
}

build_rain_plot_data <- function(blrRain, curr_year) {
  daily <- blrRain %>%
    mutate(
      Rain = as.numeric(Rain),
      DT = as.Date(DT)
    ) %>%
    summarise(Rain = sum(Rain), .by = DT) %>%
    mutate(
      Year = year(DT),
      Month = month(DT),
      Day = day(DT),
      MonthStart = floor_date(DT, "month")
    ) %>%
    mutate(
      MonthlyRain = sum(Rain),
      .by = c(Year, Month)
    ) %>%
    mutate(
      CumulRain = cumsum(Rain),
      .by = c(Year, Month)
    )

  monthly_normals <- daily %>%
    summarise(
      MonthlyAvg = mean(MonthlyRain),
      MonthlyMax = max(MonthlyRain),
      .by = Month
    )

  daily_maxima <- daily %>%
    summarise(DailyMax = max(Rain), .by = c(Month, Day))

  cumulative_range <- daily %>%
    mutate(
      CumulP10 = quantile(CumulRain, 0.10, na.rm = TRUE),
      CumulP25 = quantile(CumulRain, 0.25, na.rm = TRUE),
      CumulMedian = quantile(CumulRain, 0.50, na.rm = TRUE),
      CumulP75 = quantile(CumulRain, 0.75, na.rm = TRUE),
      CumulP90 = quantile(CumulRain, 0.90, na.rm = TRUE),
      CumulMin = quantile(CumulRain, 0.10, na.rm = TRUE),
      CumulMax = quantile(CumulRain, 0.90, na.rm = TRUE),
      .by = c(Month, Day)
    ) %>%
    distinct(Month, Day, CumulP10, CumulP25, CumulMedian, CumulP75, CumulP90, CumulMin, CumulMax)

  monthly_peak_days <- daily %>%
    filter(Year == curr_year, Rain > 0) %>%
    slice_max(order_by = Rain, n = 1, with_ties = FALSE, by = Month) %>%
    transmute(Month, PeakDay = Day)

  daily %>%
    filter(Year == curr_year) %>%
    left_join(monthly_normals, by = "Month") %>%
    left_join(daily_maxima, by = c("Month", "Day")) %>%
    left_join(cumulative_range, by = c("Month", "Day")) %>%
    left_join(monthly_peak_days, by = "Month") %>%
    mutate(
      MonthEnd = ceiling_date(MonthStart, "month"),
      normalLabel = case_when(
        Day == 1 & Month == 10 ~ paste("Normal", round(MonthlyAvg, 0), sep = "\n"),
        Day == 1 ~ as.character(round(MonthlyAvg, 0)),
        .default = ""
      ),
      actualLabel = case_when(
        Day == max(Day) & Month == 10 ~ paste("Actual", round(CumulRain, 0), sep = "\n"),
        Day == max(Day) ~ as.character(round(CumulRain, 0)),
        .default = ""
      ),
      MonthLabel = ifelse(CumulRain == MonthlyMax & Day == 15, paste("Wettest", month.name[Month], "since 1981"), ""),
      DayLabel = ifelse(Day == PeakDay & Rain == DailyMax & Rain > 0, paste0("Record ", month.name[Month], " ", Day, "; ", round(Rain, 0), " mm"), ""),
      Label = case_when(
        DayLabel != "" ~ DayLabel,
        MonthLabel != "" ~ MonthLabel,
        .default = ""
      )
    )
}

build_temperature_legend <- function(temp_data, curr_year, title_y = NULL) {
  candidate_rows <- temp_data %>%
    filter(!is.na(ActualHigh), !is.na(ActualLow)) %>%
    mutate(
      ActualRange = ActualHigh - ActualLow,
      dist_to_midyear = abs(as.numeric(Date - as.Date(paste0(curr_year, "-07-07"))))
    ) %>%
    arrange(desc(ActualRange), dist_to_midyear)

  if (nrow(candidate_rows) == 0) return(NULL)
  legend_row <- candidate_rows %>% slice(1)
  legend_date <- legend_row$Date[[1]]

  y_top <- if (is.null(title_y)) max(temp_data$RecordHigh, na.rm = TRUE) else title_y
  y_bottom <- min(temp_data$RecordLow, na.rm = TRUE)
  legend_pad <- max(2, (y_top - y_bottom) * 0.04)
  min_gap <- max(1.8, (y_top - y_bottom) * 0.06)
  high_label_y <- max(legend_row$ActualHigh, legend_row$ActualLow + min_gap / 2)
  low_label_y <- min(legend_row$ActualLow, high_label_y - min_gap)
  mid_range_y <- mean(c(legend_row$NormalLow, legend_row$NormalHigh))

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = legend_row,
      ggplot2::aes(xmin = Date - 0.30, xmax = Date + 0.30, ymin = RecordLow, ymax = RecordHigh),
      fill = weather_palette$record,
      color = NA
    ) +
    ggplot2::geom_rect(
      data = legend_row,
      ggplot2::aes(xmin = Date - 0.22, xmax = Date + 0.22, ymin = NormalLow, ymax = NormalHigh),
      fill = weather_palette$normal,
      color = NA
    ) +
    ggplot2::geom_rect(
      data = legend_row,
      ggplot2::aes(xmin = Date - 0.14, xmax = Date + 0.14, ymin = ActualLow, ymax = ActualHigh),
      fill = weather_palette$actual,
      color = NA
    ) +
    ggplot2::annotate("text", x = legend_date - 2.2, y = legend_row$RecordHigh, label = "RECORD HIGH",
      hjust = 1, vjust = 0.5, family = "serif", fontface = "bold", size = 2.1, color = weather_palette$record) +
    ggplot2::annotate("text", x = legend_date - 2.2, y = legend_row$RecordLow, label = "RECORD LOW",
      hjust = 1, vjust = 0.5, family = "serif", fontface = "bold", size = 2.1, color = weather_palette$record) +
    ggplot2::annotate("segment", x = legend_date + 0.35, xend = legend_date + 6.5,
      y = legend_row$ActualHigh, yend = high_label_y, linewidth = 0.3, color = weather_palette$text) +
    ggplot2::annotate("segment", x = legend_date + 0.35, xend = legend_date + 6.5,
      y = legend_row$ActualLow, yend = low_label_y, linewidth = 0.3, color = weather_palette$text) +
    ggplot2::annotate("text", x = legend_date + 7, y = high_label_y, label = "ACTUAL HIGH",
      hjust = 0, vjust = 0.5, family = "serif", fontface = "bold", size = 2.1, color = weather_palette$actual) +
    ggplot2::annotate("text", x = legend_date + 7, y = low_label_y, label = "ACTUAL LOW",
      hjust = 0, vjust = 0.5, family = "serif", fontface = "bold", size = 2.1, color = weather_palette$actual) +
    ggplot2::annotate("segment", x = legend_date - 3.5, xend = legend_date + 3.5,
      y = legend_row$NormalHigh, yend = legend_row$NormalHigh, linewidth = 0.3, color = weather_palette$text) +
    ggplot2::annotate("segment", x = legend_date - 3.5, xend = legend_date + 3.5,
      y = legend_row$NormalLow, yend = legend_row$NormalLow, linewidth = 0.3, color = weather_palette$text) +
    ggplot2::annotate("text", x = legend_date, y = mid_range_y,
      label = "NORMAL RANGE", hjust = 0.5, vjust = -0.2, family = "serif", fontface = "bold",
      size = 2.1, color = weather_palette$normal) +
    ggplot2::coord_cartesian(
      xlim = c(legend_date - 12, legend_date + 12),
      ylim = c(legend_row$RecordLow - legend_pad, legend_row$RecordHigh + legend_pad),
      expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = weather_palette$background, color = NA),
      plot.background = ggplot2::element_rect(fill = weather_palette$background, color = NA)
    )
}

render_weather_chart <- function(temp_data,
                                 rain_data,
                                 curr_year,
                                 title,
                                 subtitle = NULL,
                                 caption = "Data source: Oikolab") {
  if (!is.null(subtitle)) {
    subtitle <- stringr::str_replace_all(subtitle, "\u2022", "*")
  }

  month_midpoints <- weather_month_midpoints(curr_year)
  month_starts <- weather_month_starts(curr_year)
  temp_y_breaks <- seq(
    floor(min(temp_data$RecordLow, na.rm = TRUE) / 4) * 4,
    ceiling(max(temp_data$RecordHigh, na.rm = TRUE) / 4) * 4,
    by = 4
  )

  temp_main <- ggplot2::ggplot(temp_data) +
    ggplot2::geom_vline(xintercept = month_starts, color = weather_palette$normal, linewidth = 0.3) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = RecordLow, ymax = RecordHigh),
      fill = weather_palette$record,
      color = NA
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = NormalLow, ymax = NormalHigh),
      fill = weather_palette$normal,
      color = NA
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ActualLow, ymax = ActualHigh),
      fill = weather_palette$actual,
      color = NA
    ) +
    ggrepel::geom_text_repel(
      data = temp_data %>% filter(Special != "", RecordType == "high"),
      ggplot2::aes(x = Date, y = pmax(ActualHigh, RecordHigh) + 0.8, label = stringr::str_wrap(Special, 14)),
      size = 2.5,
      family = "serif",
      color = weather_palette$text,
      min.segment.length = 0,
      seed = 123,
      box.padding = 0.25
    ) +
    ggrepel::geom_text_repel(
      data = temp_data %>% filter(Special != "", RecordType == "low"),
      ggplot2::aes(x = Date, y = pmin(ActualLow, RecordLow) - 0.8, label = stringr::str_wrap(Special, 14)),
      size = 2.5,
      family = "serif",
      color = weather_palette$text,
      min.segment.length = 0,
      seed = 123,
      box.padding = 0.25,
      direction = "y",
      nudge_y = -0.4
    ) +
    ggplot2::scale_x_date(
      "",
      limits = c(as.Date(paste0(curr_year, "-01-01")), as.Date(paste0(curr_year, "-12-31"))),
      breaks = month_midpoints$x,
      labels = month_midpoints$label,
      position = "top",
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      "",
      breaks = temp_y_breaks,
      sec.axis = ggplot2::dup_axis()
    ) +
    weather_chart_theme(base_size = 10, x_position = "top") +
    ggplot2::annotate("text", x = as.Date(paste0(curr_year, "-01-05")),
      y = max(temp_y_breaks) - 0.1, label = "Temperature",
      hjust = 0, vjust = 0, family = "serif", fontface = "bold", size = 5, color = weather_palette$text) +
    ggplot2::annotate("text", x = as.Date(paste0(curr_year, "-01-05")),
      y = max(temp_y_breaks) - 1.0,
      label = "Bars represent range between the daily high and low",
      hjust = 0, vjust = 1, family = "serif", size = 3.2, color = weather_palette$text)

  temp_legend <- build_temperature_legend(temp_data, curr_year)
  if (!is.null(temp_legend)) {
    temp_main <- temp_main +
      patchwork::inset_element(temp_legend, left = 0.47, bottom = 0.12, right = 0.69, top = 0.30)
  }

  rain_limit <- max(
    c(
      rain_data$MonthlyAvg,
      rain_data$CumulRain,
      rain_data$CumulMedian,
      rain_data$CumulP90
    ),
    na.rm = TRUE
  )
  rain_break_top <- max(50, ceiling(rain_limit / 50) * 50)

  year_label_data <- rain_data %>%
    filter(CumulRain > 0) %>%
    slice_tail(n = 1) %>%
    transmute(
      x_year = DT + 3,
      y_year = CumulRain
    )

  month_end_labels <- rain_data %>%
    group_by(Month) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      min_sep = pmax(4, rain_break_top * 0.045),
      median_value = round(CumulMedian, 0),
      actual_value = round(CumulRain, 0),
      median_y_base = CumulMedian,
      actual_y_base = CumulRain,
      values_close = abs(actual_y_base - median_y_base) < min_sep,
      median_y = dplyr::if_else(values_close, median_y_base + min_sep / 2, median_y_base + rain_break_top * 0.012),
      actual_y = dplyr::if_else(values_close, pmax(actual_y_base - min_sep / 2, 0), pmax(actual_y_base - rain_break_top * 0.02, 0)),
      median_x = DT - 1.5,
      actual_x = DT + 1.5,
      left_edge = Month == min(Month, na.rm = TRUE),
      median_y = dplyr::if_else(left_edge, median_y + rain_break_top * 0.03, median_y),
      actual_y = dplyr::if_else(left_edge, actual_y + rain_break_top * 0.03, actual_y)
    ) %>%
    transmute(
      Month,
      median_x,
      actual_x,
      median_value,
      actual_value,
      median_y,
      actual_y
    )

  rain_plot <- ggplot2::ggplot(rain_data, ggplot2::aes(x = DT, group = Month)) +
    ggplot2::geom_vline(xintercept = month_starts, color = weather_palette$normal, linewidth = 0.3) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = CumulP10, ymax = CumulP90),
      fill = weather_palette$band_outer,
      alpha = 0.75,
      color = NA
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = CumulP25, ymax = CumulP75),
      fill = weather_palette$band_inner,
      alpha = 0.9,
      color = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = CumulMedian),
      color = weather_palette$text,
      linewidth = 0.55,
      alpha = 0.75,
      lineend = "butt"
    ) +
    ggplot2::geom_area(ggplot2::aes(y = CumulRain), fill = weather_palette$record, alpha = 0.28, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = CumulRain), color = weather_palette$actual, linewidth = 1.1, lineend = "butt") +
    ggplot2::geom_text(
      data = year_label_data,
      ggplot2::aes(x = x_year, y = y_year, label = "THIS YEAR"),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0.25,
      family = "serif",
      fontface = "bold",
      size = 2.7,
      color = weather_palette$actual
    ) +
    ggplot2::geom_text(
      data = month_end_labels %>% filter(median_value > 0),
      ggplot2::aes(x = median_x, y = median_y, label = median_value),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 0,
      family = "serif",
      fontface = "bold",
      size = 2.4,
      color = weather_palette$text,
      alpha = 0.85
    ) +
    ggplot2::geom_text(
      data = month_end_labels %>% filter(actual_value > 0),
      ggplot2::aes(x = actual_x, y = actual_y, label = actual_value),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      family = "serif",
      fontface = "bold",
      size = 2.4,
      color = weather_palette$actual
    ) +
    ggrepel::geom_text_repel(
      data = rain_data %>% filter(Label != ""),
      ggplot2::aes(y = CumulRain, label = stringr::str_wrap(Label, 16)),
      size = 2.3,
      family = "serif",
      fontface = "bold",
      color = weather_palette$text,
      seed = 123,
      min.segment.length = 0,
      box.padding = 0.25
    ) +
    ggplot2::scale_x_date(
      "",
      limits = c(as.Date(paste0(curr_year, "-01-01")), as.Date(paste0(curr_year, "-12-31"))),
      breaks = month_midpoints$x,
      labels = month_midpoints$label,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous("", breaks = seq(0, rain_break_top, 50), expand = c(0, 0.02)) +
    weather_chart_theme(base_size = 10) +
    ggplot2::annotate("text", x = as.Date(paste0(curr_year, "-01-05")),
      y = rain_break_top * 0.965, label = "Precipitation",
      hjust = 0, vjust = 1, family = "serif", fontface = "bold", size = 5, color = weather_palette$text) +
    ggplot2::annotate("text", x = as.Date(paste0(curr_year, "-02-05")),
      y = rain_break_top * 0.965,
      label = "Cumulative monthly precipitation against historical percentile bands",
      hjust = 0, vjust = 1, family = "serif", size = 3.2, color = weather_palette$text)

  combined <- temp_main / rain_plot +
    patchwork::plot_layout(heights = c(7, 3)) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) &
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = "serif", face = "bold", size = 20, color = weather_palette$text, hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        family = "serif", size = 10.5, color = weather_palette$actual, hjust = 0,
        margin = ggplot2::margin(t = 4, b = 10)
      ),
      plot.caption = ggplot2::element_text(
        family = "serif", size = 9, color = weather_palette$text, hjust = 1
      ),
      plot.background = ggplot2::element_rect(fill = weather_palette$background, color = NA),
      panel.background = ggplot2::element_rect(fill = weather_palette$background, color = NA)
    )

  combined
}
