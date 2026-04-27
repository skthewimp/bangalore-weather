library(tidytable)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(patchwork)

project_dir <- '/Users/Karthik/Documents/work/data work/bangalore/weather'
load(file.path(project_dir, 'data', 'bangaloreRainfall.RData'))
load(file.path(project_dir, 'data', 'bangaloreWind.RData'))

curr_year <- 2026
rain_thresh <- 0.1  # mm/h - what counts as a "rainy hour"

rain <- blrRain %>%
  mutate(
    Rain = as.numeric(Rain),
    Year = year(DT),
    Month = month(DT),
    DOY = yday(DT),
    HourOfDay = hour(DT)
  ) %>%
  filter(!is.na(Rain), Year < curr_year, DOY <= 365)

# Season labels - boundaries based on the rain river chart
rain <- rain %>%
  mutate(Season = case_when(
    Month %in% c(4, 5)        ~ "Pre-monsoon (Apr-May)",
    Month %in% c(6, 7, 8, 9)  ~ "SW monsoon (Jun-Sep)",
    Month %in% c(10, 11)      ~ "NE monsoon (Oct-Nov)",
    TRUE                      ~ "Dry (Dec-Mar)"
  ),
  Season = factor(Season, levels = c(
    "Dry (Dec-Mar)", "Pre-monsoon (Apr-May)",
    "SW monsoon (Jun-Sep)", "NE monsoon (Oct-Nov)"
  )))

n_years <- n_distinct(rain$Year)

# ---- Summary table by season ---------------------------------------------
season_summary <- rain %>%
  summarise(
    TotalMM        = sum(Rain) / n_years,
    RainyHours     = sum(Rain >= rain_thresh) / n_years,
    HoursInSeason  = n() / n_years,
    MeanIntensity  = mean(Rain[Rain >= rain_thresh]),
    MedianIntensity = median(Rain[Rain >= rain_thresh]),
    P95Intensity   = quantile(Rain[Rain >= rain_thresh], 0.95),
    MaxIntensity   = max(Rain),
    HeavyHours_5mm = sum(Rain >= 5) / n_years,
    .by = Season
  ) %>%
  mutate(
    PctTimeRaining = RainyHours / HoursInSeason * 100,
    DaysInSeason   = HoursInSeason / 24
  ) %>%
  arrange(Season)

cat("\n==================== SEASON SUMMARY ====================\n")
print(as.data.frame(season_summary), digits = 3)
cat("\n")

# ---- Per-DOY metrics, smoothed ------------------------------------------
smooth_circ <- function(x, k = 15) {
  n <- length(x); pad <- c(tail(x, k), x, head(x, k))
  zoo::rollmean(pad, k = k, fill = NA, align = 'center')[(k + 1):(n + k)]
}

doy_metrics <- rain %>%
  summarise(
    TotalRain      = sum(Rain) / n_years,
    RainyHours     = sum(Rain >= rain_thresh) / n_years,
    MeanIntensity  = ifelse(any(Rain >= rain_thresh),
                            mean(Rain[Rain >= rain_thresh]), NA_real_),
    MaxIntensity   = max(Rain),
    HeavyHours_5mm = sum(Rain >= 5) / n_years,
    .by = DOY
  ) %>%
  arrange(DOY) %>%
  mutate(across(c(TotalRain, RainyHours, MeanIntensity, MaxIntensity, HeavyHours_5mm),
                ~ smooth_circ(replace_na(.x, 0), k = 15),
                .names = "{.col}_s"))

month_starts <- yday(ymd(paste0("2023-", sprintf("%02d", 1:12), "-01")))
month_labs <- month.abb

base_theme <- list(
  ggthemes::theme_tufte(),
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background  = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 9),
    axis.ticks = element_blank(),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 12),
    plot.subtitle = element_text(color = '#5f3946', size = 9),
    strip.text = element_text(face = 'bold', color = '#3C3C3C', size = 10)
  )
)

# Season shading
season_bands <- tibble(
  xmin = c(yday(ymd("2023-04-01")), yday(ymd("2023-06-01")), yday(ymd("2023-10-01"))),
  xmax = c(yday(ymd("2023-05-31")), yday(ymd("2023-09-30")), yday(ymd("2023-11-30"))),
  Season = c("Pre-monsoon", "SW monsoon", "NE monsoon"),
  fill = c("#c97b3a", "#2e7a8a", "#b08968")
)
band_layer <- geom_rect(
  data = season_bands,
  aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
  alpha = 0.12, inherit.aes = FALSE, show.legend = FALSE
)

x_scale <- scale_x_continuous(breaks = month_starts, labels = month_labs,
                              limits = c(1, 365), expand = c(0, 0))

p_total <- ggplot(doy_metrics, aes(x = DOY, y = TotalRain_s)) +
  band_layer +
  scale_fill_identity() +
  geom_line(color = '#3C3C3C', linewidth = 0.7) +
  x_scale +
  scale_y_continuous(labels = function(x) paste0(x, " mm"), expand = c(0, 0)) +
  labs(title = "Total rainfall", subtitle = "mm per day, averaged across years",
       x = NULL, y = NULL) +
  base_theme

p_hours <- ggplot(doy_metrics, aes(x = DOY, y = RainyHours_s)) +
  band_layer + scale_fill_identity() +
  geom_line(color = '#5f3946', linewidth = 0.7) +
  x_scale +
  scale_y_continuous(labels = function(x) paste0(x, " h"), expand = c(0, 0)) +
  labs(title = "Frequency: rainy hours per day",
       subtitle = paste0("hours with at least ", rain_thresh, " mm of rain"),
       x = NULL, y = NULL) +
  base_theme

p_intensity <- ggplot(doy_metrics, aes(x = DOY, y = MeanIntensity_s)) +
  band_layer + scale_fill_identity() +
  geom_line(color = '#c97b3a', linewidth = 0.7) +
  x_scale +
  scale_y_continuous(labels = function(x) paste0(x, " mm/h"), expand = c(0, 0)) +
  labs(title = "Intensity: mean mm per rainy hour",
       subtitle = "conditional on it raining",
       x = NULL, y = NULL) +
  base_theme

p_heavy <- ggplot(doy_metrics, aes(x = DOY, y = HeavyHours_5mm_s)) +
  band_layer + scale_fill_identity() +
  geom_area(fill = '#a8543a', alpha = 0.7) +
  geom_line(color = '#a8543a', linewidth = 0.5) +
  x_scale +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Heavy-rain hours per day (>= 5 mm/h)",
       subtitle = "downpour count",
       x = NULL, y = NULL) +
  base_theme

through_year <- (p_total / p_hours / p_intensity / p_heavy) +
  plot_annotation(
    title = "Bangalore rain through the year: amount, frequency, intensity",
    subtitle = "Shaded bands: Pre-monsoon (orange) | SW monsoon (teal) | NE monsoon (tan). 15-day smoothing.",
    caption = paste0("Data: Oikolab (ERA5) hourly, 1981-", curr_year - 1),
    theme = theme(
      plot.background = element_rect(fill = '#e5e1d8', color = NA),
      plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
      plot.subtitle = element_text(color = '#5f3946', size = 10),
      plot.caption = element_text(color = '#5f3946', size = 8)
    )
  )

ggsave(file.path(project_dir, 'wind_rain', 'charts', 'bangalore_rain_through_year_metrics.png'),
       through_year, width = 12, height = 12, dpi = 300, bg = '#e5e1d8')
message("Saved: bangalore_rain_through_year_metrics.png")

# ---- Distribution of hourly intensities by season ------------------------
season_colors <- c(
  "Dry (Dec-Mar)"          = "#9c9280",
  "Pre-monsoon (Apr-May)"  = "#c97b3a",
  "SW monsoon (Jun-Sep)"   = "#2e7a8a",
  "NE monsoon (Oct-Nov)"   = "#b08968"
)

rainy_hrs <- rain %>% filter(Rain >= rain_thresh)

p_dist <- ggplot(rainy_hrs, aes(x = Season, y = Rain, fill = Season)) +
  geom_violin(scale = "width", color = NA, alpha = 0.5, width = 0.85) +
  geom_boxplot(width = 0.18, outlier.shape = NA, color = '#3C3C3C',
               linewidth = 0.4, fill = 'white', alpha = 0.85) +
  scale_y_log10(labels = function(x) paste0(x, " mm/h"),
                breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50)) +
  scale_fill_manual(values = season_colors) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 10),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10),
    plot.caption = element_text(color = '#5f3946', size = 8)
  ) +
  labs(
    title = "Hourly rain intensity by season (rainy hours only)",
    subtitle = "Log scale. Box = IQR. Pre-monsoon hours skew heavier than SW monsoon hours - thunderstorms vs steady rain.",
    caption = "Data: Oikolab (ERA5) hourly, 1981-2025.",
    x = NULL, y = NULL
  )

ggsave(file.path(project_dir, 'wind_rain', 'charts', 'bangalore_rain_intensity_by_season.png'),
       p_dist, width = 10, height = 7, dpi = 300, bg = '#e5e1d8')
message("Saved: bangalore_rain_intensity_by_season.png")

# ---- Diurnal pattern by season -------------------------------------------
diurnal <- rain %>%
  summarise(MeanRain = mean(Rain), .by = c(Season, HourOfDay)) %>%
  mutate(Season = factor(Season, levels = levels(rain$Season)))

diurnal_labels <- diurnal %>%
  group_by(Season) %>%
  slice_max(MeanRain, n = 1, with_ties = FALSE) %>%
  ungroup()

p_diurnal <- ggplot(diurnal, aes(x = HourOfDay, y = MeanRain, color = Season)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  geom_text(
    data = diurnal_labels,
    aes(x = HourOfDay, y = MeanRain, label = Season),
    hjust = -0.08, vjust = -0.6, fontface = "bold", size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(0, 23, 3),
                     labels = function(x) sprintf("%02d:00", x),
                     expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(labels = function(x) paste0(x, " mm/h"),
                     expand = expansion(mult = c(0.02, 0.12))) +
  scale_color_manual(values = season_colors, guide = "none") +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 10),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10),
    plot.caption = element_text(color = '#5f3946', size = 8)
  ) +
  labs(
    title = "When during the day does it rain?",
    subtitle = "Mean rainfall (mm/h) by hour of day, by season. All rainy seasons peak in the afternoon, but pre-monsoon is the sharpest:\nnear-zero through morning, then a tight 17-18:00 spike. SW monsoon has the highest peak; NE monsoon is broadest.",
    caption = "Data: Oikolab (ERA5) hourly, 1981-2025. Times are IST.",
    x = NULL, y = NULL
  )

ggsave(file.path(project_dir, 'wind_rain', 'charts', 'bangalore_rain_diurnal_by_season.png'),
       p_diurnal, width = 11, height = 6, dpi = 300, bg = '#e5e1d8')
message("Saved: bangalore_rain_diurnal_by_season.png")

cat("\nDone. Three charts saved to wind_rain/charts/.\n")
