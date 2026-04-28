library(tidytable)
library(tidyverse)
library(lubridate)
library(ggthemes)

project_dir <- '/Users/Karthik/Documents/work/data work/bangalore/weather'

load(file.path(project_dir, 'data', 'bangaloreWind.RData'))

curr_year <- 2026

wind_doy <- blrWind %>%
  mutate(
    Wind = as.numeric(Wind),
    Year = year(DT),
    DOY = yday(DT)
  ) %>%
  filter(!is.na(Wind), Year < curr_year, DOY <= 365) %>%
  summarise(MeanWind = mean(Wind), .by = DOY) %>%
  arrange(DOY)

month_starts <- yday(as.Date(paste0("2023-", 1:12, "-01")))
month_labels <- month.abb

p <- ggplot(wind_doy, aes(x = DOY, y = MeanWind)) +
  geom_line(color = "#9c9280", linewidth = 0.4) +
  geom_smooth(se = FALSE, color = "#5f3946", linewidth = 1.1, span = 0.15) +
  scale_x_continuous(breaks = month_starts, labels = month_labels, expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, " m/s")) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 9),
    axis.line.x = element_line(color = '#3C3C3C', linewidth = 0.2),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10, margin = margin(b = 15)),
    plot.caption = element_text(color = '#3C3C3C', size = 8)
  ) +
  labs(
    title = "Bangalore: Average Wind Speed by Day of Year",
    subtitle = "Daily mean across 1981-2025. Smoothed line in maroon, raw daily mean in tan.",
    caption = "Data source: Oikolab (ERA5).",
    x = NULL,
    y = NULL
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_wind_doy.png')
ggsave(outfile, p, width = 12, height = 6, dpi = 300)
message("Saved: ", outfile)
