library(tidytable)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(patchwork)

project_dir <- '/Users/Karthik/Documents/work/data work/bangalore/weather'

load(file.path(project_dir, 'data', 'bangaloreWind.RData'))
load(file.path(project_dir, 'data', 'bangaloreRainfall.RData'))

message("Preparing Data...")
wind <- blrWind %>%
  mutate(DT = as.POSIXct(DT), WindDir = as.numeric(WindDir), Wind = as.numeric(Wind)) %>%
  select(DT, WindDir, Wind)

rain <- blrRain %>%
  mutate(DT = as.POSIXct(DT), Rain = as.numeric(Rain)) %>%
  select(DT, Rain)

df <- rain %>%
  left_join(wind, by = "DT") %>%
  arrange(DT)

df <- df %>%
  mutate(IsRainy = Rain > 1) %>%
  mutate(RainEventID = cumsum(IsRainy & !lag(IsRainy, default = FALSE)))

events <- df %>%
  filter(IsRainy) %>%
  summarise(
    StartTime = min(DT),
    TotalRain = sum(Rain),
    .by = RainEventID
  ) %>%
  mutate(
    Year = year(StartTime),
    Month = month(StartTime),
    Day = day(StartTime),
    HalfMonth = ifelse(Day <= 15, "H1", "H2")
  )

target_hours <- bind_rows(
  events %>% mutate(TargetDT = StartTime - hours(1)),
  events %>% mutate(TargetDT = StartTime - hours(2))
) %>%
  select(RainEventID, Year, Month, HalfMonth, TargetDT)

pre_rain_wind <- target_hours %>%
  left_join(df %>% select(DT, WindDir, Wind), by = c("TargetDT" = "DT")) %>%
  filter(!is.na(WindDir) & !is.na(Wind)) %>%
  mutate(
    U = -1 * Wind * sin(WindDir * pi / 180),
    V = -1 * Wind * cos(WindDir * pi / 180),
    DirBin = round(WindDir / 15) * 15,
    DirBin = ifelse(DirBin == 360, 0, DirBin),
    Period = factor(paste0(sprintf("%02d", Month), "_", month.abb[Month], " ", HalfMonth))
  )

# 1. Wind Roses by Half-Month
message("Generating Wind Roses...")

dist_data <- pre_rain_wind %>%
  summarise(
    MeanSpeed = mean(Wind),
    Count = n(),
    .by = c(Period, DirBin)
  ) %>%
  mutate(
    Freq = Count / sum(Count),
    .by = Period
  )

vec_data <- pre_rain_wind %>%
  summarise(
    MeanU = mean(U),
    MeanV = mean(V),
    .by = Period
  ) %>%
  mutate(
    MeanSpeed = sqrt(MeanU^2 + MeanV^2),
    MeanWindDir = atan2(-MeanU, -MeanV) * 180 / pi,
    MeanWindDir = ifelse(MeanWindDir < 0, MeanWindDir + 360, MeanWindDir)
  )

p_rose <- ggplot() +
  geom_segment(
    data = dist_data,
    aes(x = DirBin, xend = DirBin, y = 0, yend = MeanSpeed, linewidth = Freq),
    color = "#9c9280", lineend = "round"
  ) +
  geom_segment(
    data = vec_data,
    aes(x = MeanWindDir, xend = MeanWindDir, y = MeanSpeed, yend = 0),
    color = "#5f3946", linewidth = 1.2,
    arrow = arrow(length = unit(0.04, "inches"), type = "closed")
  ) +
  facet_wrap(~Period, ncol = 6) +
  coord_polar(theta = "x", start = 0, direction = 1) +
  scale_linewidth_continuous(range = c(0.5, 3), labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  scale_y_continuous(limits = c(0, max(dist_data$MeanSpeed) * 1.1)) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 6),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = 'bold', hjust = 0, color = '#3C3C3C', size = 8),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10, margin = margin(b = 15))
  ) +
  labs(
    title = "Pre-Rain Wind Distributions by Half-Month",
    subtitle = "Spoke thickness = Frequency. Dark arrows show mean vector pointing inwards.",
    x = NULL, y = NULL
  )

outfile_rose <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_pre_rain_wind_roses.png')
ggsave(outfile_rose, p_rose, width = 14, height = 10, dpi = 300)
message("Saved: ", outfile_rose)

# 2. Time Series
message("Generating Time Series...")

# Let's categorize wind direction into 4 quadrants
pre_rain_wind <- pre_rain_wind %>%
  mutate(
    Quadrant = case_when(
      WindDir >= 315 | WindDir < 45 ~ "North",
      WindDir >= 45 & WindDir < 135 ~ "East",
      WindDir >= 135 & WindDir < 225 ~ "South",
      WindDir >= 225 & WindDir < 315 ~ "West"
    )
  )

ts_data <- pre_rain_wind %>%
  count(Year, Quadrant) %>%
  mutate(Pct = n / sum(n), .by = Year)

p_ts <- ggplot(ts_data, aes(x = Year, y = Pct, fill = Quadrant)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c("North" = "#5b7c99", "East" = "#9c9280", "South" = "#CD3333", "West" = "#5f3946")) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10, margin = margin(b = 15))
  ) +
  labs(
    title = "Historical Shift in Pre-Rain Wind Direction (1981-Present)",
    subtitle = "Percentage of rainfall events preceded by winds from each quadrant",
    x = "Year", y = "Proportion of Pre-Rain Winds"
  )

outfile_ts <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_pre_rain_wind_timeseries.png')
ggsave(outfile_ts, p_ts, width = 10, height = 6, dpi = 300)
message("Saved: ", outfile_ts)
