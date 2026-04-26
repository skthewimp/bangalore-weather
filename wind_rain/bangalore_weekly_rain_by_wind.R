library(tidytable)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

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
  left_join(wind, by = "DT", relationship = "many-to-many") %>%
  arrange(DT)

# Identify Rainfall Events
df <- df %>%
  mutate(IsRainy = Rain > 1) %>%
  mutate(RainEventID = cumsum(IsRainy & !lag(IsRainy, default = FALSE)))

# Summarize Events
events <- df %>%
  filter(IsRainy) %>%
  summarise(
    StartTime = min(DT),
    TotalRain = sum(Rain),
    .by = RainEventID
  ) %>%
  mutate(
    Year = year(StartTime),
    Week = isoweek(StartTime)
  ) %>%
  mutate(
    WeeklyTotalRain = sum(TotalRain),
    .by = c(Year, Week)
  ) %>%
  # FILTER: Only weeks with >= 10mm total rain
  filter(WeeklyTotalRain >= 10)

# Extract pre-rain wind (2 hours prior)
target_hours <- bind_rows(
  events %>% mutate(TargetDT = StartTime - hours(1)),
  events %>% mutate(TargetDT = StartTime - hours(2))
) %>%
  select(RainEventID, Year, Week, TotalRain, TargetDT)

pre_rain_wind <- target_hours %>%
  left_join(df %>% select(DT, WindDir, Wind), by = c("TargetDT" = "DT"), relationship = "many-to-many") %>%
  filter(!is.na(WindDir) & !is.na(Wind)) %>%
  mutate(
    U = -1 * Wind * sin(WindDir * pi / 180),
    V = -1 * Wind * cos(WindDir * pi / 180)
  )

# Calculate predominant direction for the event
event_wind <- pre_rain_wind %>%
  summarise(
    MeanU = mean(U),
    MeanV = mean(V),
    .by = c(RainEventID, Year, Week, TotalRain)
  ) %>%
  mutate(
    MeanWindDir = atan2(-MeanU, -MeanV) * 180 / pi,
    MeanWindDir = ifelse(MeanWindDir < 0, MeanWindDir + 360, MeanWindDir),
    Quadrant = case_when(
      MeanWindDir >= 315 | MeanWindDir < 45 ~ "North",
      MeanWindDir >= 45 & MeanWindDir < 135 ~ "East",
      MeanWindDir >= 135 & MeanWindDir < 225 ~ "South",
      MeanWindDir >= 225 & MeanWindDir < 315 ~ "West"
    ),
    Quadrant = factor(Quadrant, levels = c("North", "East", "South", "West"))
  )

# Aggregate rainfall volume by Week and Quadrant
num_years <- n_distinct(event_wind$Year)

weekly_vol <- event_wind %>%
  summarise(
    SumRain = sum(TotalRain),
    .by = c(Week, Quadrant)
  ) %>%
  mutate(
    AvgRainPerYear = SumRain / num_years
  )

message("Generating Weekly Rainfall Visualization...")

p <- ggplot(weekly_vol, aes(x = Week, y = AvgRainPerYear, fill = Quadrant)) +
  geom_col(width = 0.8) +
  facet_wrap(~ Quadrant, ncol = 1) +
  scale_fill_manual(values = c("North" = "#5b7c99", "East" = "#9c9280", "South" = "#CD3333", "West" = "#5f3946")) +
  scale_x_continuous(breaks = seq(1, 52, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 9),
    axis.title = element_text(face = 'bold', color = '#3C3C3C', size = 10, margin = margin(t = 10)),
    legend.position = "none",
    strip.text = element_text(face = 'bold', color = '#3C3C3C', size = 11, hjust = 0),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10, margin = margin(b = 15))
  ) +
  labs(
    title = "Bangalore's Meaningful Rainfall by Week and Wind Source",
    subtitle = "Average weekly rainfall volume from weeks with >= 10mm rain, colored by pre-rain wind direction.",
    caption = paste("Data source: Oikolab. Filtered for weeks >= 10mm. Average taken over", num_years, "years."),
    x = "Week of the Year",
    y = "Average Rainfall Volume (mm)"
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_weekly_rain_by_wind.png')
ggsave(outfile, p, width = 12, height = 10, dpi = 300)
message("Saved: ", outfile)

# Let's also print some numerical insights
cat("\n=== TOP WEEKS FOR RAIN AND PREDOMINANT WIND ===\n")
top_weeks <- weekly_vol %>%
  summarise(TotalWeeklyRain = sum(AvgRainPerYear), .by = Week) %>%
  arrange(desc(TotalWeeklyRain)) %>%
  slice_head(n = 5)

for (w in top_weeks$Week) {
  cat(sprintf("\nWeek %d (%.1f mm avg):\n", w, top_weeks$TotalWeeklyRain[top_weeks$Week == w]))
  week_data <- weekly_vol %>% filter(Week == w) %>% arrange(desc(AvgRainPerYear))
  for (i in 1:nrow(week_data)) {
    cat(sprintf("  - %s: %.1f mm\n", week_data$Quadrant[i], week_data$AvgRainPerYear[i]))
  }
}
