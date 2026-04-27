library(tidytable)
library(tidyverse)
library(lubridate)
library(ggthemes)

project_dir <- '/Users/Karthik/Documents/work/data work/bangalore/weather'
load(file.path(project_dir, 'data', 'bangaloreWind.RData'))
load(file.path(project_dir, 'data', 'bangaloreRainfall.RData'))

curr_year <- 2026

wind <- blrWind %>%
  mutate(
    Wind = as.numeric(Wind),
    WindDir = as.numeric(WindDir),
    Year = year(DT),
    DOY = yday(DT)
  ) %>%
  filter(!is.na(Wind) & !is.na(WindDir), Year < curr_year, DOY <= 365) %>%
  select(DT, Year, DOY, WindDir)

rain <- blrRain %>%
  mutate(Rain = as.numeric(Rain), Year = year(DT), DOY = yday(DT)) %>%
  filter(!is.na(Rain), Year < curr_year, DOY <= 365) %>%
  select(DT, Year, DOY, Rain)

# Join on DT, classify each hour into a sector
sector_levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
df <- wind %>%
  inner_join(rain %>% select(DT, Rain), by = "DT") %>%
  mutate(
    SectorIdx = floor(((WindDir + 22.5) %% 360) / 45) + 1,
    Sector = factor(sector_levels[SectorIdx], levels = sector_levels)
  )

n_years <- df %>% summarise(n = n_distinct(Year)) %>% pull(n)

# mm of rain per DOY per sector, averaged across years
doy_rain <- df %>%
  summarise(RainSum = sum(Rain), .by = c(DOY, Sector)) %>%
  complete(DOY = 1:365, Sector = sector_levels, fill = list(RainSum = 0)) %>%
  as_tidytable() %>%
  mutate(
    Sector = factor(Sector, levels = sector_levels),
    RainPerDay = RainSum / n_years
  )

smooth_circ <- function(x, k = 15) {
  n <- length(x)
  pad <- c(tail(x, k), x, head(x, k))
  zoo::rollmean(pad, k = k, fill = NA, align = 'center')[(k + 1):(n + k)]
}

doy_rain <- doy_rain %>%
  arrange(Sector, DOY) %>%
  mutate(RainSmooth = smooth_circ(RainPerDay, k = 15), .by = Sector)

stack_order <- c("E", "SE", "S", "SW", "W", "NW", "N", "NE")
doy_rain <- doy_rain %>% mutate(Sector = factor(Sector, levels = stack_order))

sector_colors <- c(
  "N"  = "#7a8c99",
  "NE" = "#b08968",
  "E"  = "#c97b3a",
  "SE" = "#a8543a",
  "S"  = "#5f3946",
  "SW" = "#4a6b7c",
  "W"  = "#2e7a8a",
  "NW" = "#5a8a9a"
)

month_starts <- yday(ymd(paste0("2023-", sprintf("%02d", 1:12), "-01")))
month_labs <- month.abb

stack_df <- doy_rain %>%
  filter(!is.na(RainSmooth)) %>%
  arrange(DOY, Sector) %>%
  mutate(CumTop = cumsum(RainSmooth), .by = DOY) %>%
  mutate(MidY = CumTop - RainSmooth / 2)

labels_df <- stack_df %>%
  group_by(Sector) %>%
  mutate(PeakMax = max(RainSmooth, na.rm = TRUE)) %>%
  filter(RainSmooth >= 0.8 * PeakMax) %>%
  summarise(DOY = round(median(DOY)), PeakMax = first(PeakMax)) %>%
  ungroup() %>%
  inner_join(stack_df %>% select(Sector, DOY, RainSmooth, MidY),
             by = c("Sector", "DOY")) %>%
  filter(PeakMax >= 0.25)

p <- ggplot(doy_rain, aes(x = DOY, y = RainSmooth, fill = Sector)) +
  geom_area(position = "stack", color = NA) +
  geom_text(
    data = labels_df, aes(x = DOY, y = MidY, label = Sector),
    color = "white", fontface = "bold", size = 3.8, inherit.aes = FALSE
  ) +
  scale_x_continuous(breaks = month_starts, labels = month_labs, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, " mm")) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 10),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", color = '#3C3C3C', size = 10),
    legend.text = element_text(color = '#3C3C3C', size = 9),
    legend.background = element_rect(fill = '#e5e1d8', color = NA),
    legend.key = element_rect(fill = '#e5e1d8', color = NA),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10, margin = margin(b = 15)),
    plot.caption = element_text(color = '#5f3946', size = 8)
  ) +
  labs(
    title = "Which Winds Bring Bangalore's Rain?",
    subtitle = "Average daily rainfall (mm) by day of year, decomposed by the wind direction during each rainy hour.\nTotal height = how much it rains; colour mix = which direction the wind was blowing from while it rained.",
    caption = paste0("Data source: Oikolab (ERA5). 1981-", curr_year - 1,
                     ". Hourly rainfall attributed to the concurrent wind sector. 15-day smoothing."),
    x = NULL, y = NULL
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_rain_river.png')
ggsave(outfile, p, width = 12, height = 7, dpi = 300)
message("Saved: ", outfile)
