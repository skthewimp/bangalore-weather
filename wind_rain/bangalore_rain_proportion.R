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
    WindDir = as.numeric(WindDir),
    Year = year(DT),
    DOY = yday(DT)
  ) %>%
  filter(!is.na(WindDir), Year < curr_year, DOY <= 365) %>%
  select(DT, Year, DOY, WindDir)

rain <- blrRain %>%
  mutate(Rain = as.numeric(Rain), Year = year(DT), DOY = yday(DT)) %>%
  filter(!is.na(Rain), Year < curr_year, DOY <= 365) %>%
  select(DT, Rain)

sector_levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
df <- wind %>%
  inner_join(rain, by = "DT") %>%
  mutate(
    SectorIdx = floor(((WindDir + 22.5) %% 360) / 45) + 1,
    Sector = factor(sector_levels[SectorIdx], levels = sector_levels)
  )

n_years <- df %>% summarise(n = n_distinct(Year)) %>% pull(n)

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

# Normalise to share-of-rain per DOY, then mask DOYs with < 1mm/day total
rain_threshold <- 1.0
doy_prop <- doy_rain %>%
  mutate(DOYTotal = sum(RainSmooth), .by = DOY) %>%
  mutate(
    Share = ifelse(DOYTotal >= rain_threshold, RainSmooth / DOYTotal * 100, NA_real_)
  )

stack_order <- c("E", "SE", "S", "SW", "W", "NW", "N", "NE")
doy_prop <- doy_prop %>% mutate(Sector = factor(Sector, levels = stack_order))

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

# Drop NA DOYs so geom_area renders only the rainy season
plot_data <- doy_prop %>% filter(!is.na(Share))

stack_df <- plot_data %>%
  arrange(DOY, Sector) %>%
  mutate(CumTop = cumsum(Share), .by = DOY) %>%
  mutate(MidY = CumTop - Share / 2)

labels_df <- stack_df %>%
  group_by(Sector) %>%
  mutate(PeakMax = max(Share, na.rm = TRUE)) %>%
  filter(Share >= 0.8 * PeakMax) %>%
  summarise(DOY = round(median(DOY)), PeakMax = first(PeakMax)) %>%
  ungroup() %>%
  inner_join(stack_df %>% select(Sector, DOY, Share, MidY),
             by = c("Sector", "DOY")) %>%
  filter(PeakMax >= 5)

p <- ggplot(plot_data, aes(x = DOY, y = Share, fill = Sector)) +
  geom_area(position = "stack", color = NA) +
  geom_text(
    data = labels_df, aes(x = DOY, y = MidY, label = Sector),
    color = "white", fontface = "bold", size = 3.8, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = month_starts, labels = month_labs,
    limits = c(1, 365), expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0), breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
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
    title = "When It Rains In Bangalore, Where Is The Wind Coming From?",
    subtitle = paste0("Share of each day's rainfall attributable to the concurrent wind sector. Days with under ",
                      rain_threshold, " mm/day average rainfall are omitted."),
    caption = paste0("Data source: Oikolab (ERA5). 1981-", curr_year - 1,
                     ". 15-day smoothing. Hourly rainfall attributed to the wind sector active that hour."),
    x = NULL, y = NULL
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_rain_proportion.png')
ggsave(outfile, p, width = 12, height = 7, dpi = 300)
message("Saved: ", outfile)
