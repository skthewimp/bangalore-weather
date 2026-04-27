library(tidytable)
library(tidyverse)
library(lubridate)
library(ggthemes)

project_dir <- '/Users/Karthik/Documents/work/data work/bangalore/weather'
load(file.path(project_dir, 'data', 'bangaloreWind.RData'))

curr_year <- 2026

blrWind <- blrWind %>%
  mutate(
    Wind = as.numeric(Wind),
    WindDir = as.numeric(WindDir),
    Year = year(DT),
    DOY = yday(DT)
  ) %>%
  filter(!is.na(Wind) & !is.na(WindDir), Year < curr_year, DOY <= 365)

# 8 cardinal sectors. Each sector centred on its cardinal angle, width 45 deg.
# Sector edges: N covers 337.5-22.5, NE covers 22.5-67.5, etc.
sector_levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
blrWind <- blrWind %>%
  mutate(
    SectorIdx = floor(((WindDir + 22.5) %% 360) / 45) + 1,
    Sector = factor(sector_levels[SectorIdx], levels = sector_levels)
  )

# Speed-weighted share per day-of-year per sector. Each hour contributes its
# wind speed rather than 1 - so a 6 m/s westerly counts twice as much as a
# 3 m/s easterly when both blow for an hour.
doy_dist <- blrWind %>%
  summarise(WindSum = sum(Wind), .by = c(DOY, Sector)) %>%
  complete(DOY = 1:365, Sector = sector_levels, fill = list(WindSum = 0)) %>%
  as_tidytable() %>%
  mutate(Freq = WindSum / sum(WindSum) * 100, .by = DOY) %>%
  mutate(Sector = factor(Sector, levels = sector_levels))

# 15-day rolling mean to smooth, circular (wrap year boundary)
smooth_circ <- function(x, k = 15) {
  n <- length(x)
  pad <- c(tail(x, k), x, head(x, k))
  zoo::rollmean(pad, k = k, fill = NA, align = 'center')[(k + 1):(n + k)]
}

doy_dist <- doy_dist %>%
  arrange(Sector, DOY) %>%
  mutate(FreqSmooth = smooth_circ(Freq, k = 15), .by = Sector)

# Stack walks compass clockwise from E (bottom) to NE (top), placing W at
# the antipode of E. Reading up the stack = walking around the compass.
stack_order <- c("E", "SE", "S", "SW", "W", "NW", "N", "NE")
doy_dist <- doy_dist %>% mutate(Sector = factor(Sector, levels = stack_order))

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

# Month tick positions (1st of each month) for x-axis
month_starts <- yday(ymd(paste0("2023-", sprintf("%02d", 1:12), "-01")))
month_labs <- month.abb

# Inline labels: place each sector at the *centre of its peak window* - the
# midpoint of all DOYs where its share is within 80% of that sector's max.
# This pulls labels off chart edges and separates same-time peaks.
stack_df <- doy_dist %>%
  filter(!is.na(FreqSmooth)) %>%
  arrange(DOY, Sector) %>%
  mutate(CumTop = cumsum(FreqSmooth), .by = DOY) %>%
  mutate(MidY = CumTop - FreqSmooth / 2)

labels_df <- stack_df %>%
  group_by(Sector) %>%
  mutate(PeakMax = max(FreqSmooth, na.rm = TRUE)) %>%
  filter(FreqSmooth >= 0.8 * PeakMax) %>%
  summarise(
    DOY = round(median(DOY)),
    PeakMax = first(PeakMax)
  ) %>%
  ungroup()

# Get the actual MidY at the chosen DOY for each sector
labels_df <- labels_df %>%
  inner_join(stack_df %>% select(Sector, DOY, FreqSmooth, MidY),
             by = c("Sector", "DOY")) %>%
  filter(PeakMax >= 5)  # drop sectors whose peak is too thin to label cleanly

p <- ggplot(doy_dist, aes(x = DOY, y = FreqSmooth, fill = Sector)) +
  geom_area(position = "stack", color = NA) +
  geom_text(
    data = labels_df, aes(x = DOY, y = MidY, label = Sector),
    color = "white", fontface = "bold", size = 4, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = month_starts,
    labels = month_labs,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(0, 25, 50, 75, 100),
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
    title = "Bangalore's Wind Through The Year",
    subtitle = "Share of total wind 'volume' from each compass sector, by day of year. Each hour weighted by its wind speed,\nso strong winds count more than weak ones. 15-day smoothing.",
    caption = "Data source: Oikolab (ERA5). 1981-2025. Sectors are 45 degrees wide, centred on each cardinal/intercardinal direction.",
    x = NULL, y = NULL
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_wind_river.png')
ggsave(outfile, p, width = 12, height = 7, dpi = 300)
message("Saved: ", outfile)
