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

# Inline labels: place each sector's label at the DOY where its band is
# fattest (argmax of smoothed share). Guarantees the label sits inside its
# region rather than in a valley between bimodal peaks.
stack_df <- doy_dist %>%
  filter(!is.na(FreqSmooth)) %>%
  arrange(DOY, Sector) %>%
  mutate(CumTop = cumsum(FreqSmooth), .by = DOY) %>%
  mutate(MidY = CumTop - FreqSmooth / 2)

# For each sector: find longest contiguous run of "fat" days (>=70% of peak),
# pick the centre of that run, then take the *median* MidY across the run so
# the label isn't sensitive to single-day stack jitter at the chosen DOY.
fat_runs <- stack_df %>%
  group_by(Sector) %>%
  arrange(DOY, .by_group = TRUE) %>%
  mutate(
    PeakMax = max(FreqSmooth, na.rm = TRUE),
    Fat = FreqSmooth >= 0.7 * PeakMax,
    RunId = cumsum(c(1, diff(Fat) != 0)) * Fat
  ) %>%
  filter(Fat) %>%
  group_by(Sector, RunId) %>%
  mutate(RunLen = n()) %>%
  group_by(Sector) %>%
  filter(RunLen == max(RunLen)) %>%
  ungroup()

label_doy <- fat_runs %>%
  group_by(Sector) %>%
  summarise(LabelDOY = round(median(DOY)), PeakMax = first(PeakMax)) %>%
  ungroup()

# Build label data: one row per (DOY, Sector) but text only shown on the
# matching sector's labeling DOY. Using position_stack(vjust = 0.5) so ggplot
# computes the band centre exactly as it stacks the geom_area below.
labels_df <- doy_dist %>%
  inner_join(label_doy, by = c("Sector" = "Sector"), relationship = "many-to-many") %>%
  filter(DOY == LabelDOY) %>%
  mutate(Label = ifelse(Sector == Sector, as.character(Sector), ""))

# For each LabelDOY x, we need the full stack of all sectors at that x so
# position_stack centres correctly. Build that:
plot_labels <- label_doy %>%
  rename(LabelSector = Sector) %>%
  rowwise() %>%
  mutate(rows = list(doy_dist %>% filter(DOY == LabelDOY))) %>%
  unnest(rows) %>%
  ungroup() %>%
  mutate(Label = ifelse(Sector == LabelSector, as.character(Sector), ""))

p <- ggplot(doy_dist, aes(x = DOY, y = FreqSmooth, fill = Sector)) +
  geom_area(position = "stack", color = NA) +
  geom_text(
    data = plot_labels,
    aes(x = DOY, y = FreqSmooth, group = Sector, label = Label),
    position = position_stack(vjust = 0.5),
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
