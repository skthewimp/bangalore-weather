library(tidytable)
library(tidyverse)
library(lubridate)
library(ggthemes)

project_dir <- '/Users/Karthik/Documents/work/data work/bangalore/weather'

load(file.path(project_dir, 'data', 'bangaloreWind.RData'))

curr_year <- 2026
bin_width <- 15
sector_levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

period_lookup <- tibble(
  Month = rep(1:12, each = 2),
  Half = rep(c("H1", "H2"), times = 12)
) %>%
  mutate(
    PeriodIndex = row_number(),
    HalfLabel = if_else(Half == "H1", "1-15", "16-end"),
    Season = case_when(
      Month %in% c(1, 2, 12) ~ "Easterly season",
      Month %in% c(3, 4, 5) ~ "Pre-monsoon turn",
      Month %in% c(6, 7, 8, 9) ~ "SW monsoon",
      TRUE ~ "Retreat"
    ),
    SeasonColor = case_when(
      Season == "Easterly season" ~ "#b08968",
      Season == "Pre-monsoon turn" ~ "#a8543a",
      Season == "SW monsoon" ~ "#2e7a8a",
      TRUE ~ "#5f3946"
    )
  )

wind <- blrWind %>%
  mutate(
    Wind = as.numeric(Wind),
    WindDir = as.numeric(WindDir),
    Year = year(DT),
    Month = month(DT),
    Day = day(DT),
    Half = if_else(Day <= 15, "H1", "H2")
  ) %>%
  filter(!is.na(Wind), !is.na(WindDir), Year < curr_year) %>%
  left_join(period_lookup, by = c("Month", "Half")) %>%
  mutate(
    U = -1 * Wind * sin(WindDir * pi / 180),
    V = -1 * Wind * cos(WindDir * pi / 180),
    DirBin = floor(WindDir / bin_width) * bin_width + bin_width / 2,
    DirBin = if_else(DirBin >= 360, DirBin - 360, DirBin),
    SectorIdx = floor(((WindDir + 22.5) %% 360) / 45) + 1,
    Sector = factor(sector_levels[SectorIdx], levels = sector_levels)
  )

dist_data <- wind %>%
  summarise(
    Count = n(),
    MeanSpeed = mean(Wind),
    SeasonColor = first(SeasonColor),
    .by = c(PeriodIndex, Month, Half, HalfLabel, Season, DirBin)
  ) %>%
  mutate(
    Freq = Count / sum(Count),
    .by = PeriodIndex
  )

vec_data <- wind %>%
  summarise(
    MeanU = mean(U),
    MeanV = mean(V),
    MeanWind = mean(Wind),
    SeasonColor = first(SeasonColor),
    .by = c(PeriodIndex, Month, Half, HalfLabel, Season)
  ) %>%
  mutate(
    MeanVector = sqrt(MeanU^2 + MeanV^2),
    MeanWindDir = atan2(-MeanU, -MeanV) * 180 / pi,
    MeanWindDir = if_else(MeanWindDir < 0, MeanWindDir + 360, MeanWindDir)
  )

dominant_data <- wind %>%
  summarise(
    WindSum = sum(Wind),
    .by = c(PeriodIndex, Month, Half, HalfLabel, Sector)
  ) %>%
  mutate(Share = WindSum / sum(WindSum), .by = PeriodIndex) %>%
  slice_max(Share, n = 1, with_ties = FALSE, by = PeriodIndex) %>%
  mutate(
    Label = paste0(month.abb[Month], " ", HalfLabel, "\n", Sector, " ", round(Share * 100), "%")
  ) %>%
  select(PeriodIndex, Label)

period_labels <- dominant_data$Label
names(period_labels) <- dominant_data$PeriodIndex

max_radius <- max(dist_data$MeanSpeed, na.rm = TRUE) * 1.08

p <- ggplot() +
  geom_segment(
    data = dist_data,
    aes(
      x = DirBin,
      xend = DirBin,
      y = 0,
      yend = MeanSpeed,
      linewidth = Freq,
      color = SeasonColor
    ),
    lineend = "round",
    alpha = 0.95
  ) +
  geom_segment(
    data = vec_data,
    aes(x = MeanWindDir, xend = MeanWindDir, y = MeanVector, yend = 0),
    color = "#3C3C3C",
    linewidth = 1.1,
    arrow = arrow(length = unit(0.04, "inches"), type = "closed")
  ) +
  annotate("point", x = 0, y = 0, color = "#3C3C3C", size = 0.45) +
  facet_wrap(~PeriodIndex, ncol = 6, labeller = as_labeller(period_labels)) +
  coord_polar(theta = "x", start = 0, direction = 1, clip = "off") +
  scale_color_identity() +
  scale_linewidth_continuous(range = c(0.4, 4.1)) +
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),
    labels = c("N", "E", "S", "W"),
    limits = c(0, 360)
  ) +
  scale_y_continuous(limits = c(0, max_radius)) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = "#e5e1d8", linewidth = 0),
    plot.background = element_rect(fill = "#e5e1d8", linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = "bold", color = "#3C3C3C", size = 6.5),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", color = "#3C3C3C", size = 8, hjust = 0),
    legend.position = "none",
    plot.margin = margin(18, 18, 18, 18),
    plot.title = element_text(face = "bold", color = "#3C3C3C", size = 17),
    plot.subtitle = element_text(color = "#5f3946", size = 10, margin = margin(b = 14)),
    plot.caption = element_text(color = "#5f3946", size = 8)
  ) +
  labs(
    title = "Bangalore's Wind, Split Into 24 Half-Month Windows",
    subtitle = paste0(
      "Each panel is one half-month across 1981-2025. Spoke length shows average wind speed from that direction;\n",
      "spoke thickness shows how often it blows. The label's second line gives the dominant sector and its wind-weighted share."
    ),
    caption = "Data source: Oikolab (ERA5). Arrow shows the mean wind vector. Panel colour marks the broad season.",
    x = NULL,
    y = NULL
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_wind_halfmonth_roses.png')
ggsave(outfile, p, width = 14, height = 10.5, dpi = 300)
message("Saved: ", outfile)
