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
    Month = month(DT)
  ) %>%
  filter(!is.na(Wind) & !is.na(WindDir)) %>%
  mutate(
    U = -1 * Wind * sin(WindDir * pi / 180),
    V = -1 * Wind * cos(WindDir * pi / 180),
    # 24 bins (15 degrees each)
    DirBin = round(WindDir / 15) * 15,
    DirBin = ifelse(DirBin == 360, 0, DirBin)
  )

hist_dist <- blrWind %>%
  filter(Year < curr_year) %>%
  summarise(
    MeanSpeed = mean(Wind),
    Count = n(),
    .by = c(Month, DirBin)
  ) %>%
  mutate(
    Freq = Count / sum(Count),
    .by = Month
  )

calc_vector <- function(df) {
  df %>%
    summarise(
      MeanU = mean(U),
      MeanV = mean(V),
      .by = Month
    ) %>%
    mutate(
      MeanSpeed = sqrt(MeanU^2 + MeanV^2),
      MeanWindDir = atan2(-MeanU, -MeanV) * 180 / pi,
      MeanWindDir = ifelse(MeanWindDir < 0, MeanWindDir + 360, MeanWindDir)
    )
}

hist_vec <- blrWind %>% filter(Year < curr_year) %>% calc_vector()
curr_vec <- blrWind %>% filter(Year == curr_year) %>% calc_vector()

month_labels <- c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                  "9" = "September", "10" = "October", "11" = "November", "12" = "December")

p <- ggplot() +
  geom_segment(
    data = hist_dist,
    aes(x = DirBin, xend = DirBin, y = 0, yend = MeanSpeed, linewidth = Freq),
    color = "#9c9280", lineend = "round"
  ) +
  # Arrows pointing INWARDS to the origin to represent wind hitting the observer
  geom_segment(
    data = hist_vec,
    aes(x = MeanWindDir, xend = MeanWindDir, y = MeanSpeed, yend = 0),
    color = "#5f3946", linewidth = 1.5,
    arrow = arrow(length = unit(0.06, "inches"), type = "closed")
  ) +
  geom_segment(
    data = curr_vec,
    aes(x = MeanWindDir, xend = MeanWindDir, y = MeanSpeed, yend = 0),
    color = "#CD3333", linewidth = 1.5,
    arrow = arrow(length = unit(0.06, "inches"), type = "closed")
  ) +
  facet_wrap(~Month, labeller = as_labeller(month_labels), ncol = 4) +
  coord_polar(theta = "x", start = 0, direction = 1) +
  scale_linewidth_continuous(range = c(0.5, 4.5), labels = scales::percent) +
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),
    labels = c("N", "E", "S", "W"),
    limits = c(0, 360)
  ) +
  scale_y_continuous(limits = c(0, max(hist_dist$MeanSpeed) * 1.1)) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#c8c0aa", linewidth = 0.3),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 8),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = 'bold', hjust = 0, color = '#3C3C3C', size = 11),
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10, margin = margin(b = 15))
  ) +
  labs(
    title = paste("Bangalore Wind Distribution by Month"),
    subtitle = "Spoke thickness = Frequency. Arrows point INWARDS to the origin representing wind hitting the observer.",
    caption = "Data source: Oikolab (ERA5). Historical normal spans 1981-2025.",
    x = NULL,
    y = NULL,
    linewidth = NULL
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_wind_patterns.png')
ggsave(outfile, p, width = 12, height = 10, dpi = 300)
message("Saved: ", outfile)
