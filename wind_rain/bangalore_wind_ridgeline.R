library(tidytable)
library(tidyverse)
library(lubridate)
library(ggridges)
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
  filter(!is.na(Wind) & !is.na(WindDir), Year < curr_year)

# Bin direction into 24 bins of 15 deg, count per month, normalise to %
bin_width <- 15
month_dist <- blrWind %>%
  mutate(DirBin = floor(WindDir / bin_width) * bin_width + bin_width / 2) %>%
  summarise(Count = n(), .by = c(Month, DirBin)) %>%
  mutate(Freq = Count / sum(Count) * 100, .by = Month)

# Need wraparound: replicate first bin at 360 so curve closes smoothly
wrap_extra <- month_dist %>% filter(DirBin == bin_width / 2) %>% mutate(DirBin = 360 + bin_width / 2)
month_dist <- bind_rows(month_dist, wrap_extra)

month_labels <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")
# Reverse so January is on top
month_dist <- month_dist %>%
  mutate(MonthLabel = factor(month_labels[Month], levels = rev(month_labels)))

# Cardinal direction reference lines
card_lines <- tibble(
  x = c(0, 90, 180, 270, 360),
  label = c("N", "E", "S", "W", "N")
)

p <- ggplot(month_dist, aes(x = DirBin, y = MonthLabel, height = Freq, group = MonthLabel)) +
  geom_vline(data = card_lines, aes(xintercept = x), color = '#c8c0aa', linewidth = 0.3) +
  geom_ridgeline(
    aes(fill = MonthLabel),
    scale = 0.35, alpha = 0.85, color = '#3C3C3C', linewidth = 0.3
  ) +
  scale_x_continuous(
    breaks = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
    limits = c(0, 360),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_d(option = "mako", begin = 0.15, end = 0.9, direction = -1) +
  ggthemes::theme_tufte() +
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 10),
    axis.text.y = element_text(size = 11),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 25, 15, 15),
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 16),
    plot.subtitle = element_text(color = '#5f3946', size = 10, margin = margin(b = 15)),
    plot.caption = element_text(color = '#5f3946', size = 8)
  ) +
  labs(
    title = "How Bangalore's Wind Direction Shifts Through The Year",
    subtitle = "Each ridge shows the % of hours wind blew from each direction in that month. Watch the easterly winter\nshift to a strong westerly between May and September - the southwest monsoon arriving and leaving.",
    caption = "Data source: Oikolab (ERA5). 1981-2025. Bins of 15 degrees.",
    x = NULL, y = NULL
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_wind_ridgeline.png')
ggsave(outfile, p, width = 11, height = 9, dpi = 300)
message("Saved: ", outfile)
