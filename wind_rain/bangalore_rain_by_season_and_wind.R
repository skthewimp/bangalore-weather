library(tidytable)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(patchwork)

project_dir <- '/Users/Karthik/Documents/work/data work/bangalore/weather'
load(file.path(project_dir, 'data', 'bangaloreWind.RData'))
load(file.path(project_dir, 'data', 'bangaloreRainfall.RData'))

curr_year <- 2026

wind <- blrWind %>%
  mutate(WindDir = as.numeric(WindDir), Year = year(DT), Month = month(DT)) %>%
  filter(!is.na(WindDir), Year < curr_year) %>%
  select(DT, Year, Month, WindDir)

rain <- blrRain %>%
  mutate(Rain = as.numeric(Rain), Year = year(DT), Month = month(DT)) %>%
  filter(!is.na(Rain), Year < curr_year) %>%
  select(DT, Rain)

sector_levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
df <- wind %>%
  inner_join(rain, by = "DT") %>%
  mutate(
    SectorIdx = floor(((WindDir + 22.5) %% 360) / 45) + 1,
    Sector = factor(sector_levels[SectorIdx], levels = sector_levels),
    Season = case_when(
      Month %in% c(4, 5)       ~ "Pre-monsoon (Apr-May)",
      Month %in% c(6, 7, 8, 9) ~ "SW monsoon (Jun-Sep)",
      Month %in% c(10, 11)     ~ "NE monsoon (Oct-Nov)",
      TRUE                     ~ NA_character_
    )
  ) %>%
  filter(!is.na(Season)) %>%
  mutate(Season = factor(Season, levels = c(
    "Pre-monsoon (Apr-May)", "SW monsoon (Jun-Sep)", "NE monsoon (Oct-Nov)"
  )))

n_years <- n_distinct(df$Year)

by_season_sector <- df %>%
  summarise(RainMM = sum(Rain) / n_years, .by = c(Season, Sector)) %>%
  mutate(SeasonTotal = sum(RainMM), .by = Season) %>%
  mutate(Share = RainMM / SeasonTotal * 100)

cat("\n========= RAIN BY SEASON & WIND SECTOR (mean per year) =========\n")
print(
  by_season_sector %>%
    arrange(Season, Sector) %>%
    mutate(RainMM = round(RainMM, 1), Share = round(Share, 1),
           SeasonTotal = round(SeasonTotal, 0)) %>%
    as.data.frame()
)

cat("\n========= TOP 3 WIND SECTORS BY SEASON =========\n")
top3 <- by_season_sector %>%
  arrange(Season, desc(Share)) %>%
  group_by(Season) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  mutate(RainMM = round(RainMM, 1), Share = round(Share, 1))
print(as.data.frame(top3))
cat("\n")

stack_order <- c("E", "SE", "S", "SW", "W", "NW", "N", "NE")
by_season_sector <- by_season_sector %>%
  mutate(Sector = factor(Sector, levels = stack_order))

sector_colors <- c(
  "N"  = "#7a8c99", "NE" = "#b08968", "E"  = "#c97b3a", "SE" = "#a8543a",
  "S"  = "#5f3946", "SW" = "#4a6b7c", "W"  = "#2e7a8a", "NW" = "#5a8a9a"
)

# Reverse season factor so Pre-monsoon ends up at top of horizontal bars
by_season_sector <- by_season_sector %>%
  mutate(Season = fct_rev(Season))

base_theme <- list(
  ggthemes::theme_tufte(),
  theme(
    panel.background = element_rect(fill = '#e5e1d8', linewidth = 0),
    plot.background  = element_rect(fill = '#e5e1d8', linewidth = 0),
    panel.grid = element_blank(),
    axis.text = element_text(face = 'bold', color = '#3C3C3C', size = 10),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = 'bold', color = '#3C3C3C', size = 13),
    plot.subtitle = element_text(color = '#5f3946', size = 9)
  )
)

# --- Panel A: absolute mm, stacked horizontal bar (one per season) ---
season_totals <- by_season_sector %>%
  summarise(SeasonTotal = first(SeasonTotal), .by = Season)

p_abs <- ggplot(by_season_sector, aes(y = Season, x = RainMM, fill = Sector)) +
  geom_col(width = 0.65, color = NA, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = season_totals,
    aes(y = Season, x = SeasonTotal, label = paste0(round(SeasonTotal), " mm")),
    inherit.aes = FALSE, hjust = -0.1, size = 3.5, color = '#3C3C3C', fontface = 'bold'
  ) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, " mm"),
                     expand = expansion(mult = c(0, 0.12))) +
  base_theme +
  labs(
    title = "Bangalore: rainfall by season, decomposed by wind direction",
    subtitle = "Bar length = total mean rainfall per season (mm/year). Coloured segments = mm contributed by hours when wind blew from each sector.",
    x = NULL, y = NULL
  )

# --- Panel B: same, normalized to 100% share ---
p_share <- ggplot(by_season_sector, aes(y = Season, x = Share, fill = Sector)) +
  geom_col(width = 0.65, color = NA, position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = ifelse(Share >= 4,
                       paste0(as.character(Sector), "\n", round(Share), "%"), "")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = "white", fontface = "bold", size = 3, lineheight = 0.9
  ) +
  scale_fill_manual(values = sector_colors, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  base_theme +
  labs(
    title = "Same data, normalized: share of each season's rain by wind direction",
    subtitle = "Pre-monsoon's rain comes from a much wider mix of directions than the SW monsoon's, which is overwhelmingly westerly.",
    x = NULL, y = NULL
  )

combined <- (p_abs / p_share) +
  plot_annotation(
    caption = paste0("Data: Oikolab (ERA5) hourly, 1981-", curr_year - 1,
                     ". Each rainy hour's rainfall attributed to the concurrent wind sector."),
    theme = theme(
      plot.background = element_rect(fill = '#e5e1d8', color = NA),
      plot.caption = element_text(color = '#5f3946', size = 8)
    )
  )

outfile <- file.path(project_dir, 'wind_rain', 'charts', 'bangalore_rain_by_season_and_wind.png')
ggsave(outfile, combined, width = 12, height = 8, dpi = 300, bg = '#e5e1d8')
message("Saved: ", outfile)
