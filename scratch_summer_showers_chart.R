library(tidyverse)
library(lubridate)
library(scales)

res <- readRDS("scratch_summer_showers_results.rds")
baseline <- res$baseline

bin_levels <- c("Apr 1-15", "Apr 16-30", "May 1-15", "May 16-31")
baseline$bin <- factor(baseline$bin, levels = bin_levels)

# Long form: peak + late temp by rainy / dry, by half-month
plot_df <- baseline %>%
  group_by(bin, rainy_pm) %>%
  summarise(
    Peak = mean(t_peak),
    Evening = mean(t_late),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Peak, Evening), names_to = "moment", values_to = "temp") %>%
  mutate(
    moment = factor(moment, levels = c("Peak", "Evening")),
    type = ifelse(rainy_pm, "Rainy afternoon", "Dry afternoon")
  )

p1 <- ggplot(plot_df, aes(moment, temp, group = type, color = type)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.1f", temp)),
            vjust = -0.9, size = 3.2, show.legend = FALSE) +
  facet_wrap(~bin, nrow = 1) +
  scale_color_manual(values = c("Dry afternoon" = "#d95f02",
                                 "Rainy afternoon" = "#1f78b4")) +
  scale_y_continuous(labels = label_number(suffix = "°C")) +
  labs(
    title = "Do summer showers cool Bangalore?",
    subtitle = "Mean afternoon-peak temp (1-4 PM) vs evening low (5-10 PM), 1981-2026\nRainy = >=1mm rain between 2-10 PM",
    x = NULL, y = NULL, color = NULL,
    caption = "Hourly ERA5 reanalysis, lat-lon (12.97, 77.59)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("charts/summer_showers_cooling.png", p1, width = 11, height = 5.5, dpi = 150, bg = "white")

# Frequency chart
freq <- res$freq %>% mutate(bin = factor(bin, levels = bin_levels))
p2 <- ggplot(freq, aes(bin, pct)) +
  geom_col(fill = "#1f78b4") +
  geom_text(aes(label = sprintf("%.0f%%", pct)), vjust = -0.4, size = 4) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 60)) +
  labs(
    title = "How often does it actually rain on a summer afternoon?",
    subtitle = "Share of Apr-May days with >=1 mm rain between 2-10 PM",
    x = NULL, y = NULL,
    caption = "Hourly ERA5 reanalysis, 1981-2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("charts/summer_showers_frequency.png", p2, width = 9, height = 5, dpi = 150, bg = "white")

cat("Charts saved.\n")
