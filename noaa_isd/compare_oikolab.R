## compare_oikolab.R
## Sanity-check NOAA GHCN-H station obs against the project's Oikolab/ERA5 series.
## Daily aggregates only - hourly alignment between point obs and reanalysis is noisy.
## Compares temperature and wind only (GHCN-H has no rainfall for these stations).

library(tidyverse)
library(lubridate)
library(patchwork)

script_dir <- tryCatch({
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
}, error = function(e) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    getwd()
  }
})
project_dir <- dirname(script_dir)

load(file.path(script_dir,  "data", "bangaloreISD.RData"))
load(file.path(project_dir, "data", "bangaloreTemperature.RData"))
load(file.path(project_dir, "data", "bangaloreWind.RData"))

## Use VOBL when available (2008+), HAL before that
isd_daily <- blrISD %>%
  mutate(Date = as.Date(DT)) %>%
  arrange(Station == "VOBL") %>%
  distinct(Date, hour = hour(DT), minute = minute(DT), .keep_all = TRUE) %>%
  summarise(
    HighISD = max(Temp, na.rm = TRUE),
    LowISD  = min(Temp, na.rm = TRUE),
    WindISD = mean(WindSpd, na.rm = TRUE),
    .by = Date
  ) %>%
  mutate(across(c(HighISD, LowISD, WindISD), ~ ifelse(is.infinite(.), NA, .)))

oik_temp_daily <- blrTemp %>%
  filter(!is.na(Temp)) %>%
  mutate(Date = as.Date(DT), Temp = as.numeric(Temp)) %>%
  summarise(HighOik = max(Temp), LowOik = min(Temp), .by = Date)

oik_wind_daily <- blrWind %>%
  filter(!is.na(Wind)) %>%
  mutate(Date = as.Date(DT)) %>%
  summarise(WindOik = mean(as.numeric(Wind)), .by = Date)

cmp <- isd_daily %>%
  inner_join(oik_temp_daily, by = "Date") %>%
  inner_join(oik_wind_daily, by = "Date")

stats <- cmp %>%
  summarise(
    n         = n(),
    high_bias = mean(HighISD - HighOik, na.rm = TRUE),
    low_bias  = mean(LowISD  - LowOik,  na.rm = TRUE),
    wind_bias = mean(WindISD - WindOik, na.rm = TRUE),
    high_corr = cor(HighISD, HighOik, use = "complete.obs"),
    low_corr  = cor(LowISD,  LowOik,  use = "complete.obs"),
    wind_corr = cor(WindISD, WindOik, use = "complete.obs")
  )
print(stats)

p1 <- cmp %>% ggplot(aes(HighOik, HighISD)) +
  geom_point(alpha = 0.2, size = 0.5) + geom_abline(col = "red") +
  labs(title = "Daily high (°C)", x = "Oikolab", y = "GHCN-H station")
p2 <- cmp %>% ggplot(aes(LowOik, LowISD)) +
  geom_point(alpha = 0.2, size = 0.5) + geom_abline(col = "red") +
  labs(title = "Daily low (°C)", x = "Oikolab", y = "GHCN-H station")
p3 <- cmp %>% ggplot(aes(WindOik, WindISD)) +
  geom_point(alpha = 0.2, size = 0.5) + geom_abline(col = "red") +
  labs(title = "Daily mean wind (m/s)", x = "Oikolab", y = "GHCN-H station")

out <- p1 + p2 + p3 + plot_annotation(
  title = "NOAA GHCN-H station obs vs Oikolab/ERA5 reanalysis",
  subtitle = paste0("Daily aggregates, n=", stats$n, " days")
)
outfile <- file.path(script_dir, "compare_oikolab.png")
ggsave(outfile, out, width = 12, height = 4)
message("Saved: ", outfile)
out
