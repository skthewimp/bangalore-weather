library(arrow)
library(dplyr)

commandArgs(trailingOnly = TRUE) -> args
path <- if (length(args)) args[[1]] else "data/weatherunion_bengaluru_hourly.parquet"

open_dataset(path) %>%
  group_by(locality_id, current_locality_name) %>%
  summarise(
    first_ist = min(hour_ist),
    latest_ist = max(hour_ist),
    hours = n(),
    source_minutes = sum(source_minutes),
    valid_temperature_hours = sum(!is.na(temperature_c)),
    valid_humidity_hours = sum(!is.na(humidity_pct)),
    valid_wind_speed_hours = sum(!is.na(wind_speed_ms)),
    valid_wind_direction_hours = sum(!is.na(wind_direction_deg)),
    valid_rain_hours = sum(!is.na(rain_mm)),
    valid_rain_intensity_hours = sum(!is.na(rain_intensity_max_mm_min)),
    .groups = "drop"
  ) %>%
  collect() %>%
  arrange(current_locality_name) -> coverage

cat("WeatherUnion file:", normalizePath(path), "\n")
cat("Checked at:", format(Sys.time(), tz = "Asia/Kolkata"), "IST\n")
print(
  coverage,
  n = Inf,
  width = Inf
)
