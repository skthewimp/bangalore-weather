# Bangalore weather data sources

Repository: `/home/karthik/apps/bangalore-weather`.
Choose the source by spatial scale, time horizon, and whether the question asks
about station observations or a historical baseline.

| Question | Source |
|---|---|
| Recent observed rain/temperature/wind in a named neighbourhood; locality comparisons; humidity | WeatherUnion, if the requested station, dates, and measurements are covered |
| Decades of trends, records, normals, typical seasonal or hourly patterns; older dates | Oikolab archive, ERA5 rows only |
| Recent city weather with no locality specified | WeatherUnion for observed conditions, stating which stations and aggregation; ERA5 for a question explicitly about the historical city series |
| Local recent weather in historical context | Both, separately labelled; account for point sensors versus gridded reanalysis |
| Current conditions or future forecasts | Neither archive guarantees live conditions or forecasts; report freshness and seek an appropriate live source if required |

An explicit requested source takes precedence where it can answer the question;
explain coverage limitations rather than silently replacing it. The WeatherUnion
archive is too short for multi-year records, normals, or claims of climatic trends.
Do not splice it into the ERA5 series or interpret their difference as climate change.

## WeatherUnion

- File: `data/weatherunion_bengaluru_hourly.parquet`.
- Key: `locality_id`, `hour_ist`; one hourly aggregate per station, timestamps in
  `Asia/Kolkata`. An hour timestamp identifies the start of that local hour.
- Snapshot copied on 2026-09-16: 141,571 rows, 32 stations; earliest observation
  2026-02-22, latest 2026-09-15 23:00 IST. These are snapshot facts, not ongoing guarantees.
- 31 stations reached September 15; Koramangala only has March 3–13. Other stations
  also have differing starts and gaps. Inspect the file for current coverage.
- This file is manually refreshed. The site's daily Oikolab job does **not** update it.
  The historical WeatherUnion downloader currently needs a signed-in browser session.

| Field | Meaning |
|---|---|
| `locality_id`, `city_name`, `locality_name` | Station identifier and source locality labels |
| `current_locality_name`, `latitude`, `longitude` | Station lookup metadata; use ID for joins |
| `device_type_as_of_2026_08_22` | Device metadata as of that date, not a live classification |
| `hour_ist` | Hour start in IST |
| `source_rows`, `source_minutes` | Raw reading count and distinct source timestamps contributing to the hour; duplicates were collapsed before hourly aggregation |
| `temperature_c` | Mean temperature, °C |
| `humidity_pct` | Mean relative humidity, % |
| `wind_speed_ms` | Mean speed, m/s |
| `wind_direction_deg` | Circular mean direction, degrees; speed-weighted, with fallback weight 1 for missing/nonpositive speed |
| `rain_intensity_max_mm_min` | Maximum reported intensity during the hour, mm/min; not mean intensity or hourly rainfall |
| `rain_accumulation_end_mm` | Maximum reported daily accumulation within the hour, mm; the name does not guarantee a reading at the exact hour end |
| `rain_mm` | Derived hourly rain: difference between consecutive hourly accumulations; at midnight uses the new day's accumulation; gaps/missing previous values/negative non-midnight changes produce NA |

The stored temperatures are hourly means. Daily maxima/minima of these are extrema
of hourly means, not the true minute-level temperature extremes. Raw minute readings
and air-quality measurements are not retained in this Parquet file.

### Coverage and missing data

From the repository root run:

```bash
Rscript analysis_questions/weatherunion_coverage.R
```

This queries the file for per-station date limits, hour counts and valid measurement
counts. For an answer, repeat the check inside the requested time window and for the
actual variables used. Overall source-minute counts do not guarantee each sensor
metric is present for every minute. Match overlapping valid hours when comparing
stations. Report expected versus observed hours and gaps before comparing daily totals.
Never turn absent rainfall into zero with an unchecked `sum(..., na.rm = TRUE)`.
Daily cumulative rain should not be summed; use derived hourly rain with a stated
coverage rule. Do not sum rainfall depths across stations as a Bangalore rainfall total.

Query through Arrow and collect only the filtered/aggregated results. For example,
this lists localities and their coverage without loading the full archive into R:

```r
library(arrow)
library(dplyr)

open_dataset("data/weatherunion_bengaluru_hourly.parquet") -> weatherunion

weatherunion %>%
  group_by(locality_id, current_locality_name) %>%
  summarise(
    first_ist = min(hour_ist),
    latest_ist = max(hour_ist),
    hours = n(),
    valid_rain_hours = sum(!is.na(rain_mm)),
    valid_temperature_hours = sum(!is.na(temperature_c)),
    .groups = "drop"
  ) %>%
  collect() -> station_coverage
```

Use `as.POSIXct(..., tz = "Asia/Kolkata")` for requested time bounds and filter on
`hour_ist` before aggregation. Resolve locality names from the dataset; do not invent
a station or substitute another locality without saying so.

## Oikolab / ERA5

- Files: `data/bangaloreTemperature.RData`, `data/bangaloreRainfall.RData`,
  `data/bangaloreWind.RData`; existing notebooks demonstrate their objects and columns.
- Long-term gridded reanalysis archive, starting in 1981; not a neighbourhood station network.
- Filter `Source == "era5"` before historical analysis. GFS-backed rows can be revised
  and remain excluded from analytical posts. Check the latest **ERA5** date rather
  than the latest timestamp across all sources.
- Updated by the existing daily job at 08:00 IST. A successful job does not imply
  ERA5 is current through today or that WeatherUnion was refreshed.
- Inspect timezone and timestamp alignment before comparisons. ERA5 samples and
  WeatherUnion hour aggregates do not necessarily describe identical time intervals.
- Compare matching calendar windows for partial periods. A recent local observation
  can accompany an ERA5 historical baseline, but does not establish a same-series record.

Record selected source(s), location(s), period, aggregation, freshness and coverage
limitations in the notebook and reply. Retain the existing Hermes draft/publish rules.

## Map layers

Station coordinates, OSM major roads and a legacy BBMP outline are stored in
`data/maps/bangalore/` as GeoJSON and GeoPackage. Read [mapping.md](mapping.md)
for coordinate provenance, layer names, joining examples, CRS, coverage and attribution.
