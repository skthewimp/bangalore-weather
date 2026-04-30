# NOAA GHCN-Hourly — Alternative Bangalore Weather Source

Real station observations from the two Bangalore airport weather stations, pulled directly from NOAA's GHCN-Hourly archive. (NOAA deprecated the older ISD database in 2025; GHCN-H is the replacement.)

## Why this folder exists

The main project pulls from Oikolab, which serves ERA5 reanalysis. ERA5 is model output, not direct measurements. This folder fetches the raw airport observations (METAR/SYNOP) instead.

## What you get — and what you don't

GHCN-Hourly for the Bangalore stations contains:

| Variable | Status |
|---|---|
| Temperature (°C) | **Solid** — 30-minute reports, ~100% complete |
| Wind speed (m/s) | **Solid** |
| Wind direction (deg) | **Solid** |
| Relative humidity (%) | **Solid** |
| **Precipitation (mm)** | **Empty.** All NA. METAR doesn't carry mm totals. |
| Sea-level pressure | Empty for these stations |

So this source is a real-station replacement for **temperature and wind only**. For rainfall you still need Oikolab/ERA5 or an IMD scrape.

## Stations

| Station ID | Name | Coverage |
|---|---|---|
| `INM00043296` | Bangalore HAL Airport | 2005-present |
| `INI0000VOBL` | Kempegowda Intl Airport | 2008-present |

Lag: ~5-7 days behind real time (NOAA processing).

## Files

- `fetch_historical.R` — one-time bulk pull, both stations, all years. Saves `data/bangaloreISD.RData`.
- `update.R` — re-pulls current year for both stations, dedupes, appends.
- `compare_oikolab.R` — sanity check: ISD station obs vs Oikolab/ERA5 daily aggregates.

## Source URL pattern

```
https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/
  access/by-year/{YEAR}/psv/GHCNh_{STATION}_{YEAR}.psv
```

PSV = pipe-separated values. ~3-13 MB per station-year.

## Data shape

`blrISD` tibble:

```
DT          POSIXct   observation timestamp (UTC)
Station     chr       "VOBL" or "HAL"
Temp        dbl       air temperature, °C
WindSpd     dbl       wind speed, m/s
WindDir     dbl       wind direction, degrees
RH          dbl       relative humidity, %
```
