# Mapping Bangalore weather

Map assets are in `/home/karthik/apps/bangalore-weather/data/maps/bangalore/`.
They work offline after download. All output vector layers use WGS84 / EPSG:4326;
GeoJSON coordinates are longitude, latitude (not latitude, longitude).

| Asset | Use |
|---|---|
| `bangalore_weather_maps.gpkg` | One GIS file with `weatherunion_stations`, `osm_major_roads`, `bbmp_legacy_outline` layers |
| `weatherunion_stations.geojson`, `.csv` | Station IDs, locality names and the archive's existing latitude/longitude |
| `osm_major_roads.geojson` | Motorway/trunk/primary/secondary/tertiary roads and their links, with OSM IDs and names where available |
| `bbmp_legacy_outline.geojson` | Dissolved legacy BBMP ward outline; historical reference only |
| `bbmp_legacy_wards_source.geojson` | Unmodified DataMeet ward source; do not treat as current GBA wards |
| `sources.json`, `roads.overpassql` | Download time, sources, versions, licences, checksums, bounding box and exact road query |

## Coordinates and spatial scope

The hourly archive already contains coordinates for all 32 stations. These points
preserve that metadata; no locality-centre geocoding was used. Physical station
position accuracy and the original coordinate datum have not been independently
verified; WGS84 is the plotting interpretation. Do not imply rooftop-level precision.

The road query covers the station extent with a 0.04-degree margin, including the
northern airport/Devanahalli stations. Returned OSM ways are complete and can extend
outside that box. This is a major-road reference map, not every residential street
or a routing network. Keep stations outside legacy BBMP visible; do not silently clip
them to the city outline. The boundary is an 2022 BBMP delimitation reference and is not the
current GBA/corporation/ward geography.

## Overlay workflow

1. Read `data_sources.md` and check measurement/date coverage first.
2. Aggregate the requested metric to one row per station for the requested period,
   retaining valid and expected observation counts.
3. Join to station geometry by `locality_id`, not locality text. Confirm every
   selected ID has a point. Mark unavailable measurements distinctly from zero.
4. Draw roads and the legacy outline as context, then plot station observations.
   A point observation is not a ward-wide value. Do not fill wards or interpolate
   a continuous rainfall/temperature surface without an explicitly justified method.
5. State period, units, source and coverage; include the map attributions below.
   Use EPSG:32643 (UTM zone 43N) for distance/buffer calculations in metres.

This creates a spatial daily rainfall table, ready for plotting. The date is an
example; substitute the user's period. It deliberately requires all 24 derived
hourly rainfall values; inspect source-minute coverage too before claiming complete
sensor coverage. A valid hourly accumulation difference does not guarantee 60 readings.

```r
library(arrow)
library(dplyr)
library(sf)

as.POSIXct(
  "2026-09-15 00:00:00",
  tz = "Asia/Kolkata"
) -> start_ist
end_ist <- start_ist + 24 * 60 * 60

open_dataset("data/weatherunion_bengaluru_hourly.parquet") %>%
  filter(hour_ist >= start_ist, hour_ist < end_ist) %>%
  group_by(locality_id) %>%
  summarise(
    observed_hours = n(),
    valid_rain_hours = sum(!is.na(rain_mm)),
    observed_rain_mm = sum(rain_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect() %>%
  mutate(
    daily_rain_mm = if_else(
      valid_rain_hours == 24,
      observed_rain_mm,
      NA_real_
    )
  ) -> daily_rain

st_read(
  "data/maps/bangalore/bangalore_weather_maps.gpkg",
  layer = "weatherunion_stations",
  quiet = TRUE
) -> stations
stopifnot(all(daily_rain$locality_id %in% stations$locality_id))

stations %>%
  left_join(daily_rain, by = "locality_id") -> rain_points
```

Oikolab/ERA5 is a gridded historical series, not a network of neighbourhood sensors.
Do not duplicate its Bangalore value across all WeatherUnion points and present that
as measured spatial variation. Match timestamps, metric definitions and spatial scope
before comparisons involving both sources.

## Attribution and refresh

On maps using the roads include **© OpenStreetMap contributors**, with
<https://www.openstreetmap.org/copyright> (ODbL). For the outline/wards include
**DataMeet, Spatial Data of Municipalities, CC BY-SA 2.5 India**, linking to
<https://github.com/datameet/Municipal_Spatial_Data/blob/master/Bangalore/Readme.md>. Weather station data:
**WeatherUnion**. Keep source-specific licences distinct when sharing the map package.

These are downloaded snapshots; map layers are not refreshed by the Oikolab daily
job. To rebuild into a new directory from the repository root:

```bash
Rscript analysis_questions/build_weather_map_layers.R \
  data/weatherunion_bengaluru_hourly.parquet \
  /tmp/bangalore-weather-maps-new \
  analysis_questions/download_map_sources.py
```

The builder requires R `arrow`, `dplyr`, `sf` and Python `requests` (available on the
server). It refuses to overwrite an existing output directory. Validate the new
package and retain previous layers before replacing the live map directory.
The station coordinate export should be regenerated if archive station metadata changes.
