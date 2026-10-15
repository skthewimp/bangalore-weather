library(arrow)
library(dplyr)
library(sf)

commandArgs(trailingOnly = TRUE) -> args
stopifnot(length(args) == 3)
archive_path <- args[[1]]
output_dir <- args[[2]]
download_script <- args[[3]]
if (dir.exists(output_dir)) stop("Use a new output directory to preserve existing map layers.")
dir.create(output_dir, recursive = TRUE)

open_dataset(archive_path) %>%
  select(locality_id, current_locality_name, latitude, longitude) %>%
  distinct() %>%
  collect() -> stations
stopifnot(
  !anyDuplicated(stations$locality_id),
  all(is.finite(stations$latitude)),
  all(is.finite(stations$longitude)),
  all(abs(stations$latitude) <= 90),
  all(abs(stations$longitude) <= 180)
)

stations %>%
  mutate(coordinate_source = "Existing WeatherUnion archive metadata; precision unverified") %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) -> points
write.csv(
  stations,
  file.path(output_dir, "weatherunion_stations.csv"),
  row.names = FALSE
)
st_write(
  points,
  file.path(output_dir, "weatherunion_stations.geojson"),
  quiet = TRUE
)
system2(
  "python3",
  c(shQuote(download_script), shQuote(output_dir))
) -> download_status
stopifnot(download_status == 0)

st_read(
  file.path(output_dir, "bbmp_legacy_wards_source.geojson"),
  quiet = TRUE
) %>%
  st_transform(32643) %>%
  st_make_valid() %>%
  st_union() %>%
  st_transform(4326) -> outline_geometry
st_sf(
  name = "Legacy BBMP outline (DataMeet)",
  boundary_status = "Historical reference; not current GBA boundary",
  geometry = outline_geometry
) -> outline
st_write(
  outline,
  file.path(output_dir, "bbmp_legacy_outline.geojson"),
  quiet = TRUE
)
st_read(
  file.path(output_dir, "osm_major_roads.geojson"),
  quiet = TRUE
) -> roads

list(
  weatherunion_stations = points,
  osm_major_roads = roads,
  bbmp_legacy_outline = outline
) -> layers
package_path <- file.path(output_dir, "bangalore_weather_maps.gpkg")
for (layer_name in names(layers)) {
  st_write(
    layers[[layer_name]],
    package_path,
    layer = layer_name,
    quiet = TRUE
  )
  st_read(
    package_path,
    layer = layer_name,
    quiet = TRUE
  ) -> reread
  stopifnot(
    nrow(reread) == nrow(layers[[layer_name]]),
    st_crs(reread)$epsg == 4326,
    !any(st_is_empty(reread)),
    all(st_is_valid(st_transform(reread, 32643)))
  )
}
cat("Validated GeoPackage layers:\n")
print(st_layers(package_path))
