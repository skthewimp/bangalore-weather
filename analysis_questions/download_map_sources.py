"""Fetch a modest OSM road extract and a versioned legacy BBMP ward layer."""
import datetime
import hashlib
import json
from pathlib import Path
import sys
import requests

out = Path(sys.argv[1])
stations = json.loads((out / 'weatherunion_stations.geojson').read_text())
xy = [f['geometry']['coordinates'] for f in stations['features']]
west, south = min(p[0] for p in xy) - .04, min(p[1] for p in xy) - .04
east, north = max(p[0] for p in xy) + .04, max(p[1] for p in xy) + .04
bounds = [west, south, east, north]
classes = 'motorway|trunk|primary|secondary|tertiary|motorway_link|trunk_link|primary_link|secondary_link|tertiary_link'
query = f'[out:json][timeout:120];way["highway"~"^({classes})$"]({south},{west},{north},{east});out geom;'
(out / 'roads.overpassql').write_text(query + '\n')
endpoint = 'https://overpass-api.de/api/interpreter'
print('Downloading OSM major roads', bounds, flush=True)
r = requests.get(endpoint, params={'data': query}, headers={'User-Agent': 'BangaloreWeatherMaps/1.0 (personal weather analysis)'}, timeout=150)
r.raise_for_status()
raw = r.json()
if raw.get('remark'):
    raise RuntimeError(raw['remark'])
features = []
for e in raw['elements']:
    if e['type'] != 'way':
        continue
    coords = [[p['lon'], p['lat']] for p in e.get('geometry', [])]
    if len(coords) < 2:
        raise RuntimeError(f'Missing road geometry: {e["id"]}')
    tags = e.get('tags', {})
    features.append({'type': 'Feature', 'properties': {'osm_id': str(e['id']), **{k: tags.get(k, '') for k in ['name', 'highway', 'ref', 'bridge', 'tunnel']}}, 'geometry': {'type': 'LineString', 'coordinates': coords}})
assert features, 'No roads returned'
(out / 'osm_major_roads.geojson').write_text(json.dumps({'type':'FeatureCollection', 'features':features}, ensure_ascii=False))
print('Road segments:', len(features), flush=True)
repo = 'datameet/Municipal_Spatial_Data'
r = requests.get(f'https://api.github.com/repos/{repo}/commits', params={'path':'Bangalore/BBMP.geojson','per_page':1}, timeout=40)
r.raise_for_status()
commit = r.json()[0]['sha']
url = f'https://raw.githubusercontent.com/{repo}/{commit}/Bangalore/BBMP.geojson'
r = requests.get(url, timeout=60)
r.raise_for_status()
wards = r.json()
assert wards['type'] == 'FeatureCollection' and wards['features']
(out / 'bbmp_legacy_wards_source.geojson').write_bytes(r.content)
manifest = {
    'retrieved_at_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
    'crs': 'EPSG:4326',
    'road_query_bbox_wsen': bounds,
    'roads': {'endpoint': endpoint, 'query_file':'roads.overpassql', 'osm_timestamp_utc':raw.get('osm3s',{}).get('timestamp_osm_base'), 'segments':len(features), 'license':'ODbL-1.0', 'attribution':'© OpenStreetMap contributors', 'license_url':'https://www.openstreetmap.org/copyright', 'scope':'Major roads only; complete ways intersecting query box, not clipped; not a navigable full street network'},
    'boundary': {'url':url, 'source_commit':commit, 'features':len(wards['features']), 'license':'CC-BY-SA-2.5-IN', 'attribution':'DataMeet, Spatial Data of Municipalities', 'source_page':'https://projects.datameet.org/Municipal_Spatial_Data/', 'scope':'BBMP 2022 delimitation (243 wards), scraped from KSRSAC by DataMeet; not current GBA administrative boundaries', 'license_source':'https://github.com/datameet/Municipal_Spatial_Data/blob/master/Bangalore/Readme.md'},
    'stations': {'source':'weatherunion_bengaluru_hourly.parquet', 'coordinates':'Existing latitude/longitude metadata; not geocoded here; physical station precision and datum not independently verified. Interpreted as WGS84 for plotting.'},
}
for name in ['weatherunion_stations.geojson','osm_major_roads.geojson','bbmp_legacy_wards_source.geojson']:
    manifest.setdefault('sha256', {})[name] = hashlib.sha256((out / name).read_bytes()).hexdigest()
(out / 'sources.json').write_text(json.dumps(manifest, indent=2))
print('Legacy ward polygons:', len(wards['features']), flush=True)
