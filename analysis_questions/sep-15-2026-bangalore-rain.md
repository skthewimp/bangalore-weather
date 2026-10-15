# Bangalore's September 15 rain: where it arrived, how much fell, and what it did to temperature

- **Question:** Yesterday, there was fairly heavy rain in parts of Bangalore. Can we map how rain moved across WeatherUnion stations, show how much fell in different parts of town, and examine the impact on temperature?
- **Trigger prompt:** Yesterday, there was fairly heavy rain in parts of Bangalore. Now, can you map out maybe a line graph for each weather station from Weather Union or something like that, how the rain went in terms of where it started raining first, where it rained later? Another thing you could possibly do is on a map, show how much it rained in different parts of town and stuff. Also, another analysis to do related to this is what impact did the rain have on temperature in different parts of Bangalore?
- **Date analysed:** 2026-09-15 (IST)
- **Source:** WeatherUnion hourly archive snapshot, `data/weatherunion_bengaluru_hourly.parquet`, copied 2026-09-16.

## Analysis contract

- **Unit:** station-hour; station-day for the map and onset summary.
- **Rain start:** first hourly `rain_mm > 0.1` after midnight IST. This is an hourly sensor-derived onset, not the exact minute rain began.
- **Rain total:** sum of derived hourly `rain_mm`; a station total is reported only when all 24 hourly rain values are valid.
- **Temperature impact:** among the 18 stations with temperature coverage, compare the mean temperature in the three hours before the first rain hour with the mean in the first three hours starting at rain onset. This is an association/descriptive before-after comparison, not a causal estimate.
- **Missingness:** MG Road, HSR Layout, Varthur, Rajarajeshwari Nagar, Whitefield, Marathahalli, Devanahalli, BIAL Airport Road, Yelahanka, Aavalahalli, Brookefields, Kammanahalli and Nagavara have no temperature values in this day's snapshot; rain remains available for all 31 stations reaching September 15.
- **Spatial caveat:** points are station observations, not ward-wide estimates. No interpolation is used.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE, message=FALSE, warning=FALSE, fig.width=12, fig.height=7, dpi=150)
library(tidyverse)
library(lubridate)
library(arrow)
library(sf)
library(scales)
library(grid)
root_dir <- normalizePath(file.path(dirname(knitr::current_input()), ".."))
chart_dir <- file.path(root_dir, "docs", "assets", "analysis")
dir.create(chart_dir, recursive=TRUE, showWarnings=FALSE)
weatherunion <- open_dataset(file.path(root_dir,"data","weatherunion_bengaluru_hourly.parquet"))
start_ist <- as.POSIXct("2026-09-15 00:00:00", tz="Asia/Kolkata")
end_ist <- as.POSIXct("2026-09-16 00:00:00", tz="Asia/Kolkata")
w <- weatherunion %>% filter(hour_ist >= start_ist, hour_ist < end_ist) %>% select(locality_id,current_locality_name,hour_ist,rain_mm,temperature_c,latitude,longitude,source_minutes) %>% collect() %>% arrange(locality_id,hour_ist)
w <- w %>% group_by(locality_id) %>% mutate(hour=hour(hour_ist), cumulative_rain=cumsum(replace_na(rain_mm,0)), rain_event=replace_na(rain_mm,0)>0.1) %>% ungroup()
onsets <- w %>% filter(rain_event) %>% group_by(locality_id,current_locality_name) %>% summarise(first_rain_hour=min(hour), .groups="drop")
daily <- w %>% group_by(locality_id,current_locality_name,latitude,longitude) %>% summarise(observed_hours=n(), valid_rain_hours=sum(!is.na(rain_mm)), rain_mm=sum(rain_mm,na.rm=TRUE), .groups="drop") %>% mutate(rain_mm=if_else(valid_rain_hours==24,rain_mm,NA_real_)) %>% left_join(onsets,by=c("locality_id","current_locality_name"))
temp_effect <- w %>% left_join(onsets,by=c("locality_id","current_locality_name")) %>% filter(!is.na(temperature_c), !is.na(first_rain_hour)) %>% group_by(locality_id,current_locality_name,first_rain_hour) %>% summarise(pre_temp=mean(temperature_c[hour >= first_rain_hour-3 & hour < first_rain_hour],na.rm=TRUE), post_temp=mean(temperature_c[hour >= first_rain_hour & hour < first_rain_hour+3],na.rm=TRUE), rain_total=sum(rain_mm,na.rm=TRUE), .groups="drop") %>% mutate(temp_change_c=post_temp-pre_temp)
base_theme <- theme_minimal(base_size=10) + theme(plot.background=element_rect(fill="white",colour=NA), panel.background=element_rect(fill="white",colour=NA), panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), axis.text=element_text(colour="#3c3c3c"), strip.text=element_text(face="bold",colour="#3c3c3c"), plot.title=element_text(face="bold",colour="#3c3c3c"), plot.subtitle=element_text(colour="#666666"), plot.caption=element_text(colour="#666666",size=8))
```

## First, pulse check

```{r facts}
daily %>% arrange(desc(rain_mm)) %>% select(current_locality_name,rain_mm,first_rain_hour,valid_rain_hours) %>% print(n=40)
temp_effect %>% arrange(temp_change_c) %>% select(current_locality_name,first_rain_hour,pre_temp,post_temp,temp_change_c) %>% print(n=30)
```

Across 31 stations, the rain totals are complete at the hourly level. Temperature is available at 18 stations. The onset chart uses cumulative rain, so a flat line means no additional rain at that station; the first upward step is the first hourly rain window.

## Where did the rain arrive first?

```{r rain-lines}
line_path <- file.path(chart_dir,"bangalore-sep-15-2026-rain-arrival.png")
p1 <- w %>% mutate(station=fct_reorder(current_locality_name,cumulative_rain,.fun=max)) %>% ggplot(aes(hour,cumulative_rain,group=locality_id)) + geom_step(colour="#176b87",linewidth=.55) + geom_point(data=w %>% filter(rain_event),aes(hour,cumulative_rain),colour="#e56b3f",size=.6) + facet_wrap(~station,ncol=4,scales="free_y") + scale_x_continuous(breaks=c(0,6,12,18,23),labels=function(x)sprintf("%02d:00",x)) + labs(title="Rain arrived in waves across Bangalore on 15 September",subtitle="Cumulative hourly rain by WeatherUnion station; orange points mark hourly rain > 0.1 mm",x="Hour (IST)",y="Cumulative rain (mm)",caption="WeatherUnion hourly archive snapshot; station observations, not area-wide estimates.") + base_theme
p1
ggsave(line_path,p1,width=12,height=10,dpi=180,bg="white")
```

The onset ordering is shown in the facts table and in the step-up locations in the small multiples. Because this is hourly data, stations with the same first-rain hour are tied; the plot cannot resolve which sensor received the first drops within that hour.

## How much fell where?

```{r rain-map}
map_path <- file.path(chart_dir,"bangalore-sep-15-2026-rain-map.png")
stations <- st_read(file.path(root_dir,"data/maps/bangalore/bangalore_weather_maps.gpkg"),layer="weatherunion_stations",quiet=TRUE)
roads <- st_read(file.path(root_dir,"data/maps/bangalore/bangalore_weather_maps.gpkg"),layer="osm_major_roads",quiet=TRUE)
outline <- st_read(file.path(root_dir,"data/maps/bangalore/bangalore_weather_maps.gpkg"),layer="bbmp_legacy_outline",quiet=TRUE)
rain_points <- stations %>% left_join(daily,by="locality_id")
p2 <- ggplot() + geom_sf(data=roads,colour="grey88",linewidth=.18) + geom_sf(data=outline,fill=NA,colour="grey65",linewidth=.3) + geom_sf(data=rain_points %>% filter(!is.na(rain_mm)),aes(colour=rain_mm),size=3.2) + geom_sf_text(data=rain_points %>% filter(!is.na(rain_mm) & rain_mm >= 30),aes(label=current_locality_name),size=2.8,nudge_y=.012,check_overlap=TRUE) + scale_colour_gradient(low="#cfe8f3",high="#b2182b",trans="sqrt",labels=label_number(suffix=" mm")) + labs(title="Rainfall was highly uneven across Bangalore",subtitle="24-hour station totals for 15 September 2026; colour scale uses square-root spacing",colour="Rain",caption="WeatherUnion stations. Roads: © OpenStreetMap contributors. Boundary: legacy BBMP reference / DataMeet; not a current ward map.") + coord_sf(expand=FALSE) + base_theme + theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),panel.grid=element_blank(),legend.position="right")
p2
ggsave(map_path,p2,width=10,height=8,dpi=180,bg="white")
```

This map shows measured station points, not a continuous rainfall surface. Rajajinagar, MG Road, New BEL Road and HSR Layout were among the wettest stations; the exact ranking is in the facts table above.

## What happened to temperature?

```{r temp-impact}
temp_path <- file.path(chart_dir,"bangalore-sep-15-2026-temperature-rain-impact.png")
p3 <- temp_effect %>% mutate(station=fct_reorder(current_locality_name,temp_change_c)) %>% ggplot(aes(temp_change_c,station)) + geom_vline(xintercept=0,colour="grey65",linewidth=.35) + geom_segment(aes(x=0,xend=temp_change_c,y=station,yend=station),colour="#9bb7c2",linewidth=1) + geom_point(aes(colour=temp_change_c),size=3) + scale_colour_gradient2(low="#2166ac",mid="#f7f7f7",high="#b2182b",midpoint=0,labels=label_number(suffix="°C")) + labs(title="Rain cooled some stations sharply, but not all equally",subtitle="Change from the 3 hours before onset to the first 3 hours after onset; only stations with temperature data",x="Temperature change (°C)",y=NULL,colour="Change",caption="Descriptive before-after comparison of hourly mean temperatures; not a causal estimate. WeatherUnion snapshot.") + base_theme + theme(legend.position="right")
p3
ggsave(temp_path,p3,width=10,height=7,dpi=180,bg="white")
```

This comparison is deliberately modest: it measures a local before-after change around each station's own rain onset, not the causal effect of rain. It also excludes 13 stations with no temperature data for this date.

## Checks and verdict

- All 31 stations in the September 15 window have 24 observed and valid hourly rainfall values.
- Temperature is complete for 18 of 31 stations; no temperature impact claim is made for the other 13.
- Rain is derived from changes in reported accumulation; it is not a direct minute-level gauge total.
- The map does not interpolate between sensors or sum station values into a city total.

**Insightful: yes, exploratory only.** The station-by-station view can show that this was a spatially uneven event with different onset hours, while the map makes the uneven totals immediately visible. The temperature panel is useful as a descriptive companion, but the one-day before-after comparison is not enough for a causal or general climate claim. Do not publish without a separate historical or multi-event follow-up.

**Charts:**

- `docs/assets/analysis/bangalore-sep-15-2026-rain-arrival.png`
- `docs/assets/analysis/bangalore-sep-15-2026-rain-map.png`
- `docs/assets/analysis/bangalore-sep-15-2026-temperature-rain-impact.png`

**Blog post:** not published; this is a draft analysis from the chat workflow.
