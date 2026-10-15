library(tidyverse)
library(arrow)
library(lubridate)
library(grid)
library(gridExtra)
root_dir <- "/home/karthik/apps/bangalore-weather"
out_dir <- file.path(root_dir, "docs/assets/analysis")
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
d <- open_dataset(file.path(root_dir,"data/weatherunion_bengaluru_hourly.parquet"))
make_table <- function(day, label) {
  start <- as.POSIXct(paste0("2026-09-", day, " 00:00:00"), tz="Asia/Kolkata")
  end <- as.POSIXct(paste0("2026-09-", ifelse(day=="14","15","16"), " 00:00:00"), tz="Asia/Kolkata")
  x <- d %>% filter(hour_ist >= start, hour_ist < end) %>%
    select(current_locality_name, rain_mm) %>% collect() %>%
    group_by(current_locality_name) %>%
    summarise(mm=sum(rain_mm,na.rm=TRUE), valid=sum(!is.na(rain_mm)), .groups="drop") %>%
    filter(valid==24) %>% arrange(desc(mm), current_locality_name) %>%
    mutate(value=sprintf("%.1f",mm), whole=sub("\\..*","",value), frac=sub(".*\\.",".",value))
  rows <- nrow(x)
  h <- 1.10 + rows * 0.235
  png(file.path(out_dir,paste0("bangalore-sep-",day,"-2026-rainfall-totals.png")), width=1800, height=round(h*150), res=150, bg="white")
  par(mar=c(.2,.2,.2,.2), xaxs="i", yaxs="i")
  plot.new(); plot.window(xlim=c(0,14), ylim=c(0,rows+3), asp=NA)
  text(.2, rows+2.3, paste0("Station rainfall totals - ", label), adj=c(0,0.5), font=2, cex=1.35, col="#303030")
  text(.2, rows+1.55, "24-hour WeatherUnion station totals (mm), highest first", adj=c(0,0.5), cex=.92, col="#666666")
  text(.2, rows+.75, "Station", adj=c(0,0.5), font=2, cex=.88, col="#303030")
  text(11.25, rows+.75, "Rain (mm)", adj=c(1,0.5), font=2, cex=.88, col="#303030")
  abline(h=rows+.4, col="#aaaaaa", lwd=1)
  ys <- rows:1
  text(.2, ys, x$current_locality_name, adj=c(0,0.5), cex=.82, col="#3c3c3c")
  # Keep the decimal point on one vertical rule for every row.
  text(10.65, ys, x$whole, adj=c(1,0.5), family="mono", cex=.82, col="#3c3c3c")
  text(10.72, ys, x$frac, adj=c(0,0.5), family="mono", cex=.82, col="#3c3c3c")
  text(13.8, 0.45, "Source: WeatherUnion hourly archive snapshot.", adj=c(1,0.5), cex=.68, col="#777777")
  dev.off()
}
make_table("14", "14 September 2026")
make_table("15", "15 September 2026")
