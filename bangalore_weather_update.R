require(tidytable)
require(tidyverse)
require(patchwork)
require(yaml)
require(rvest)
require(lubridate)

primKey <- Sys.getenv("OIKOLAB_PRIMARY")
secKey <- Sys.getenv("OIKOLAB_SECONDARY")
load('~/Documents/work/data work/bangaloreRainfall.RData')
load('~/Documents/work/data work/bangaloreTemperature.RData')

blrTemp %>%
  filter(!is.na(Temp)) ->
  blrTemp

blrRain %>%
  filter(!is.na(Rain)) -> 
  blrRain

startDate <-  str_sub(max(blrTemp$DT), 1, 19) %>% str_replace_all(" ", "T")
endDate <- paste0(Sys.Date(), 'T00:00:00')

bloreLat <- 12.9716
bloreLon <- 77.5946

url <- paste0("https://api.oikolab.com/weather?start=", startDate, "&end=",endDate,"&param=temperature&freq=H&lat=",bloreLat,"&lon=",bloreLon,"&api-key=",primKey)

tmp <- tempfile()
download.file(url, tmp)

b1 <- jsonlite::fromJSON(tmp)
b2 <- jsonlite::fromJSON(b1$data)
b2$data %>%
  as_tibble() %>%
  set_names(c("Latlong", "Source", "Something", "SomethingElse", "Temp")) %>%
  mutate(
    Index = b2$index,
    DT= as.POSIXct(Index, origin='1970-01-01')
  ) ->
  blrTempNew

url <- paste0("https://api.oikolab.com/weather?start=", startDate, "&end=",endDate,"&param=total_precipitation&freq=H&lat=",bloreLat,"&lon=",bloreLon,"&api-key=",primKey)
tmp <- tempfile()
download.file(url, tmp)
r1 <- jsonlite::fromJSON(tmp)
r2 <- jsonlite::fromJSON(r1$data)
r2$data %>%
  as_tibble() %>%
  set_names(c("Latlong", "Source", "Something", "SomethingElse", "Rain")) %>%
  mutate(
    Index = r2$index,
    DT=as.POSIXct(Index, origin='1970-01-01')
  ) ->
  blrRainNew


blrRain %>%
  bind_rows(blrRainNew) ->
  blrRain
blrTemp %>%
  bind_rows(blrTempNew) ->
  blrTemp

save(blrTemp, file='~/Documents/work/data work/bangaloreTemperature.RData')
save(blrRain, file='~/Documents/work/data work/bangaloreRainfall.RData')

curr_year <- max(year(blrTemp$DT))


blrTemp %>%
  mutate(
    DT = as.Date(DT), 
    Month = month(DT),
    Day = day(DT),
    Temp = as.numeric(Temp)
  ) %>%
  mutate(
    High = max(Temp),
    Low = min(Temp),
    .by = DT
  ) %>%
  summarise(
    RecordHigh = max(Temp),
    RecordLow = min(Temp),
    NormalHigh = median(High),
    NormalLow = median(Low), 
    High2022 = max(Temp[year(DT) == curr_year]),
    Low2022 = min(Temp[year(DT) == curr_year]),
    .by = c(Month,Day)
  ) %>%
  mutate(
    Date = make_date(curr_year, Month, Day),
    High2022 = ifelse(is.infinite(High2022),NormalHigh, High2022),
    Low2022 = ifelse(is.infinite(Low2022),NormalHigh, Low2022), 
    Special = case_when(
      High2022 >= RecordHigh ~ paste("Hottest", format(Date, "%b-%d"), "since 1980"),
      Low2022 <= RecordLow ~ paste("Coldest", format(Date, "%b-%d"), "since 1980"),
      .default = ""
    )
  ) %>%
  ggplot() + 
  geom_segment(aes(x = Date, xend = Date, y = RecordLow, yend = RecordHigh), linewidth = 1, col = "#d4cbaa")+ 
  geom_segment(aes(x = Date, xend = Date, y = NormalLow, yend = NormalHigh), linewidth = 1, col = '#888888') + 
  geom_segment(aes(x = Date, xend = Date, y = Low2022, yend = High2022), linewidth = 1, col = "#490000",alpha = 0.9) + 
  ggrepel::geom_text_repel(aes(x = Date, y = NormalHigh, label = str_wrap(Special, 10)), size = 2.5, direction = 'y')  +
  scale_x_date("", lim = c(floor_date(Sys.Date(), '1 year'), ceiling_date(Sys.Date(), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), date_labels = '%B', position = 'top', expand = expansion(mult = 0)) + 
  scale_y_continuous("", breaks = seq(10, 42, 4) )  +
  ggthemes::theme_tufte()  + 
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold'), axis.line.y = element_line(colour = 'black', linewidth = 0.2),  panel.background = element_rect(fill = rgb(0.85, 0.85, 0.75), linewidth = 0), plot.background = element_rect(fill = rgb(0.85, 0.85, 0.75)) ) +
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 38, label = "Temperature", hjust = 0, fontface = 'bold', label.size = NA, size = 3) + 
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 37.5, label = str_wrap("Brown bars represent range between the daily high and low", 40), hjust = 0, vjust = 1, label.size = NA, size = 2.5) + 
  annotate("text", x = as.Date(paste0(curr_year, '-09-05')), y = 14, label = str_wrap("Dark grey bars show normal range; Beige show record range", 40), label.size = NA, size = 2.5) ->
  tempPlot


blrRain %>%
  mutate(
    Rain = as.numeric(Rain),
    DT = as.Date(DT)
  ) %>%
  summarise(
    Rain = sum(Rain),
    .by = DT
  ) %>%
  mutate(
    Month = month(DT),
    Day = day(DT), 
    MonthYear = floor_date(DT, '1 month')
  ) %>%
  mutate(
    MonthlyRain = sum(Rain),
    .by = MonthYear
  ) %>%
  mutate(
    DailyAvg = mean(Rain),
    DailyMax = max(Rain),
    .by = c(Month, Day)
  ) %>%
  mutate(
    MonthlyAvg = mean(MonthlyRain),
    MonthlyMax = max(MonthlyRain), 
    .by = Month
  ) %>%
  filter(year(DT) == curr_year) %>%
  arrange(DT) %>%
  mutate(
    CumulRain = cumsum(Rain), 
    CumulAvg = cumsum(DailyAvg),
    .by = Month
  ) %>%
  mutate(
    normalLabel = case_when(
      Day == 1 & Month == 10 ~ paste("Normal", round(MonthlyAvg, 0), sep = '\n'), 
      Day == 1 ~ as.character(round(MonthlyAvg, 0)),
      .default = ""
    ),
    actualLabel = case_when(
      Day == max(Day) & Month == 10 ~ paste("Actual", round(CumulRain, 0), sep = '\n'), 
      Day == max(Day) ~ as.character(round(CumulRain, 0)),
      .default = ""
    ),
    MonthLabel = ifelse(CumulRain == MonthlyMax & Day == 15, paste("Wettest", month.name[Month], "since 1981"), ""),
    .by = Month
  ) %>%
  mutate(
    DayLabel = ifelse(Rain == DailyMax, paste0("Record ", month.name[Month], "-", Day, "; ", round(Rain, 0), " mm"), ""),
    .by = c(Month, Day)
  ) %>%
  mutate(
    Label = case_when(
      DayLabel != "" ~ DayLabel, 
      MonthLabel != "" ~ MonthLabel, 
      .default = ""
    )
  ) %>%
  ggplot(aes(x = DT)) + 
  geom_segment(aes(x = DT, xend = DT, y = CumulRain - Rain, yend = CumulRain), linewidth = 1, col = '#490000') + 
  #geom_area(aes(y = CumulRain, group = Month), lwd = 1, col = '#005566', fill = '#d4cbaa', alpha = 0.4) +
  geom_step(aes(y = MonthlyAvg, group = Month), lwd = 1, col = 'darkgreen') + 
  geom_text(aes(y = MonthlyAvg, label = normalLabel), vjust = -0.05, hjust = 0, size = 2.5, fontface = 'bold') + 
  geom_text(aes(y = CumulRain, label = actualLabel), vjust = -0.05, hjust = 1, size = 2.5, fontface = 'bold') + 
  ggrepel::geom_text_repel(aes(y = CumulRain, label = str_wrap(Label, 10)), size = 2,  fontface = 'bold') +
  scale_x_date("", lim = c(floor_date(Sys.Date(), '1 year'), ceiling_date(Sys.Date(), '1 year')),  breaks = seq(as.Date(paste0(curr_year, '-01-15')), as.Date(paste0(curr_year, '-12-15')), by = '1 month'), date_labels = '%B', expand = expansion(mult = 0)) + 
  ggthemes::theme_tufte() + 
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.grid.minor.x = element_line(colour = 'black', linewidth = 0.1), axis.text.x = element_text(face = 'bold'), axis.line.y = element_line(colour = 'black', linewidth = 0.2), panel.background = element_rect(fill = rgb(0.85, 0.85, 0.75), linewidth = 0), plot.background = element_rect(fill = rgb(0.85, 0.85, 0.75)) ) + 
  scale_y_continuous("", breaks = seq(0,500, 50)) + 
  annotate("text", x = as.Date(paste0(curr_year, '-01-05')), y = 260, label = "Precipitation", hjust = 0, fontface = 'bold', size = 3) + 
  annotate("text", x = as.Date(paste0(curr_year, '-02-05')), y = 260, label = str_wrap("Cumulative monthly precipitation in mm compared to normal monthly precipitation", 1000), hjust = 0,  size = 2.5)  ->
  rainPlot

tempPlot + rainPlot +
  plot_layout(ncol = 1, heights = c(70, 30)) + 
  plot_annotation(title = paste("Bangalore's Weather in", curr_year), caption = "Data source: Oikokab") &
  theme(plot.title = element_text(face = 'bold', hjust = 0.05),  panel.background = element_rect(fill = "#eae4db"), plot.background = element_rect(fill = "#eae4db", linewidth = 0)) 



