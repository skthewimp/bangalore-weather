library(tidyverse)
library(lubridate)

load("data/bangaloreTemperature.RData")
load("data/bangaloreRainfall.RData")

# Join hourly temp + rain
blrRain <- blrRain %>% mutate(Rain = as.numeric(Rain)) %>%
  group_by(Index) %>% slice(1) %>% ungroup()
blrTemp <- blrTemp %>% mutate(Temp = as.numeric(Temp)) %>%
  group_by(Index) %>% slice(1) %>% ungroup()

df <- blrTemp %>%
  select(Index, DT, Temp) %>%
  inner_join(blrRain %>% select(Index, Rain), by = "Index") %>%
  mutate(
    DT = as_datetime(DT),
    date = as_date(DT),
    year = year(DT),
    month = month(DT),
    day = day(DT),
    hour = hour(DT)
  ) %>%
  arrange(DT)

# Restrict to Apr-May
am <- df %>% filter(month %in% c(4, 5))

cat("Apr-May hours:", nrow(am), "\n")
cat("Years:", min(am$year), "-", max(am$year), "\n")

# Define rain event: hour with rain >= 1mm (filter drizzle).
# To avoid double-counting same storm, mark "event start" as first rain hour
# preceded by >=3 dry hours.
am <- am %>%
  mutate(is_rain = Rain >= 1)

# Use full df ordering for lag calc
df2 <- df %>%
  mutate(is_rain = Rain >= 1) %>%
  arrange(DT) %>%
  mutate(
    prev1 = lag(is_rain, 1),
    prev2 = lag(is_rain, 2),
    prev3 = lag(is_rain, 3),
    event_start = is_rain & !coalesce(prev1, FALSE) &
                  !coalesce(prev2, FALSE) & !coalesce(prev3, FALSE)
  )

# For each event in Apr-May, compute temp before (avg t-3..t-1) and min after (t..t+3)
events <- df2 %>%
  filter(event_start, month(DT) %in% c(4, 5)) %>%
  select(DT, year, month_e = month, day_e = day, hour_e = hour, Rain_e = Rain)

cat("Events (Apr-May, >=1mm, dry-3h-prior):", nrow(events), "\n")

# Build offsets table - join to grab surrounding hours
get_window <- function(events_df, df_all) {
  evs <- events_df %>% mutate(eid = row_number())
  # Cross-join offsets -3..+5
  offsets <- tibble(off = -3:5)
  ev_x <- evs %>%
    crossing(offsets) %>%
    mutate(DT_t = DT + hours(off)) %>%
    left_join(df_all %>% select(DT, Temp, Rain), by = c("DT_t" = "DT"))
  ev_x
}

ev_x <- get_window(events, df2)

# Summarise: pre = mean Temp at off in -3,-2,-1; post_min = min Temp at off in 0..5
summary_ev <- ev_x %>%
  group_by(eid, DT, year, month_e, day_e, hour_e) %>%
  summarise(
    pre_temp = mean(Temp[off %in% c(-3,-2,-1)], na.rm = TRUE),
    post_min = min(Temp[off >= 0 & off <= 5], na.rm = TRUE),
    rain_total = sum(Rain[off >= 0 & off <= 5], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(drop = pre_temp - post_min)

# Filter to afternoon/evening events (showers commonly 14:00-22:00 IST)
afternoon <- summary_ev %>% filter(hour_e >= 14, hour_e <= 22)
cat("\nAfternoon/evening events (14h-22h):", nrow(afternoon), "\n")

cat("\n=== Overall drop after summer shower (afternoon events) ===\n")
print(afternoon %>% summarise(
  n = n(),
  mean_pre = mean(pre_temp, na.rm=T),
  mean_postmin = mean(post_min, na.rm=T),
  mean_drop = mean(drop, na.rm=T),
  median_drop = median(drop, na.rm=T),
  q90_drop = quantile(drop, 0.9, na.rm=T)
))

# Baseline: same hours on dry days - what's the natural cool-off from peak?
# For each Apr-May day, find max afternoon temp (14-17h) and min by 22h
baseline <- df2 %>%
  filter(month %in% c(4,5)) %>%
  group_by(date = as_date(DT), year) %>%
  summarise(
    rain_pm = sum(Rain[hour(DT) >= 14 & hour(DT) <= 22], na.rm=T),
    t_peak = max(Temp[hour(DT) >= 13 & hour(DT) <= 16], na.rm=T),
    t_late = min(Temp[hour(DT) >= 17 & hour(DT) <= 22], na.rm=T),
    .groups = "drop"
  ) %>%
  mutate(
    afternoon_drop = t_peak - t_late,
    rainy_pm = rain_pm >= 1
  ) %>%
  filter(is.finite(t_peak), is.finite(t_late))

cat("\n=== Natural peak->evening drop, rainy vs dry afternoons ===\n")
print(baseline %>% group_by(rainy_pm) %>% summarise(
  n = n(),
  mean_peak = mean(t_peak),
  mean_late = mean(t_late),
  mean_drop = mean(afternoon_drop),
  median_drop = median(afternoon_drop)
))

# Variation by half-month
baseline <- baseline %>%
  mutate(
    mo = month(date),
    dy = day(date),
    bin = case_when(
      mo == 4 & dy <= 15 ~ "Apr 1-15",
      mo == 4 & dy >  15 ~ "Apr 16-30",
      mo == 5 & dy <= 15 ~ "May 1-15",
      mo == 5 & dy >  15 ~ "May 16-31"
    )
  )

cat("\n=== By half-month: dry vs rainy afternoon ===\n")
hm <- baseline %>% group_by(bin, rainy_pm) %>% summarise(
  n = n(),
  mean_peak = mean(t_peak),
  mean_late = mean(t_late),
  mean_drop = mean(afternoon_drop),
  .groups = "drop"
)
print(hm)

# Net cooling effect: rainy minus dry, by half-month
cat("\n=== Extra cooling from rain vs dry day, by half-month ===\n")
net <- baseline %>%
  group_by(bin) %>%
  summarise(
    n_dry = sum(!rainy_pm),
    n_rainy = sum(rainy_pm),
    drop_dry = mean(afternoon_drop[!rainy_pm]),
    drop_rainy = mean(afternoon_drop[rainy_pm]),
    extra_cooling = drop_rainy - drop_dry,
    peak_dry = mean(t_peak[!rainy_pm]),
    peak_rainy = mean(t_peak[rainy_pm]),
    late_dry = mean(t_late[!rainy_pm]),
    late_rainy = mean(t_late[rainy_pm])
  )
print(net)

# How often does rain occur in Apr-May afternoons?
cat("\n=== Rain-afternoon frequency by half-month ===\n")
freq <- baseline %>% group_by(bin) %>% summarise(
  days = n(),
  rainy_days = sum(rainy_pm),
  pct = mean(rainy_pm) * 100
)
print(freq)

saveRDS(list(events=summary_ev, baseline=baseline, hm=hm, net=net, freq=freq),
        "scratch_summer_showers_results.rds")
