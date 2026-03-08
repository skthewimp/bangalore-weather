# Visualising Bangalore's Weather, One Year at a Time

A couple of years ago, I found myself in one of those arguments about whether Bangalore's weather has actually changed, or whether we're all just getting older and more complainy. Someone said "April used to be nice", someone else said "no, April was always horrible, it's March that's gotten worse", and I realised nobody had the data to settle it.

So I built a chart.

The idea was simple - take hourly temperature and rainfall data for Bangalore going back to 1981 (from the Oikolab API), and produce a single visualisation that shows the current year's weather against historical norms and records. The design is inspired by the NYT weather charts - each day gets a vertical bar showing the temperature range, layered on top of the historical normal range (grey) and all-time record range (beige). You can immediately see when a day was unusually hot, unusually cold, or record-breaking.

The rainfall panel does something slightly different. Instead of daily bars, it shows cumulative monthly precipitation building up through each month, compared to the normal monthly total (the green step line). This makes it easy to spot months that were wetter or drier than usual - if the brown bars reach the green line early, it was a wet month.

## The code

There are two R scripts. `bangalore_weather_update.R` fetches the latest data from Oikolab and produces a chart for the current year - I run this periodically to keep things up to date. `bangalore_weather_historical.R` can generate the chart for any year going back to 1981.

Both scripts resolve their own directory at runtime, so you can source them from wherever - RStudio, Rscript, a cron job, it doesn't matter. This is a thing I should have done from the start instead of hardcoding paths (I learnt this the hard way after a folder reorganisation broke everything).

The historical script also has a small LLM component. After R computes about 30 weather statistics for the year - hot day counts, cold streaks, rainfall totals, record-breaking days - it ranks them by deviation from historical average and sends the top 5 to a local Gemma 3 model (via Ollama) for rephrasing into 4 subtitle bullets. The model does no analysis; it just turns "42 days above 35°C vs the usual 18" into something readable. I was particular about keeping the LLM's role minimal - the data analysis is all in R, and the chart works fine even if Ollama isn't running.

The update script does something similar but scoped to the last two weeks - it computes recent temperature and rainfall stats compared to historical norms for the same calendar dates, and generates a 3-bullet summary. Useful for a quick "how has the weather been lately" at a glance.

## What shows up in the data

The fun part of having 40+ years of data is that you can actually see patterns. 2025, for instance, had 37 days that crossed 34°C - double the usual 17. There was a 30-day hot streak from mid-April to end of April. But also a 30-day dry spell from late January through early March, which is not that unusual for Bangalore's dry season but felt longer than normal.

The record-breaking days are the ones that catch my eye. When a random Tuesday in March is hotter than every single March Tuesday going back to 1981, that's worth noting. The chart labels these automatically.

The charts are all in R using ggplot2, with `ggthemes::theme_tufte()` as the base. Muted earth tones, minimal gridlines - the data should do the talking. `patchwork` handles stacking the temperature and rainfall panels.

If you want to generate charts for your own years or fork this for another city, the code is on [GitHub](https://github.com/skthewimp/bangalore-weather). You'll need an Oikolab API key for the data, and optionally Ollama for the commentary bits.
