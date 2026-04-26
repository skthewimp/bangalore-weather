library(tidytable)
library(dplyr)
library(lubridate)
library(ggplot2)
load('/Users/Karthik/Documents/work/data work/bangalore/weather/data/bangaloreWind.RData')

blrWind <- blrWind %>%
  mutate(WindDir = as.numeric(WindDir)) %>%
  filter(!is.na(WindDir))

blrWind %>%
  select(-Source, -Something, -SomethingElse) %>%
  mutate(Week = week(DT)) %>%
  summarise(
    Mean_Dir = mean(WindDir),
    Median_Dir = median(WindDir),
    # East (45 to 135) or West (225 to 315)
    Pct_East_West = mean(WindDir > 45 & WindDir < 135 | WindDir > 225 & WindDir < 315),
    # North (315 to 45) or South (135 to 225)
    Pct_North_South = mean(WindDir <= 45 | WindDir >= 315 | (WindDir >= 135 & WindDir <= 225)),
    .by = Week
  ) %>%
  ggplot(aes(x = Week, y = Pct_East_West)) + geom_point() + geom_line() 



# Let's also check the 8 compass directions
dir_table <- blrWind %>%
  mutate(
    Compass = case_when(
      WindDir >= 337.5 | WindDir < 22.5 ~ "N",
      WindDir >= 22.5 & WindDir < 67.5 ~ "NE",
      WindDir >= 67.5 & WindDir < 112.5 ~ "E",
      WindDir >= 112.5 & WindDir < 157.5 ~ "SE",
      WindDir >= 157.5 & WindDir < 202.5 ~ "S",
      WindDir >= 202.5 & WindDir < 247.5 ~ "SW",
      WindDir >= 247.5 & WindDir < 292.5 ~ "W",
      WindDir >= 292.5 & WindDir < 337.5 ~ "NW"
    )
  ) %>%
  count(Compass) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  arrange(desc(Pct))

print(dir_table)
