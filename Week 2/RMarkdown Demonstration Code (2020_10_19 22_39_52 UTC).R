# code chunk 1
library(nasaweather)
library(tidyverse)

# code chunk 2
year <- 1995

# code chunk 4
means <- atmos %>%
  filter(year == year) %>%
  group_by(long, lat) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
            pressure = mean(pressure, na.rm = TRUE),
            ozone = mean(ozone, na.rm = TRUE),
            cloudlow = mean(cloudlow, na.rm = TRUE),
            cloudmid = mean(cloudmid, na.rm = TRUE),
            cloudhigh = mean(cloudhigh, na.rm = TRUE)) %>%
  ungroup()

# code chunk 6
ggplot(data = means, aes(x = temp, y = ozone)) +
  geom_point()

# code chunk 7
means$locale <- "north america"
means$locale[means$lat < 10] <- "south pacific"
means$locale[means$long > -80 & means$lat < 10] <- "south america"
means$locale[means$long > -80 & means$lat > 10] <- "north atlantic"

# code chunk 8
lm(ozone ~ temp + locale + temp:locale, data = means)

# code chunk 9
ggplot(means, aes(temp, ozone, color = locale)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ locale)

# code chunk 10
mod <- lm(ozone ~ temp, data = means)
mod2 <- lm(ozone ~ temp + locale, data = means)
mod3 <- lm(ozone ~ temp + locale + temp:locale, data = means)

anova(mod, mod2, mod3)


