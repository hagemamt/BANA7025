library(readr)
url <- 
  'http://academic.udayton.edu/kissock/http/Weather/gsod95-current/OHCINCIN.txt'
weather <- read_table(url, col_names = c("Month", "Day", "Year", "Temp"))
weather

#2.
mean(weather$Temp)

#3.
min(weather$Temp)
max(weather$Temp)

#4. 
weather$Temp[weather$Temp == -99] <- NA
summary(weather$Temp)

#5.
boxplot(Temp ~ Month, data = weather)
