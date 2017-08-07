library(dplyr)
library(lubridate)
library(ggplot2)

data <- read.csv("./bought-by-lfc.csv")

data <- data %>%
  mutate(Date = dmy(Date))

dates <- data %>%
  select(Date) %>%
  filter(between(month(Date), 5, 10 )) %>% #summer transfer window
  mutate (Date = as.Date(format(Date, "2017/%m/%d"), "%Y/%m/%d")) #make all month-day pairs comparable

qplot(dates$Date)

# percent of transfers
sum(dates$Date > today())/length(dates$Date) 

# number of transfers per season
sum(dates$Date > today())/(2017 - 2004) 
