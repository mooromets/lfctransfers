library(dplyr)
library(lubridate)
library(ggplot2)

data <- read.csv("./bought-by-lfc.csv")

data <- data %>%
  mutate(Date = dmy(Date))

dates <- data %>%
  select(Date, Player, Fee) %>%
  filter(between(month(Date), 5, 10 )) %>% #summer transfer window
  mutate (Fee = ifelse(gsub("[^[:digit:]]", "", Fee) == "", 0, as.integer(gsub("[^[:digit:]]", "", Fee)) / 10^6),
          Year = year(Date), 
          Date = as.Date(format(Date, "2017/%m/%d"), "%Y/%m/%d")) #make all month-day pairs comparable 

qplot(dates$Date)

# percent of transfers till the end
sum(dates$Date > today())/length(dates$Date) 

# number of transfers per season till the end
sum(dates$Date > today())/(2017 - 2004) 

# top 3 transfers
top <- dates %>% group_by(Year) %>% top_n(n=3, wt=Fee)
# percent of transfers till the end
sum(top$Date > today())/length(top$Date) 

# number of transfers per season till the end
sum(top$Date > today())/(2017 - 2004) 

qplot(top$Date)

#number of top transfers per season till the end
top %>%
  filter(Date > today()) %>%
  group_by(Year) %>%
  summarise(n = n(), sum = sum(Fee) )
