library(fpp3)
library(shiny)
library(dplyr)


boston_marathon$Time <- as.numeric(boston_marathon$Time)

boston_marathon$Time[is.na(boston_marathon$Time)] <- 0

bm_countries <- boston_marathon %>% 
  select(Event, Year, Champion, Country)


boston_marathon$Time <- ifelse(boston_marathon$Time == 0, NA, boston_marathon$Time) 

interpolated <- boston_marathon %>% 
  model(ARIMA(Time)) %>% 
  interpolate(boston_marathon) %>% 
  left_join(bm_countries, by = c("Event", "Year")) %>% 
  select(Event, Year, Champion, Country, Time)


