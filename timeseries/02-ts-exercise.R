library(dplyr)
library(lubridate)
library(nycflights13)
library(ggplot2)

# 1. Open the `flights` dataset

data(flights)

# 2. Use make_datetime() to combine the year, month, day, hour and minute variables
# into a single variable containing the departure time

flights_dt <- flights %>% 
  dplyr::select(year, month, day, hour, minute, dep_delay) %>% 
  mutate(departure = make_datetime(?????))

# 3. Remove the variables you used to make up the departure variable
flights_dt <- flights_dt %>% 
  dplyr::select(?????)

# 4. Make a line plot of the average departure delay within each hour of the day
?????

