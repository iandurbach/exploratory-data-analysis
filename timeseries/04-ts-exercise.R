# Use the tools above to see if there are more purchases made on certain days of the week.

library(dplyr)
library(lubridate)
library(tsibble)
library(ggplot2)
library(feasts)

# 1. Load data
load("timeseries/data/purchase_data.rdata")

# 2. Use group_by and count() to get number of purchases (any brand) for each day in each 
# month
???
  
# 3. Turn this into a tsibble. What's the key, what's the index? Is it regular?
store_counts_ts <- as_tsibble(x = ???,
                          key = ???,
                          index = ???,
                          regular = ???)

# 4. Plot the number of purchases per day in each month (facet or colour by month)
store_counts_ts %>%
  ggplot(aes(x = ??,
             y = ??)) + geom_line(group = 1) +
  facet_wrap(~ ??)

# 5. Do a seasonal decomposition using STL
sdecomp <- ???

autoplot(sdecomp) + xlab("Day")


