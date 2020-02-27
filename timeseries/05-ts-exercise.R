# Repeat the analysis above for the `ChickWeight` dataset. Look for any interesting 
# patterns in individual chicks.

library(dplyr)
library(lubridate)
library(tsibble)
library(ggplot2)
library(brolgar)

# 1. Load the `ChickWeight` data
data("ChickWeight")

# 2. Turn this into a tsibble. What's the key, what's the index? Is it regular?
chick_weight <- as_tsibble(x = ???,
                           key = ???,      # individual identifier
                           index = ???,     # number of days since hatching
                           regular = ???)

# Sample a handful of time series
???
  
# Plot 4 chicks per facet, in 5 facets
???