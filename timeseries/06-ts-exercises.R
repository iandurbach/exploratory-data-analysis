# Create interactive plot of weekly total purchases for each brand A, B, C, D

library(dplyr)
library(tidyr)
library(ggplot2)
library(dygraphs)

# 1. Load data
load("timeseries/data/purchase_data.rdata")

# 2. Use group_by and count() to get number of purchases for each brand in each week
??
  
# 3. Use pivot_wider to spread long to wide for dygraph format (one column per brand)
??
  
# 4. Plot using dygraph
??
  