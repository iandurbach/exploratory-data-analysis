library(dplyr)
library(ggplot2)
library(plotly)

# load the diamonds dataset from the ggplot2 package
data(diamonds)
diamonds

# will try guess right plot if no extra info
plot_ly(diamonds, x = ~cut)

# or we can tell it what to plot
plot_ly(diamonds, x = ~cut) %>% add_histogram()

# basic style options
plot_ly(diamonds, x = ~cut, color = "red")
plot_ly(diamonds, x = ~cut, color = I("red"))
plot_ly(diamonds, x = ~cut, color = I("red"), stroke = I("black"))
plot_ly(diamonds, x = ~cut, color = I("red"), stroke = I("black"), span = I(5))
plot_ly(diamonds, x = ~cut, color = I("red"), stroke = I("black"), span = I(5), alpha = I(0.2))

# use the pipe to make the plot in the exercise
diamonds %>%
  ???
