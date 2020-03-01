library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(gapminder)
library(gganimate)

df <- data.frame(f = 1:100,
                 t <- seq(0,10*pi, length.out=100), 
                 x = (1*t + 1) * cos(t),
                 y = (1*t + 1) *sin(t),
                 phase = rep(c("a", "b", "c", "d"), each = 25))

# static plot
p <- ggplot(df, aes(x, y)) + geom_point(aes(frame = f))
p

# basic animated plot
p1 <- ggplot(df, aes(x, y)) +
  geom_point(color = 'red', size = 3) +
  transition_time(f) 
animate(p1)

# during testing you can speed up rendering by changing frames per second and 
# duration settings
animate(p1, fps = 8, duration = 4)

### shadow_*() 
# defines how data from other points in time should be presented

# shadow_mark: shows the raw data behind the current frame as background marks
ps1 <- p1 +
  shadow_mark(colour = 'black', size = 0.5)
animate(ps1)

# shadow_wake: like shadow_mark, but plots past points differently depending on 
# how old they are 
ps2 <- p1
  shadow_wake(colour = 'black', wake_length = 0.25, size = 0.5)
animate(ps2)
# control the amount of wake with the `wake_length` option
# notice the shadow "wraps" so at t=1 you see a "wake" containing the LAST few
# observations. Figure out how to turn this off!

# shadow_trail: keeps every nth frame to create a "breadcrumb" trail. 
ps3 <- p1 +
  shadow_trail(distance = 0.2)
animate(ps3)

# try it out on some real data

# in the time series lecture we looked at weekly patterns of purchases and 
# made a plot something like this

load("timeseries/data/purchase_data.rdata")

store_counts <- purchase_data %>% 
  count(month, day) 

store_counts %>% ggplot(aes(day, n)) +
  geom_line(aes(group = month))

# animate this plot, using shadow_xxx and view_xxx to control how it looks

# base ggplot
sp1 <- store_counts %>% ggplot(aes(day, n)) +
  geom_line(aes(colour = month, group = month), size = 1.5) +
  scale_color_distiller(palette = "YlOrRd") 
sp1

# the next animations render much quicker if we remove "group = month", which 
# is needed for the static ggplot but not for the animations, because we use 
# month as the transition variable
sp1 <- store_counts %>% ggplot(aes(day, n)) +
  geom_line(aes(colour = month), size = 1.5) +
  scale_color_distiller(palette = "YlOrRd") 

sp2 <- sp1 + 
  transition_time(month) 
animate(sp2, fps = 8, duration = 4)

# add a shadow
sp3 <- sp2 +
  shadow_mark(colour = 'black', size = 0.75)
animate(sp3, fps = 8, duration = 4)

# plot window follows the current line around
sp4 <- sp3 + view_follow()
animate(sp4, fps = 8, duration = 4)
