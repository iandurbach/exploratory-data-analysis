library(dplyr)
library(ggplot2)
library(plotly)

# load the diamonds data
data(diamonds)

# make the original ggplot
p <- diamonds %>% sample_n(1000) %>%
  ggplot(aes(x = x, y = price, colour = clarity)) +
  geom_point() + geom_smooth(se = FALSE) +
  labs(x = "Diamond width (x-dim)", y = "Price") +
  scale_color_brewer(palette = "Set2")

# make a similar plot, but with a geom_density2d geom
p <- diamonds %>% sample_n(1000) %>%
  ggplot(aes(x = x, y = price, colour = clarity)) +
  geom_density2d() +
  labs(x = "Diamond width (x-dim)", y = "Price") +
  scale_color_brewer(palette = "Set2")

# view it
p

# can't see much because of overlap, two options are 

# facet by clarity rather than (or in addition to) colour
p2 <- diamonds %>% sample_n(1000) %>%
  ggplot(aes(x = x, y = price, colour = clarity)) +
  geom_density2d() +
  labs(x = "Diamond width (x-dim)", y = "Price") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~clarity)
p2

# or ggplotly and then view interactively
ggplotly(p)
