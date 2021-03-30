library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(patchwork)
library(scales)

# load datasets we'll use
data(sleep)
data(diamonds)
data(co2)
# tidy co2 
co2 <- data.frame(Year = rep(1959:1997, each = 12),
                  Month = rep(format(ISOdate(2004,1:12,1),"%B"), 39),
                  co2 = as.numeric(co2))

matrics <- data.frame(
  year = 2014:2018,
  EC = c(65.4,56.8,59.3,65,70.6),
  FS = c(82.8,81.6,88.2,86.1,87.5),
  Gau = c(84.7,84.2,85.1,85.1,87.9),
  KZN = c(69.7,60.7,66.4,72.9,76.2),
  Lim = c(72.9,65.9,62.5,65.6,69.4),
  Mpu = c(79,78.6,77.1,74.8,79),
  NW = c(84.6,81.5,82.5,79.4,81.1),
  NC = c(76.4,69.4,78.7,75.6,73.3),
  WC = c(82.2,87.7,85.9,82.8,81.5)
)

matrics_long <- pivot_longer(matrics, cols = -1, names_to = "province", values_to = "pass_rate")

# geometries
# geometries denoted by geom_xxx are the building blocks of ggplots

# common 1D geoms: geom_density, geom_histogram, geom_freqpoly, geom_bar
# common 2D geoms: geom_point, geom_line, geom_bar(stat = "identity")

### 1D geoms only need you to specify an "x" in the aesthetics

## geom_density
diamonds %>% 
  ggplot(aes(x = price)) + 
  geom_density()

## geom_histogram
diamonds %>% 
  ggplot(aes(x = price)) + 
  geom_histogram()

## geom_freqpoly
diamonds %>% 
  ggplot(aes(x = price)) + 
  geom_freqpoly()

## additional parameters control smoothing (see ?geom_xxx)

# EXERCISE: adapt each of the plots above so that the densities/histograms are 
# either "smoother" or "rougher". Choose an appropriate amount of smoothing.

## geom_bar

# default behaviour for geom_bar is to count stuff, so 1D
diamonds %>% ggplot(aes(x = cut)) + 
  geom_bar()

### 2D geoms need x and y in aes

## geom_point

diamonds %>% ggplot(aes(x = carat, y = price)) + 
  geom_point()

# often nice to add a smooth line of best fit with geom_smooth, which
# uses generalised additive models (gams) by default, covered in later
# courses
diamonds %>% ggplot(aes(x = carat, y = price)) + 
  geom_point() + 
  geom_smooth()

# EXERCISE: adapt the code you just used to fit a straight line rather than a smooth

## geom_line

co2 %>% filter(Month == "January") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_line()

# EXERCISE: show lines for all 12 months on the plot (a) by using fill or colour, 
# (b) without using fill or colour

# EXERCISE: flip Month and Year around so that each line represents a Year, and the 
# Month is on the x-axis

## geom_area

# just fills in area under a geom_line
co2 %>% filter(Month == "January") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_area()

## geom_bar

# with (stat = "identity") plots x and y directly, no counts
diamonds %>% group_by(cut) %>% summarize(meanprice = mean(price)) %>%
  ggplot(aes(x = cut, y = meanprice)) + 
  geom_bar(stat = "identity")

### discretising continuous variables

## ggplot provides 3 helper functions:
# cut_interval: divides data into fixed number of bins
# cut_width: divides data into bins of fixed width
# cut_number: divides data into n bins each containing (approximately) the same number of points

diamonds %>%
  ggplot(aes(x = cut_interval(price, n = 5))) + 
  geom_bar()

diamonds %>%
  ggplot(aes(x = cut_width(price, width = 5000))) + 
  geom_bar()

diamonds %>%
  ggplot(aes(x = cut_number(price, n = 5))) + 
  geom_bar()

# these are especially useful when grouping/faceting on continuous variables

# EXERCISE: Display the distribution of price conditional on cut and carat. 
# Try facetting by cut and grouping by carat. Try facetting by carat and grouping by cut. 
# Which do you prefer?

# EXERCISE: Compare the relationship between price and carat for each colour.
# What makes it hard to compare the groups? Is grouping better or facetting? 
# If you use facetting, what annotation might you add to make it easier to see 
# the differences between panels?

### Labels (labs)

## sets text for axis labels and title
## NOT the appearance of the text, that's a "theme" (next)

co2 %>% filter(Month == "January") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_line() +
  labs(x = "Year", y = "C02 level (PPM)", 
       title = "Increasing atmospheric concentrations of CO2", 
       subtitle = "Values for February, March and April of 1964 obtained by interpolation")

# nice to have CO2 with a subscript on the "2"
co2 %>% filter(Month == "January") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_line() +
  labs(x = "Year", y = expression(paste(CO[2]," level")), 
       title = expression(paste("Increasing atmospheric concentrations of ", CO[2])), 
       subtitle = "Values for February, March and April of 1964 obtained by interpolation")

### Themes

## fine-scale control and polishing

# loads of options
?theme

# make a plot
pp <- diamonds %>% sample_n(1000) %>%
  ggplot(aes(x = carat, y = price)) + 
  geom_point()
pp 

# change the background colour
pp + 
  theme(panel.background = element_rect(fill = "red"))

# that's too dark, add some transparency
pp + 
  theme(panel.background = element_rect(fill = alpha('red', 0.2)))

# change axis text 
pp + 
  theme(axis.text = element_text(colour = "red", size = rel(1.5)))

# change axis labels text 
pp + 
  theme(axis.title = element_text(colour = "red", size = rel(1.5)))

# can also add transparency to axis labels (not a good idea here) 
pp + 
  theme(axis.title = element_text(colour = alpha('red', 0.2), size = rel(1.5)))

# arrows on the axes
pp + 
  theme(axis.line = element_line(arrow = arrow()))

# even finer control... changes look of arrow heads
pp + 
  theme(axis.line = element_line(arrow = arrow(angle = 20, length = unit(0.1, "inches"))))

# grid lines
pp +
  theme(panel.grid.major = element_line(colour = "red"),
        panel.grid.minor = element_line(colour = "orange"))

pp +
  theme(panel.grid.major = element_line(colour = "red", size = 1),
        panel.grid.minor = element_line(colour = "orange", size = 1))

# spacing between gridlines isn't a theme setting, its in "scale_"
pp +
  theme(panel.grid.major = element_line(colour = "red"),
        panel.grid.minor = element_line(colour = "orange")) +
  scale_x_continuous(breaks = seq(from = 0, to = 3, by = 0.5),
                     minor_breaks = seq(from = 0, to = 3, by = 0.1))

# same for y axis
pp +
  theme(panel.grid.major = element_line(colour = "red"),
        panel.grid.minor = element_line(colour = "orange")) +
  scale_y_continuous(breaks = seq(from = 0, to = 20000, by = 10000),
                     minor_breaks = seq(from = 0, to = 20000, by = 500))

# can also set manual axis text at tickmarks
pp +
  scale_y_continuous(breaks = seq(from = 0, to = 20000, by = 5000),
                     labels = c("R0", "R5,000", "R10,000", "R15,000", "R20,000"))

# longer axis ticks
pp +
  scale_y_continuous(breaks = seq(from = 0, to = 20000, by = 5000),
                     labels = c("R0", "R5,000", "R10,000", "R15,000", "R20,000")) +
  theme(axis.ticks.length=unit(.25, "cm"))

# put it all together
pp + 
  labs(x = "Weight of diamond (carats)", y = "Price of diamond (USD)") +
  theme(panel.background = element_rect(fill = alpha('red', 0.2)),
        axis.text = element_text(colour = "red", size = rel(1.5)),
        axis.title = element_text(colour = "red", size = rel(1.5)))


# revisiting log scale example from lecture
data(msleep)

a <- ggplot(msleep, aes(x = bodywt, y = brainwt)) + 
  geom_point(na.rm = TRUE) + 
  labs(x = "Body weight (kg)", y = "Brain weight (kg)")
a 

# log scaling
a + scale_x_log10() + scale_y_log10() 

# nice tick marks with annotation_xxx
a + scale_x_log10() + scale_y_log10() + annotation_logticks() 

# advanced axis breaks and labelling options with scales package
a + scale_x_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  labs(x = "Body weight (kg)", y = "Brain weight (kg)")

# EXERCISE: choose any of the plots you've made and add axis labels, title, and 
# subtitle

# EXERCISE: Improve the plot below by adding appropriate axis labels, 
# rotating the x axis text so that it is vertical, and change
# the axis text so that it does not include numbers in scientific notation

diamonds %>%
  ggplot(aes(x = cut_interval(price, n = 5))) + 
  geom_bar()

# EXERCISE: Use geom_label to add a label to the plot below that identifies 
# the diamond with the largest value-per-carat. Bonus: make the labelling 
# "automatic" so that it still works if you resample another 10 diamonds 
# (make the label's location dynamic, rather than at a fixed (x,y) location)

diamonds %>% sample_n(10) %>%
  ggplot(aes(x = carat, y = price)) + 
  geom_point()

### Legends

# revisiting class example #1
s1 <- ggplot(msleep, 
       aes(x = bodywt, y = brainwt, colour = order)) +
  geom_point(na.rm = TRUE) + 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10",scales::math_format(10^.x))) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  xlab("Body weight (kg)") + ylab("Brain weight (kg)") 
s1 

s1 + theme(legend.position = "bottom") 
s1 + theme(legend.position = "bottom") + guides(colour = guide_legend(ncols = 8))

matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + theme_bw() + scale_colour_brewer(palette = "Set3")


# revisiting class example #2

s2 <- matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + theme_bw() + scale_colour_brewer(palette = "Set3") 

# inset legend 
s2 +
  # coordinates for legend.position are x- and y- offsets from the bottom-left of the plot, ranging from 0 - 1.
  theme(legend.position = c(0.82,0.12), legend.title = element_blank(), 
        legend.text=element_text(size=8)) +
  guides(colour = guide_legend(ncol = 3))

# with geom_label

# make dataframe with x and y coords of labels, here just taking 2015 as x and 
# pass rate in 2015 as y, for illustration
textlabels <- matrics_long %>% filter(year == 2015) %>% 
  mutate(xpos = year, ypos = pass_rate) # don't have to rename, again for illustration

s2 +
  geom_label(data = textlabels, aes(x = xpos, y = ypos, label = province)) +
  theme(legend.position = "none")

s2 +
  geom_label_repel(data = textlabels, aes(x = xpos, y = ypos, label = province)) +
  theme(legend.position = "none")

### Adding geoms from different datasets

# Task: make a figure that shows the CO2 concentration over time for a
# specific month as a line (like done above), and then on top of that
# plots the mean CO2 concentrations over time (i.e. averages over all
# months) as a bar graph

co2mean <- co2 %>% group_by(Year) %>% summarize(mean_co2 = mean(co2))

co2 %>% filter(Month == "June") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_bar(data = co2mean, aes(x = Year, y = mean_co2),
           stat = "identity", alpha = 0.4) + 
  geom_line(colour = "red")

# limiting the range on the y-axis
co2 %>% filter(Month == "June") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_bar(data = co2mean, aes(x = Year, y = mean_co2),
           stat = "identity", alpha = 0.4) + 
  geom_line(colour = "red") +
  ylim(c(300, 400))

# can't use ylim, drops the observations, need "coord_cartesian"
co2 %>% filter(Month == "June") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_bar(data = co2mean, aes(x = Year, y = mean_co2),
           stat = "identity", alpha = 0.4) + 
  geom_line(colour = "red") +
  coord_cartesian(ylim = c(300, 375))

# this is probably a better plot
co2 %>% filter(Month == "June") %>%
  ggplot(aes(x = Year, y = co2)) + 
  geom_line(data = co2mean, aes(x = Year, y = mean_co2)) + 
  geom_line(colour = "red") 

### Showing multiple plots

# Make a few plots
p1 <- diamonds %>% ggplot(aes(x = price)) + geom_density() + ggtitle("Default")
p2 <- diamonds %>% ggplot(aes(x = price)) + geom_density(adjust = 0.5) + ggtitle("Wiggly")
p3 <- diamonds %>% ggplot(aes(x = price)) + geom_density(adjust = 5) + ggtitle("Smooth")
p4 <- diamonds %>% ggplot(aes(x = price)) + geom_histogram() + ggtitle("Histogram")

# layout with gridExtra package
grid.arrange(p1, p2, p3, p4, nrow = 1)
grid.arrange(p1, p2, p3, p4, nrow = 2)
grid.arrange(p1, p2, p3, layout_matrix = matrix(c(1,2,1,3), nrow = 2))
grid.arrange(p1, p2, p3, layout_matrix = matrix(c(1,1,2,3), nrow = 2))

# layout with patchwork package
p1 + p2 
p1 + p2 + p3 + plot_layout(ncol = 3)
p1 + p2 + p3 + plot_layout(ncol = 3, widths = c(2, 1, 1))

# nesting plots
p1 + { p2 + p3 } + plot_layout(ncol = 2)
p1 + { p2 + { p3 + p4 } } + plot_layout(ncol = 1)

# various operators for manipulating layout (-, /, |)
p1 + p2 - p3 + plot_layout(ncol=1)
p1 + p2 - p3 - p4 + plot_layout(ncol=1)
p1 + p2 - p3 + p4 + plot_layout(ncol=1)
p1 + p2 - (p3 + p4) + plot_layout(ncol=1)
p1 + p2 - (p3 + p4 + p1) + plot_layout(ncol=1)

(p1 | p2 | p3) / p4
p4 / (p1 | p2 | p3)

# saving your plots
p_all <- p1 + p2 + p3 + p4 + plot_layout(ncol = 4)
ggsave(p_all, file = "myplot.png", width = 9, height = 3, dpi = 400)
