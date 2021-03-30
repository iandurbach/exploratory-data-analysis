# R code for goodpractice lecture

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(formatR)
library(gridExtra)
library(ggrepel)

## Slide 27
my_data <- data.frame(
  variable = c("A", "B", "C", "D", "E"), 
  value = c(23, 22, 19.5, 18.5, 17))

# don't worry too much about code below -- pie charts are hard to make in 
# ggplot, for good reason
ggplot(my_data, aes(x = factor(1), y = value)) +
  geom_bar(width = 1, colour = "black", fill = "white", stat = "identity") + 
  geom_text(aes(x = 1.7, y = cumsum(value) - 10, label = variable)) +
  coord_polar(theta = "y") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = rel(1.5), face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())



## ---- echo=FALSE, fig.height=5-------------------------------------------------------------------
ggplot(my_data, aes(x = value, y = reorder(variable, value))) +  
  geom_point() 


## ---- echo=FALSE, fig.height=5-------------------------------------------------------------------
my_data <- data.frame(
  year = c(1977, 1978, 1979, 1980),
  USA = c(30.0, 29.1, 27.2, 22.0),
  West.Germany = c(12.4, 12.4, 12.7, 12.2),
  France = c(11.6, 11.5, 12.1, 12.0), 
  Japan = c(17.7, 19.0, 19.9, 24.3),
  Other = c(28.3, 28.0, 28.1, 29.5))

my_data_long <- pivot_longer(my_data, cols = USA:Other, names_to = "country", values_to = "production")

my_data_long <- arrange(my_data_long, country)

# making a bar chart
ggplot(my_data_long, aes(x = factor(year), y = production, fill = country)) +
  # use 'stat = "identity"' when just want to plot raw numbers
  geom_bar(stat = "identity", width = 0.7, colour = "black") +
  # qualitative colour palette, see ?scale_fill_brewer
  scale_fill_brewer(type = "qual") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100))


## ---- echo=FALSE, fig.height=5-------------------------------------------------------------------
# new: facetting
# new: manual axis labels
ggplot(my_data_long, aes(x = year, y = production)) +
  geom_line() +
  geom_point() +
  facet_grid(~ country) + 
  scale_x_continuous(labels = c(1977, "", 1979, "")) 


## ---- echo=FALSE, fig.height=5-------------------------------------------------------------------
# new: reordering factor levels on an axis with 'reorder'
# new: geom_line(group = 1)
# new: make titles with 'labs' argument
ggplot(my_data_long %>% filter(year == "1980"),
       aes(x = reorder(country, production), y = production)) +
  geom_line(group = 1) + # need this because x is a factor
  geom_point() +
  labs(title = "Share of world car production in 1980") 


## ---- echo=FALSE, fig.height=5-------------------------------------------------------------------
ggplot(my_data_long %>% filter(country == "France"), 
       aes(x = year, y = production)) +
  geom_line() +
  geom_point() +
  labs(title = "France's share of world car production") 


## ---- echo=FALSE---------------------------------------------------------------------------------
acres <- data.frame(year = c(1914, 1931, 1936, 1942, 1952, 1962),
                    acres = c(16, 43, 80, 95, 110, 138))

ggplot(acres, aes(x = factor(year), y = acres)) +
  geom_point() + geom_line(group = 1) +
  labs(x = "Year", y = "Number of Acres") 


## ---- echo=FALSE---------------------------------------------------------------------------------
ggplot(acres, aes(x = year, y = acres)) +
  geom_point() + geom_line() +
  labs(x = "Year", y = "Number of Acres") 


## ----echo=FALSE,out.width=600--------------------------------------------------------------------
include_graphics("figs/tufte_opecrates.jpg")


## ----echo=FALSE----------------------------------------------------------------------------------
# new: more `labs' arguments (x and y axis labels)
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

matrics_long %>% 
  filter(province == "NC", year %in% c("2016", "2017")) %>%
  ggplot(aes(x = year, y = pass_rate)) +
  geom_line() + geom_point() +
  labs(x = "Year", y = "Pass rate (%)", 
       title = "Northern Cape education in crisis!") +
  scale_x_continuous(breaks = c(2016, 2017))


## ---- echo=FALSE---------------------------------------------------------------------------------
matrics_long %>% 
  filter(province == "NC") %>%
  ggplot(aes(x = year, y = pass_rate)) +
  geom_line() + geom_point() +
  labs(x = "Year", y = "Pass rate (%)", 
       title = "Northern Cape education in crisis?") 


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: colour palettes with scale_colour_brewer
matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + geom_point() + 
  scale_colour_brewer(palette = "Set3")


## ------------------------------------------------------------------------------------------------
data("ChickWeight")
ChickWeight <- ChickWeight %>% 
  mutate(Week = factor(1 + round(Time/7)))
head(ChickWeight)


## ---- echo = FALSE-------------------------------------------------------------------------------
# new: alpha for transparency
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = Diet), alpha = 0.2) 


## ---- echo = FALSE-------------------------------------------------------------------------------
# new: fill and colour
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = Diet, colour  = Diet), alpha = 0.2) 


## ---- echo = FALSE-------------------------------------------------------------------------------
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = Week, colour = Week), alpha = 0.2) 


## ---- echo = FALSE-------------------------------------------------------------------------------
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = Diet, colour = Week), alpha = 0.2) 


## ---- echo = FALSE, warnings = TRUE--------------------------------------------------------------
# new: interaction
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = interaction(Diet,Week), 
                   colour = interaction(Diet,Week)), alpha = 0.2) +
  scale_fill_brewer(type = "qual")



## ---- echo = FALSE-------------------------------------------------------------------------------
# new: facet_wrap with free scaling
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = Diet), alpha = 0.2) + 
  facet_wrap(~Week, scales = "free")


## ---- echo = FALSE-------------------------------------------------------------------------------
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = Week), alpha = 0.2) + 
  facet_wrap(~Diet, scales = "free")


## ---- echo = FALSE-------------------------------------------------------------------------------
# new: adjust kernel width to control smoothing in geom_density()
ChickWeight %>% ggplot(aes(x = weight)) + 
  geom_density(aes(fill = Week), alpha = 0.2, adjust = 3) + 
  facet_wrap(~Diet, scales = "free")


## ----echo=FALSE, out.width=600-------------------------------------------------------------------
include_graphics("figs/hatching.png")


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: manual setting of axes limits with xlim and ylim
df <- data.frame(x = 1:500, y = log(1:500) + rnorm(100, 0, 0.4)) 

df %>% ggplot(aes(x = x, y = y)) + 
  geom_point() +
  xlim(0,5000) + ylim(c(0, 30))


## ---- echo=FALSE, warning = TRUE, fig.height=4---------------------------------------------------
df <- data.frame(x = 1:500, y = log(1:500) + rnorm(100, 0, 0.4)) 

df %>% ggplot(aes(x = x, y = y)) + 
  geom_point() +
  xlim(0,400) + ylim(c(0, 30))


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: add best fit smooth through points with stat_smooth
data(co2)
co2_data <- data.frame(
  co2 = as.numeric(co2),
  date = seq(as.Date("1959-01-01"),  by = "month", length.out = 12*39))

co2_data <- subset(co2_data, date < as.Date("1991-01-01"))

p1 <- ggplot(co2_data, aes(x = date, y = co2)) +
  geom_point() + stat_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(x = NULL, y = "Carbon Dioxide (ppm)") 
p1


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: manually set axis breaks 
p1 + scale_y_continuous(breaks = seq(0, 300, 100), limits = c(0, 400))


## ---- echo=FALSE, fig.height=4-------------------------------------------------------------------
data(msleep)

a <- ggplot(msleep, aes(x = bodywt, y = brainwt)) + 
  geom_point(na.rm = TRUE) + 
  labs(x = "Body weight (kg)", y = "Brain weight (kg)")
a 


## ---- echo=FALSE, fig.height=4-------------------------------------------------------------------
# new: log scaling
a + scale_x_log10() + scale_y_log10() 


## ---- echo=FALSE, fig.height=4-------------------------------------------------------------------
# new: nice tick marks with annotation_xxx
a + scale_x_log10() + scale_y_log10() + annotation_logticks() 


## ----echo=FALSE, fig.height=4--------------------------------------------------------------------
# new: advanced axis breaks and labelling options with scales package
a + scale_x_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  labs(x = "Body weight (kg)", y = "Brain weight (kg)")


## ---- echo=FALSE, fig.height=5-------------------------------------------------------------------
# bad way
ggplot(my_data_long %>% filter(year == "1980"),
       aes(x = reorder(country, production), y = production)) +
  geom_line(group = 1) + # need this because x is a factor
  geom_point() +
  theme_bw() 


## ---- echo=FALSE, fig.height=5-------------------------------------------------------------------
# good way
ggplot(my_data_long %>% filter(year == "1980"),
       aes(x = reorder(country, production), y = production)) +
  geom_line(group = 1) + # need this because x is a factor
  geom_point() +
  labs(x = "Country", y = "Share of world car production in 1980 (%)") +
  theme_bw() 


## ---- echo=FALSE---------------------------------------------------------------------------------
matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + geom_point() + 
  scale_colour_brewer(palette = "Set3")


## ---- echo=FALSE---------------------------------------------------------------------------------
matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + geom_point() + 
  labs(x = "Year", y = "Pass rate (%)", colour = "Province") +
  scale_colour_brewer(palette = "Set3")


## ---- echo=FALSE, fig.width=4, fig.align='default'-----------------------------------------------
# new: geom_histogram
# new: set limits using data rather than fixed values
set.seed(123)
houseprice <- rbind(data.frame(suburb = "A", price = rnorm(3000, 3000, 1000)),
                    data.frame(suburb = "B", price = rnorm(1000, 6000, 500)))

hpA <- houseprice %>% filter(suburb == "A")
hpB <- houseprice %>% filter(suburb == "B")

ggplot(hpA, aes(price)) +
  geom_histogram(breaks = seq(min(hpA$price), max(hpA$price), length.out = 25)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb A: 2010-2019") 

ggplot(hpB, aes(price)) +
  geom_histogram(breaks = seq(min(hpB$price), max(hpB$price), length.out = 25)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb B: 2010-2019") 


## ---- echo=FALSE, fig.width=4, fig.align='default'-----------------------------------------------
ggplot(hpA, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb A: 2010-2019") 

ggplot(hpB, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb B: 2010-2019") 


## ---- echo=FALSE, fig.width=4, fig.align='default'-----------------------------------------------
ggplot(hpA, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb A: 2010-2019") +
  ylim(c(0,300))

ggplot(hpB, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb B: 2010-2019") +
  ylim(c(0,300))


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: grid.arrange for layout of plots
p1 <- ggplot(hpA, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb A: 2010-2019") +
  ylim(c(0,300))

p2 <- ggplot(hpB, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb B: 2010-2019") +
  ylim(c(0,300))

grid.arrange(p1, p2, ncol = 2)


## ---- echo=FALSE, fig.width=4, fig.height=5------------------------------------------------------
grid.arrange(p1, p2, ncol = 1)


## ---- echo = FALSE-------------------------------------------------------------------------------
ggplot(houseprice, aes(price)) +
  geom_histogram(bins = 40) +
  labs(x = "Sale price (R 000)", title = "Sales 2010-2019") + 
  facet_grid(. ~ suburb) # try with scales="free" option added 


## ---- echo = FALSE, fig.width=7------------------------------------------------------------------
# new: older/other way to set labs (xlab and ggtitle)
ggplot(houseprice, aes(price)) +
  geom_histogram(bins = 25) +
  xlab("Sale price (R 000)")  +
  ggtitle("Sales 2010-2019") + 
  facet_grid(suburb ~ .)


## ---- echo = FALSE, fig.width=7.5, fig.height=5--------------------------------------------------
ggplot(hpA, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb A: 2010-2019") +
  ylim(c(0,400))


## ---- echo = FALSE, fig.width=5, fig.height=5----------------------------------------------------
ggplot(hpA, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb A: 2010-2019") +
  ylim(c(0,400))


## ---- echo = FALSE, fig.width=2, fig.height=5----------------------------------------------------
ggplot(hpA, aes(price)) +
  geom_histogram(breaks = seq(min(houseprice$price), max(houseprice$price), length.out = 40)) +
  labs(x = "Sale price (R 000)", title = "Sales in Suburb A: 2010-2019") +
  ylim(c(0,400))


## ----echo=FALSE, out.width=300-------------------------------------------------------------------
# All defaults
include_graphics("figs/houseprice.png")


## ----echo=FALSE, out.width=700-------------------------------------------------------------------
# All defaults
include_graphics("figs/houseprice_stretched.png")


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: panel.background, panel.grid (see ?theme for LOTS of options, experiment!)
# new: element_xxx (see ?element_line for details)
ggplot(co2_data, aes(x = date, y = co2)) +
  geom_point() + stat_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(x = NULL, y = "Carbon Dioxide (ppm)") +
  theme(panel.background = element_rect(fill = 'gray40'),
        panel.grid = element_line(colour = 'gray60'))


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: more options for element_xxx (colour, size)
ggplot(co2_data, aes(x = date, y = co2)) +
  geom_point() + stat_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(x = NULL, y = "Carbon Dioxide (ppm)") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid = element_line(colour = 'gray20', size = 2))


## ---- echo=FALSE---------------------------------------------------------------------------------
ggplot(co2_data, aes(x = date, y = co2)) +
  geom_point() + stat_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(x = NULL, y = "Carbon Dioxide (ppm)") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid = element_line(colour = 'gray20', size = 0.02))


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: theme_bw()
ggplot(co2_data, aes(x = date, y = co2)) +
  geom_point() + stat_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(x = NULL, y = "Carbon Dioxide (ppm)") +
  theme_bw()


## ---- echo=FALSE---------------------------------------------------------------------------------
ggplot(msleep, 
       aes(x = bodywt, y = brainwt, colour = order)) +
  geom_point(na.rm = TRUE) + 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10",scales::math_format(10^.x))) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  xlab("Body weight (kg)") + ylab("Brain weight (kg)") 


## ---- echo=FALSE---------------------------------------------------------------------------------
matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + theme_bw() + scale_colour_brewer(palette = "Set3")


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: geom_label

# make dataframe with x and y coords of labels, here just taking 2015 as x and 
# pass rate in 2015 as y, for illustration
textlabels <- matrics_long %>% filter(year == 2015) %>% 
  mutate(xpos = year, ypos = pass_rate) # don't have to rename, again for illustration

matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + theme_bw() + scale_colour_brewer(palette = "Set3") +
  geom_label(data = textlabels, aes(x = xpos, y = ypos, label = province)) +
  theme(legend.position = "none")


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: ggrepel changes geom_label to geom_label_repel
# new: removing legend with theme(...)
matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + theme_bw() + scale_colour_brewer(palette = "Set3") +
  geom_label_repel(data = textlabels, aes(x = xpos, y = ypos, label = province)) +
  theme(legend.position = "none")


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: greater control over legend with guides() and guide_legend()
ggplot(msleep, 
       aes(x = bodywt, y = brainwt, colour = order)) +
  geom_point(na.rm = TRUE) + 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10",scales::math_format(10^.x))) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  xlab("Body weight (kg)") + ylab("Brain weight (kg)") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncols = 8))


## ---- echo=FALSE---------------------------------------------------------------------------------
# new: more control over legend with theme(...)
matrics_long %>% 
  ggplot(aes(x = year, y = pass_rate, colour = province)) +
  geom_line() + theme_bw() + scale_colour_brewer(palette = "Set3") +
  # coordinates for legend.position are x- and y- offsets from the bottom-left of the plot, ranging from 0 - 1.
  theme(legend.position = c(0.82,0.12), legend.title = element_blank(), 
        legend.text=element_text(size=8)) +
  guides(colour = guide_legend(ncol = 3))


## ----echo=FALSE,out.width=600--------------------------------------------------------------------
include_graphics("figs/corruption.png")


## ----echo=FALSE,out.width=700--------------------------------------------------------------------
include_graphics("figs/sharktrans.png")


## ----echo=FALSE,out.width=600--------------------------------------------------------------------
include_graphics("figs/broman/tableB.png")


## ----echo=FALSE,out.width=700--------------------------------------------------------------------
include_graphics("figs/bigtable.png")


## ----echo=FALSE,out.width=600--------------------------------------------------------------------
include_graphics("figs/biastable.png")


## ----echo=FALSE,out.width=600--------------------------------------------------------------------
include_graphics("figs/biasfigure.png")

