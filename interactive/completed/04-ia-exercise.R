library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(sf)
library(gapminder)
library(rnaturalearth)
library(gganimate)

# load country stats over time
data(gapminder)
# the rnaturalearth package contains maps from the Natural Earth dataset
# use this to get spatial objects (polygons) for each country
world <- ne_countries(returnclass = "sf")

# what we want to do is to merge the geometry column from the 'world' dataset
# with the rest of the data from the `gapminder` dataset

# one possible problem is that we might not have gapminder data for every country in
# the world. Another is that names might not match up exactly. Let's check on this.

# gapminder countries
gapnames <- as.character(unique(gapminder$country))
gapnames
# nearth countries
nenames <- unique(world$name_long)
nenames
# number of countries in gapminder not in nearth
sum(!(gapnames %in% nenames))
# which ones 
gapnames[!(gapnames %in% nenames)]
# we could go back and see if they really aren't there, or just a difference in 
# spelling. For now, we'll just assume we don't have data for these countries and 
# move on -- there are not too many of these tricky-to-handle countries

# extract African countries from gapminder
gapminder <- gapminder %>% filter(continent == "Africa") 
# extract African countries from world
africa <- world %>% filter(continent == "Africa") 

# we only need the geometry column from `africa` (remember we want to merge this
# into the gapminder data), so just select the columns we need
africa <- africa %>% dplyr::select(name = name_long, geometry)

# do a database join to add the sfc (simple feature column: the geometry) to 
# the gapminder data
africa_gap <- inner_join(gapminder, africa, by = c("country" = "name"))
class(africa_gap)

# inner_join resets object class so not an sf object any more. Need to tell R
# again that its an sf object
africa_gap <- st_as_sf(africa_gap)

# plot with transition_time - one frame per year
ptt <- ggplot(africa_gap) +
  geom_sf(data = africa, colour = "#ffffff20", fill = "#2d2d2d60", size = .5) +
  geom_sf(aes(fill = lifeExp, frame = year)) +
  scale_fill_distiller(palette = "YlOrRd") +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
animate(ptt, renderer = gifski_renderer(loop = T))

# plot with transition_manual - one frame per 5-year cycle
ptm <- ggplot(africa_gap) +
  geom_sf(data = africa, colour = "#ffffff20", fill = "#2d2d2d60", size = .5) +
  geom_sf(aes(fill = lifeExp, frame = year)) +
  scale_fill_distiller(palette = "YlOrRd") +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {current_frame}', x = 'GDP per capita', y = 'life expectancy') +
  transition_manual(year)
animate(ptm, renderer = gifski_renderer(loop = F))

# save your preferred one as a gif
anim_save(filename = "africa_changes.gif", animation = ptm)
