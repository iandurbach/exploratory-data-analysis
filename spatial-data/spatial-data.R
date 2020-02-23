## ---- include = FALSE----------------------------------------------------------------
knitr::opts_chunk$set(fig.height=3.5,
                      fig.align='center',
                      tidy.opts=list(width.cutoff=60),
                      tidy=TRUE, 
                      warning = FALSE,
                      message=FALSE) 
library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(leaflet)
library(stringr)
library(tmap)
library(spData)
library(spDataLarge)
library(knitr)


## ----wrap-hook, echo=FALSE-----------------------------------------------------------
# text wrapping for nice slides
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})
knitr::opts_chunk$set(linewidth=60)


## ---- echo = FALSE, fig.height=5-----------------------------------------------------
rsa_towns <- st_read("data/rsa_towns.shp", quiet = TRUE)
plot(st_geometry(rsa_towns))


## ---- echo = FALSE, fig.height=5-----------------------------------------------------
plot(nz["Median_income"])


## ---- echo = FALSE, fig.height=5-----------------------------------------------------
mydata <- expand.grid(x = 1:10, y = 1:10) %>% 
  mutate(z1 = x + y, 
         z2 = (x-mean(x))^2 + (y-mean(y))^2)

myraster <- rasterFromXYZ(mydata)
plot(myraster[["z2"]])


## ---- echo = FALSE, fig.height=5-----------------------------------------------------
plot(elevation)


## ------------------------------------------------------------------------------------
# POINT
(pt1 <- st_point(c(5, 2))) 


## ---- echo = FALSE-------------------------------------------------------------------
plot(pt1)
title("POINT", line = 0.1)
box()


## ------------------------------------------------------------------------------------
# LINESTRING
(ls1 <- st_linestring(rbind(c(5, 2), c(1, 3), c(3, 4))))           


## ---- echo = FALSE-------------------------------------------------------------------
plot(ls1)
title("LINESTRING", line = 0.1)
box()


## ------------------------------------------------------------------------------------
# POLYGON
pl <- list(rbind(c(5, 2), c(1, 3), c(3, 4), c(5, 2)))
(poly1 <- st_polygon(pl))  


## ---- echo = FALSE-------------------------------------------------------------------
plot(poly1)
title("POLYGON", line = 0.1)
box()


## ------------------------------------------------------------------------------------
# MULTIPOINT
mpmat <- rbind(c(5, 2), c(1, 3), c(3, 4))
(mp1 <- st_multipoint(mpmat))


## ---- echo = FALSE-------------------------------------------------------------------
plot(mp1)
title("MULTIPOINT", line = 0.1)
box()


## ------------------------------------------------------------------------------------
# MULTILINESTRING
mll <- list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2)), 
            rbind(c(1, 2), c(2, 4)))
(mls1 <- st_multilinestring((mll)))


## ---- echo = FALSE-------------------------------------------------------------------
plot(mls1)
title("MULTILINESTRING", line = 0.1)
box()


## ------------------------------------------------------------------------------------
# MULTIPOLYGON
mpyl <- list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(1, 5))),
             list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
(mpoly1 <- st_multipolygon(mpyl))


## ---- echo = FALSE-------------------------------------------------------------------
plot(mpoly1)
title("MULTIPOLYGON", line = 0.1)
box()


## ------------------------------------------------------------------------------------
# GEOMETRYCOLLECTION
gcl <- list(ls1, pt1, mpoly1)
(geomcol <- st_geometrycollection(gcl))


## ---- echo = FALSE-------------------------------------------------------------------
plot(geomcol)
title("GEOMETRYCOLLECTION", line = 0.1)
box()


## ------------------------------------------------------------------------------------
class(pt1)
class(mp1)


## ------------------------------------------------------------------------------------
# sfc POINT
pt1 <- st_point(c(5, 2))
pt2 <- st_point(c(1, 3))
(points_sfc <- st_sfc(pt1, pt2))


## ------------------------------------------------------------------------------------
class(points_sfc)
is.list(points_sfc) # is this a list?
is.list(mp1)        # is this (multipoint) a list?
st_geometry_type(points_sfc)


## ------------------------------------------------------------------------------------
lnd_point <- st_point(c(0.1, 51.5))                 # sfg object
lnd_geom <- st_sfc(lnd_point, crs = 4326)           # sfc object
lnd_attrib <- data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf <- st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
lnd_sf


## ------------------------------------------------------------------------------------
st_crs(lnd_sf)


## ------------------------------------------------------------------------------------
x_ll <- read.csv("data/confirmed-sightings.csv")
x_ll <- st_as_sf(x_ll, coords = c("Longitude", "Latitude"), crs = 4326)
st_crs(x_ll)
d_ll <- st_distance(x_ll[1,], x_ll[2,])


## ---- tidy = FALSE-------------------------------------------------------------------
x_utm <- st_transform(x_ll, 
                      crs = "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs")
st_crs(x_utm)


## ------------------------------------------------------------------------------------
d_utm <- st_distance(x_utm[1,], x_utm[2,])
c(d_ll, d_utm)


## ------------------------------------------------------------------------------------
occupancies <- st_read("data/Mongolia_SL.shp", quiet = TRUE)
head(occupancies)


## ------------------------------------------------------------------------------------
plot(st_geometry(occupancies))


## ------------------------------------------------------------------------------------
plot(occupancies["Team"])


## ------------------------------------------------------------------------------------
plot(occupancies["Occ"])


## ------------------------------------------------------------------------------------
occupancies %>% st_crs()     # check the CRS


## ------------------------------------------------------------------------------------
occupancies <- st_read("data/Mongolia_SL.shp", quiet = TRUE) %>% 
  st_set_crs("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs")
occupancies %>% st_crs()     


## ------------------------------------------------------------------------------------
sights <- read.csv("data/confirmed-sightings.csv")
sights <- st_as_sf(sights, coords = c("Longitude", "Latitude"), crs = 4326)
sights %>% st_crs()


## ---- tidy = FALSE-------------------------------------------------------------------
# reprojecting
my_crs <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
sights <-  st_transform(sights, crs = my_crs)


## ------------------------------------------------------------------------------------
plot(sights["Elevation"])


## ------------------------------------------------------------------------------------
raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
new_raster <- raster(raster_filepath)
plot(new_raster)


## ---- tidy=FALSE---------------------------------------------------------------------
mydata <- expand.grid(x = 1:100, y = 1:100) %>% 
  mutate(z1 = x + y, z2 = x - y)
myraster <- rasterFromXYZ(mydata)
plot(myraster[["z2"]])


## ------------------------------------------------------------------------------------
raster1 <- raster(nrows = 10, ncols = 10, res = 1, 
                 xmn = 0, xmx = 10, ymn = 0, ymx = 10,
                 vals = 1:100)
plot(raster1)


## ------------------------------------------------------------------------------------
raster2 <- raster(nrows = 10, ncols = 10, res = 1, 
                 xmn = 0, xmx = 10, ymn = 0, ymx = 10,
                 vals = runif(100))
myrb <- brick(raster1, raster2)
plot(myrb)


## ------------------------------------------------------------------------------------
occ_gt90 <- occupancies %>% filter(Occ > 0.6)
plot(occ_gt90)


## ------------------------------------------------------------------------------------
sights_per_cell <- st_intersects(x = occupancies, y = sights)
sel_logical <- lengths(sights_per_cell) > 0
occ_with_sights <- occupancies[sel_logical, ]
plot(occ_with_sights["Occ"])


## ---- message=FALSE, results='hide'--------------------------------------------------
occupancies %>% group_by(Team) %>%
  summarize(meanOcc = mean(Occ)) %>% head()


## ---- tidy=FALSE---------------------------------------------------------------------
occ_per_team <- occupancies %>% group_by(Team) %>%
  summarize(meanOcc = mean(Occ),
            geometry = st_union(geometry))
plot(occ_per_team[1:12,"meanOcc"])


## ---- fig.height = 2.5---------------------------------------------------------------
map0 <- tm_shape(occupancies) + tm_fill()
map1 <- tm_shape(occupancies) + tm_borders()
tmap_arrange(map0, map1, nrow = 1)


## ------------------------------------------------------------------------------------
map2 <- tm_shape(occupancies) + tm_polygons()
map3 <- tm_shape(occupancies) + tm_polygons("Occ")
tmap_arrange(map2, map3, nrow = 1)


## ------------------------------------------------------------------------------------
tm_shape(occupancies) +
  tm_polygons("Occ") +
  tm_text("Team", size = 0.4)


## ---- fig.cap="Color settings are especially important for maps. Here use of the same palette obscures detail."----
tm_shape(occupancies) + tm_polygons("Occ") +
  tm_shape(sights) + tm_dots("Elevation", size = 0.5)


## ---- echo=TRUE----------------------------------------------------------------------
tm_shape(occupancies) + tm_polygons("Occ") +
  tm_shape(sights) + tm_dots("Elevation", size = 0.5, palette = "BuGn")


## ---- echo=TRUE----------------------------------------------------------------------
tm_shape(occupancies) + tm_polygons("Occ") +
  tm_shape(sights) + tm_dots("Mountain", size = 0.5, palette = "Set3")


## ---- echo=TRUE----------------------------------------------------------------------
tm_shape(occupancies) + tm_polygons("Occ") +
  tm_shape(sights) + tm_dots("Elevation", size = 0.5, palette = "PRGn")


## ---- echo = FALSE, fig.height = 7---------------------------------------------------
library(RColorBrewer)
display.brewer.all()


## ----tidy = FALSE--------------------------------------------------------------------
tm_shape(occupancies) +
  tm_polygons(c("Occ", "Team")) +
  tm_facets(sync = TRUE, ncol = 2)


## ------------------------------------------------------------------------------------
ggplot() + geom_sf(data = occupancies)


## ------------------------------------------------------------------------------------
ggplot() + geom_sf(data = occupancies, aes(fill = Occ))


## ---- tidy = FALSE-------------------------------------------------------------------
ggplot() + geom_sf(data = occupancies, aes(fill = Occ)) +
  geom_sf(data = sights, colour = "red")


## ---- tidy = FALSE-------------------------------------------------------------------
ggplot() + geom_sf(data = occupancies, aes(fill = Occ)) +
  geom_sf(data = sights, aes(colour = Elevation)) +
  scale_colour_distiller(palette = "PRGn")


## ------------------------------------------------------------------------------------
leaflet() %>% addTiles()


## ------------------------------------------------------------------------------------
leaflet() %>% addTiles() %>%
  setView(lng = 94.1, lat = 47.4, zoom = 7)


## ---- tidy = FALSE-------------------------------------------------------------------
leaflet() %>% addTiles() %>%
  setView(lng = 94.1, lat = 47.4, zoom = 4)


## ---- tidy = FALSE-------------------------------------------------------------------
leaflet() %>% addProviderTiles("Esri.WorldStreetMap") %>%
  setView(lng = 94.1, lat = 47.4, zoom = 4)


## ---- tidy = FALSE-------------------------------------------------------------------
leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = 94.1, lat = 47.4, zoom = 4)


## ---- tidy = FALSE-------------------------------------------------------------------
### <b>
leaflet(data = st_transform(occupancies, crs = 4326)) %>%
  ### </b>
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons()


## ---- tidy = FALSE-------------------------------------------------------------------
leaflet(data = st_transform(occupancies, crs = 4326)) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons() %>%
  ### <b>
  addMarkers(data = st_transform(sights, crs = 4326))
### </b>


## ---- tidy = FALSE-------------------------------------------------------------------
### <b>
pal <- colorNumeric("YlOrRd", domain = occupancies$Occ)
### </b>
leaflet(st_transform(occupancies, crs = 4326)) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(data = st_transform(sights, crs = 4326)) %>%
  ### <b>
  addPolygons(color = ~pal(Occ), fillOpacity = 0.4) %>%
  addLegend(pal = pal, values = ~Occ) %>%
  addMiniMap(position = "bottomleft")
### </b>

