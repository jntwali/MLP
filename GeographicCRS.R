library(ggplot2)
library(rgdal)
library(raster)

newTheme <- list(theme(line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.position = 'none'))

worldBound <- readOGR(dsn = 'Data/earthanalyticswk4/global/ne_110m_land/ne_110m_land.shp')

worldBound_df <- fortify(worldBound)

worldMap <- ggplot(worldBound_df, aes(long, lat, group = group)) +
  geom_polygon() +
  coord_equal() +
  labs(x = 'Longitude (degrees)', 
       y = 'Latitude (degrees)',
       title = 'Global map - Geographic coordinate system', 
       subtitle = 'WGS84 Datum, units: Degrees - Latitute / Longitude')

graticule <- readOGR('Data/earthanalyticswk4/global/ne_110m_graticules_all', 
                     layer = 'ne_110m_graticules_15')

graticule_df <- fortify(graticule)

# plot graticules

ggplot() + geom_path(data = graticule_df, 
                     aes(long, lat, group = group),
                     linetype = 'dashed', colour = 'grey70')
# bounding box for ploting data


bbox <- readOGR('Data/earthanalyticswk4/global/ne_110m_graticules_all/ne_110m_wgs84_bounding_box.shp')

bbox_df <- fortify(bbox)


latLongMap <- ggplot(bbox_df, aes(long,lat, group = group)) +
  geom_polygon(fill = "white") +
  geom_polygon(data = worldBound_df, aes(long,lat, group = group, fill = hole)) +
  geom_path(data = graticule_df, aes(long, lat, group = group), linetype = "dashed", color = "grey70") +
  coord_equal() +  labs(title = "World Map - Geographic WGS84 (long/lat degrees)")  +
  newTheme +
  
  scale_fill_manual(values = c("black", "white"), guide = "none") # change colors & remove legend

# an expample to understand the Universal transverse Mercator


boulder_df <- data.frame(lon = c(476911.31), lat = c(4429455.35))

ggplot() + geom_point(data = boulder_df, 
                      aes(lon, lat, group = NULL), colour = 'springgreen',
                      size = 5)
# convert to spacial points

coordinates(boulder_df) <- 1:2

# assign CRS=> UTM zone 13 N

crs(boulder_df) <- CRS('+init=epsg:2957')

# geographic projection

boulder_df_geog <- spTransform(boulder_df, crs(worldBound))
coordinates(boulder_df_geog)

# plot data on the world map

boulder_df_geog <- as.data.frame(coordinates(boulder_df_geog))

# plot map

worldMap <- ggplot(worldBound_df, aes(long, lat, group = group)) +
  geom_polygon() +
  xlab('Longitude (Degrees)') +
  ylab('Latitute (Degrees)') +
  coord_equal() +
  ggtitle('Global map _ Geographic coordinate system -WGS84 Datum\n Units: Degrees - Latitute / Longitude')

worldMap + geom_point(data = boulder_df_geog, 
                      aes(lon, lat, group = NULL), 
                      colour = 'springgreen', size = 5)
  
