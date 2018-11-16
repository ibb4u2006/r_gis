packages <- c('ggplot2', 'data.table', 'rgdal', 'raster', 'rgeos', 'reshape2', 'dplyr', 
              'RColorBrewer', 'leaflet', 'sf', 'mapview',  'tmap')

install.packages(packages)

for(i in 1:length(packages)){
  library(packages[i], character.only = TRUE)
}  

load("data/gpm_nl.rdata")
str(gpm_d_prcp)

gpm_d_prcp[, month := month(time)]
gpm_d_prcp[, year := year(time)]

gpm_d_prcp[, sum_month := sum(prcp), .(lon, lat, year)]

gpm_mon_prcp <- gpm_d_prcp[, .(mean(sum_month)), month]
colnames(gpm_mon_prcp)[2] <- "prcp"

ggplot(gpm_mon_prcp, aes(x = month)) +
  geom_bar()

load("data/gpm_nl.rdata")

one_day <- gpm_d_prcp[time == "2016-04-15"]
one_day[, time := NULL]

ggplot(one_day, aes(x = lon, y = lat, fill = prcp)) +
  geom_tile()

coordinates(one_day) <- ~ lon + lat
gridded(one_day) = TRUE
proj4string(one_day) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

one_day_raster <- raster(one_day)

#To plot spatial objects we will start with just plot()
plot(one_day_raster)

prcp_col <- brewer.pal(n = 9, name = 'Blues') # Create a pallete of blue
plot(one_day_raster, col = prcp_col)


load("data/gpm_nl.rdata")

#Create monthly data table
gpm_mon_prcp <- gpm_d_prcp
gpm_mon_prcp[, month := month(time)]
gpm_mon_prcp <- gpm_mon_prcp[, sum(prcp), .(lon, lat, month)]

#Pick a month
one_month <- gpm_mon_prcp[month == 3]
one_month[, month := NULL]

ggplot(one_month, aes(x = lon, y = lat, fill = V1)) +
  geom_tile()

#Transform data table to spatial object
coordinates(one_month) <- ~ lon + lat
gridded(one_month) = TRUE
proj4string(one_month) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#Transform spatial object to raster
one_month_raster <- raster(one_month)

#Plot the raster
plot(one_month_raster)

prcp_col <- brewer.pal(n = 9, name = 'Blues') # Create a pallete of blue
plot(one_month_raster, col = prcp_col)


##############

borders <- readOGR("data/NLD_adm0.shp")
plot(borders)

plot(one_day, col = prcp_col)
plot(borders, add = TRUE)

#Cropping the raster over a vector can be achieved with the mask() function:
one_day_masked <- mask(one_day_raster, borders)

plot(one_day_masked, col = prcp_col)
plot(borders, add = T)

#Now we can proceed with some statistical analysis over the new raster:
cellStats(one_day_masked, mean)
cellStats(one_day_masked, max)

hist(one_day_masked)

#we can aggregate the raster to a coarser resolution:
one_day_masked_agg <- aggregate(one_day_masked, fact = 3, fun = mean) #na.rm = T

plot(one_day_masked_agg, col = prcp_col)
plot(borders, add = T)

load("data/gpm_nl.rdata")
#Pick two days with max precipitation > 100

gpm_d_prcp[prcp > 100]
one_day_100_1 <- gpm_d_prcp[time == "2015-06-05"]
one_day_100_2 <- gpm_d_prcp[time == "2015-08-31"]

one_day_100_1[, time := NULL]
one_day_100_1[, time := NULL]

#Transform data table to spatial object
coordinates(one_day_100_1) <- ~ lon + lat
gridded(one_day_100_1) = TRUE
proj4string(one_day_100_1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

coordinates(one_day_100_2) <- ~ lon + lat
gridded(one_day_100_2) = TRUE
proj4string(one_day_100_2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Transform spatial object to raster
one_day_raster_100_1 <- raster(one_day_100_1)
one_day_raster_100_2 <- raster(one_day_100_2)

#Mask to borders
one_day_masked_100_1 <- mask(one_day_raster_100_1)
one_day_masked_100_2 <- mask(one_day_raster_100_2)

plot(one_day_masked_100_1, col = prcp_col)
plot(one_day_masked_100_2, col = prcp_col)

one_day_masked_agg_1 <- aggregate(one_day_masked_100_1, fact = 5, fun = mean) #na.rm = T
one_day_masked_agg_2 <- aggregate(one_day_masked_100_2, fact = 5, fun = mean) #na.rm = T

plot(one_day_masked_agg_1, col = prcp_col)
cellStats(one_day_masked_agg_1, var)
cellStats(one_day_masked_1, var)
cellStats(one_day_masked_100_1, var)
cellStats(one_day_masked_100_1, var)

one_day_diff_100 <- one_day_masked_100_1 - one_day_masked_100_2
plot(one_day_diff_100)

xy <- data.frame(x = 5.0, y = 52.0)
xy <- SpatialPoints(xy, proj4string = crs(one_day_masked_agg))
extract(one_day_masked_agg, xy)

extract(one_day_masked_agg, borders)[[1]]




#Online Maps
library(leaflet)
col_pal <- colorNumeric(rev(c("#0C2C84", "#41B6C4", "#FFFFCC")), 
                        values(one_day_masked$prcp),
                        na.color = "transparent")
leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addRasterImage(one_day_masked, colors = col_pal, opacity = 0.6) %>%
  addLegend(pal = col_pal, values = values(one_day_masked$prcp),
            title = "precip.")

map_types <- c('Esri.WorldImagery', 'Stamen.TerrainBackground','Stamen.Toner',
               'OpenStreetMap.BlackAndWhite', 'Esri.NatGeoWorldMap', 'Stamen.Watercolor')
mapview(one_day_masked, map.types = map_types) 
