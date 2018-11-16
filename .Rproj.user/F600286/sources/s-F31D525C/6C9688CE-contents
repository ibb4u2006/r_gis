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
