library(maptools)
library(rgdal)

bng = CRS("+init=epsg:22770") # british national grid
wards = readShapePoly("/Data/boundary-files/glondon_wards_2001/glondon_caswa_2001.shp",
+ proj4string=bng) 
stations = read.csv("/Data/tfl/reference_files/stations_imd_llsoa_wards.csv")
coordinates(stations) = c("osd_lon","osd_lat")
proj4string(stations) = bng
plot(wards)
plot(stations,add=T,col="red",pch=15)