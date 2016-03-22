
library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(raster)   ## To convert an "Extent" object to a "SpatialPolygons" object.
library(rgeos)
library(rgdal)
library(plyr)
library(dplyr)
library(leaflet)
library(sp)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
library(spatialEco)

############ MAPPING ##########################################################
###############################################################################

### Read shape file #########


setwd("C:/SATELLITE_STUFF/GIOVANNI_NASA/PM25_CMAQ")
dir <- "C:/SATELLITE_STUFF/OMI_NO2/UK_Borders"

### Load Shapefile for UK 
shp <- readOGR(dsn = dir, layer = "GBR_adm0")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)

#### Load PM2.5 from CMAQ model (2009-2011) ################################# 

PM25_CMAQ_2009_2011 <- read.csv("PM25_CMAQ_2009_2011.csv", header = TRUE)


############   regridding on a regular grid at 10km ################################

PM25_CMAQ_2009_2011$x <- PM25_CMAQ_2009_2011$Lon
PM25_CMAQ_2009_2011$y <- PM25_CMAQ_2009_2011$Lat

coordinates(PM25_CMAQ_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(PM25_CMAQ_2009_2011)

x.range <- as.numeric(c(-9.04, 2.54))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(48.70, 62.05))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(PM25_CMAQ_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = PM25_AVG ~ 1, locations = PM25_CMAQ_2009_2011, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_AVG")  # give names to the modelled variables

write.csv(idw.output, file = "PM25_CMAQ_2009_2011_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_AVG))


#### define projection for Satellite dataframe data ##############

PM25_CMAQ_2009_2011 <- read.csv("PM25_CMAQ_2009_2011_interp.csv", heade = TRUE)
PM25_CMAQ_2009_2011 <-  subset(PM25_CMAQ_2009_2011, !is.na(PM25_AVG) & PM25_AVG>0)

crs <- projection(shp) ### get projections from shp file

PM25_CMAQ_2009_2011 <- SpatialPointsDataFrame(PM25_CMAQ_2009_2011[,1:2], PM25_CMAQ_2009_2011, 
                                             proj4string=CRS(crs)) 
# plot(PM25_CMAQ_2009_2011)


#### Points inside polygon (UK)
pts.poly <- over(PM25_CMAQ_2009_2011, shp[,"GADMID"])
PM25_CMAQ_2009_2011$id <- pts.poly$GADMID

# Aggregate by zone/polygon
###  Make a dataframe ###
data_points <- PM25_CMAQ_2009_2011@data

#### Sum of emission in UK polygon [ppm]
data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(PM25_sum = sum(PM25_AVG))  


PM25_sum_UK <- (data_points$PM25_sum[1]) ### ug/m3


