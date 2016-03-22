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


setwd("C:/SATELLITE_STUFF/GIOVANNI_NASA/PM2.5_Donkelaar")
dir <- "C:/SATELLITE_STUFF/OMI_NO2/UK_Borders"

### Load Shapefile for UK 
shp <- readOGR(dsn = dir, layer = "GBR_adm0")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)

#### Load PM25 from satellite data (Donkelaar et al.) 
PM25_2009_2011_SAT <- read.csv("PM25_UK_2009_2011_clean.csv", header = TRUE)
PM25_2009_2011_SAT <- subset(PM25_2009_2011_SAT, !is.na(PM25) & PM25>0)


########################################################################

#### define projection for Satellite dataframe data ##############

crs <- projection(shp) ### get projections from shp file

PM25_2009_2011_SAT <- SpatialPointsDataFrame(PM25_2009_2011_SAT[,1:2], PM25_2009_2011_SAT, 
                                   proj4string=CRS(crs)) 

plot(PM25_2009_2011_SAT)

#### Points into inside polygon (UK)
pts.poly <- over(PM25_2009_2011_SAT, shp[,"GADMID"])
PM25_2009_2011_SAT$id <- pts.poly$GADMID

# Aggregate by zone/polygon
###  Make a dataframe ###
data_points <- PM25_2009_2011_SAT@data

#### Sum of emission in UK polygon [ppm]
data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(PM25_sum = sum(PM25))  


PM25_sum_UK <- (data_points$PM25_sum[1]) ### ug/m3


