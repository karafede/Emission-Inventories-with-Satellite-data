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


setwd("C:/SATELLITE_STUFF/GIOVANNI_NASA/CH4")
dir <- "C:/SATELLITE_STUFF/OMI_NO2/UK_Borders"

### Load Shapefile for UK 
shp <- readOGR(dsn = dir, layer = "GBR_adm0")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)


#### Load CH4 AIRS satellite data 

CH4_2013_nc <- raster("g4.timeAvgMap.AIRX3STD_006_TotCH4_A.20130101-20131231.12W_48N_3E_59N.nc")

plot(CH5_2013_nc)
plot(shp, add=TRUE, lwd=1)


### crop raster over the UK shp file  ###############################

CH4_2013_nc_cropped <- crop(CH4_2013_nc, extent(shp))
CH4_2013_nc_cropped <- mask(CH4_2013_nc_cropped, shp)

plot(CH4_2013_nc_cropped)
plot(shp, add=TRUE, lwd=2)


### Exctract poitns from raster ######################################

CH4_2013_AIRS <- rasterToPoints(CH4_2013_nc_cropped)
colnames(CH4_2013_AIRS) <- c("Lon", "Lat", "CH4_2013")
CH4_2013_AIRS <- as.data.frame (CH4_2013_AIRS)
CH4_2013_AIRS <- subset(CH4_2013_AIRS, !is.na(CH4_2013) & CH4_2013>0)
write.csv(CH4_2013_AIRS, file = "CH4_2013_OMI.csv", row.names=FALSE)


#### define projection for Satellite dataframe data ##############

crs <- projection(shp) ### get projections from shp file

CH4_2013_AIRS <- SpatialPointsDataFrame(CH4_2013_AIRS[,1:2], CH4_2013_AIRS, 
                                       proj4string=CRS(crs))

plot(CH4_2013_AIRS)

pts.poly <- over(CH4_2013_AIRS, shp[,"GADMID"])
CH4_2013_AIRS$id <- pts.poly$GADMID

# Aggregate by zone/polygon
###  Make a dataframe ###
data_points <- CH4_2013_AIRS@data
head(data_points)


#### Sum of emission in UK polygon 
data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(CH4_sum = sum(CH4_2013))  


CH4_sum_UK <- (data_points$CH4_sum[1]) ### molecules/cm2
Surface_UK <- (shp@data$SQKM)*1e+10 ### from km2 to cm2
CH4_MASS <- 16.04 ### g/mol
N_Avogadro <- 6.022140857e+23
Total_mass_grams <- (CH4_sum_UK * Surface_UK * CH4_MASS)/ N_Avogadro
Total_mass_ktonnes <- Total_mass_grams/1000000000  #### converison in Ktonnes
Total_mass_CO2_eq <-  Total_mass_ktonnes/25 


