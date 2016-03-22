
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


setwd("C:/SATELLITE_STUFF/OMI_NO2/2013")
dir <- "C:/SATELLITE_STUFF/OMI_NO2/UK_Borders"

### Load Shapefile for UK 
shp <- readOGR(dsn = dir, layer = "GBR_adm0")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)

#### Load NO2 OMI satellite data 

NO2_2013_nc <- raster("g4.timeAvgMap.OMNO2d_003_ColumnAmountNO2CloudScreened.20130101-20131231_UK.nc")
NO2_2013_nc <- raster("g4.timeAvgMap.OMNO2d_003_ColumnAmountNO2CloudScreened.20130101-20131231.12W_48N_3E_59N.nc")

plot(NO2_2013_nc)
plot(shp, add=TRUE, lwd=1)


### crop raster over the UK shp file  ###############################

NO2_2013_nc_cropped <- crop(NO2_2013_nc, extent(shp))
NO2_2013_nc_cropped <- mask(NO2_2013_nc_cropped, shp)

plot(NO2_2013_nc_cropped)
plot(shp, add=TRUE, lwd=2)

### Exctract poitns from raster ######################################

NO2_2013_OMI <- rasterToPoints(NO2_2013_nc_cropped)
head(NO2_2013_OMI)
colnames(NO2_2013_OMI) <- c("Lon", "Lat", "NO2_2013")
NO2_2013_OMI <- as.data.frame (NO2_2013_OMI)
NO2_2013_OMI <- subset(NO2_2013_OMI, !is.na(NO2_2013) & NO2_2013>0)
write.csv(NO2_2013_OMI, file = "NO2_2013_OMI.csv", row.names=FALSE)


#### define projection for Satellite dataframe data ##############

crs <- projection(shp) ### get projections from shp file

NO2_2013_OMI <- SpatialPointsDataFrame(NO2_2013_OMI[,1:2], NO2_2013_OMI, 
                                       proj4string=CRS(crs))

plot(NO2_2013_OMI)

pts.poly <- over(NO2_2013_OMI, shp[,"GADMID"])
NO2_2013_OMI$id <- pts.poly$GADMID

# Aggregate by zone/polygon
###  Make a dataframe ###
data_points <- NO2_2013_OMI@data
head(data_points)


#### Sum of emission in UK polygon [molecules/cm2]
data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(NO2_sum = sum(NO2_2013))


NO2_sum_UK <- (data_points$NO2_sum[1]) ### molecules/cm2
Surface_UK <- (shp@data$SQKM)*1e+10 ### from km2 to cm2
NO2_MASS <- 46.055 ### g/mol
N_Avogadro <- 6.022140857e+23
Total_mass_grams <- (NO2_sum_UK * Surface_UK * NO2_MASS)/ N_Avogadro
Total_mass_ktonnes <- Total_mass_grams/1000000000


