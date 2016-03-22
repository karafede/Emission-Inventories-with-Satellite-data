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


setwd("C:/SATELLITE_STUFF/GIOVANNI_NASA/SO2")
dir <- "C:/SATELLITE_STUFF/OMI_NO2/UK_Borders"

### Load Shapefile for UK 
shp <- readOGR(dsn = dir, layer = "GBR_adm0")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)

#### Load SO2 OMI satellite data 

SO2_2013_nc <- raster("g4.timeAvgMap.OMSO2e_003_ColumnAmountSO2_PBL.20130101-20131231.13W_UK.nc")
SO2_2013_nc <- raster("g4.timeAvgMap.OMSO2e_003_ColumnAmountSO2_PBL.20130101-20131231.12W_48N_4E_60N.nc")

plot(SO2_2013_nc)
plot(shp, add=TRUE, lwd=1)

### crop raster over the UK shp file  ###############################

SO2_2013_nc_cropped <- crop(SO2_2013_nc, extent(shp))
SO2_2013_nc_cropped <- mask(SO2_2013_nc_cropped, shp)

plot(SO2_2013_nc_cropped)
plot(shp, add=TRUE, lwd=2)

### Exctract poitns from raster ######################################

SO2_2013_OMI <- rasterToPoints(SO2_2013_nc_cropped)
colnames(SO2_2013_OMI) <- c("Lon", "Lat", "SO2_2013")
SO2_2013_OMI <- as.data.frame (SO2_2013_OMI)
SO2_2013_OMI <- subset(SO2_2013_OMI, !is.na(SO2_2013) & SO2_2013>0)
write.csv(SO2_2013_OMI, file = "SO2_2013_OMI.csv", row.names=FALSE)


#### define projection for Satellite dataframe data ##############

crs <- projection(shp) ### get projections from shp file

#  CO_2013_SAT <- SpatialPointsDataFrame(CO_2013_SAT[,1:2], CO_2013_SAT, 
#                                     proj4string=CRS(crs))
SO2_2013_OMI <- SpatialPointsDataFrame(SO2_2013_OMI[,1:2], SO2_2013_OMI, 
                                       proj4string=CRS(crs))

plot(SO2_2013_OMI)


pts.poly <- over(SO2_2013_OMI, shp[,"GADMID"])
SO2_2013_OMI$id <- pts.poly$GADMID

# Aggregate by zone/polygon
###  Make a dataframe ###
data_points <- SO2_2013_OMI@data


#### Sum of emission in UK polygon [Dobon Units (DU)]
data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(SO2_sum = sum(SO2_2013))


SO2_sum_UK <- (data_points$SO2_sum[1])*2.69e+16 ### molecules/cm2
Surface_UK <- (shp@data$SQKM)*1e+10 ### from km2 to cm2
SO2_MASS <- 64.066 ### g/mol
N_Avogadro <- 6.022140857e+23
Total_mass_grams <- (SO2_sum_UK * Surface_UK * SO2_MASS)/ N_Avogadro
Total_mass_ktonnes <- Total_mass_grams/1000000000


