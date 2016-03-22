
library(RNetCDF)
library(fields)
library(maptools)
library(rgdal)

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
library(pracma)


setwd ("C:/SATELLITE_STUFF/GOSAT") 
dir <- "C:/SATELLITE_STUFF/OMI_NO2/UK_Borders"

### Read shape file #########

### Load Shapefile for UK 
shp <- readOGR(dsn = dir, layer = "GBR_adm0")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)


# Open data file
CH4 <- "GRIDDED_GOSAT_SWIR_XCH4_LEICESTER.nc"    ## 5 km resolution (ppbV)
CO2 <- "GRIDDED_GOSAT_SWIR_XCO2_LEICESTER.nc"  ## 5 km resolution (ppmV)

CH4 <- open.nc(CH4)
CO2 <- open.nc(CO2)

#Read data
CH4  <- read.nc(CH4)
CO2  <- read.nc(CO2)

LAT_CH4 <- as.vector(CH4$latitude_grid)  ### get Lat
LON_CH4 <- as.vector(CH4$longitude_grid)  ### get Lon
# CH4_grid <-as.matrix(CH4$gridded_data)  ### get cH4
LAT <- repmat(LAT_CH4,1,72)
LAT <- t(LAT)
LON = repmat(LON_CH4,27,1)
LON = c(LON)



# data, lat, long

Jan <- CH4$gridded_data[1, , ]
Jan[is.na(Jan)] <- 0 

Feb <- CH4$gridded_data[2, , ]
Feb[is.na(Feb)] <- 0 

Mar <- CH4$gridded_data[3, , ]
Mar[is.na(Mar)] <- 0 

Apr <- CH4$gridded_data[4, , ]
Apr[is.na(Apr)] <- 0 

May <- CH4$gridded_data[5, , ]
Jan[is.na(Jan)] <- 0 

Jun <- CH4$gridded_data[6, , ]
Jun[is.na(Jun)] <- 0 

July <- CH4$gridded_data[7, , ]
July[is.na(July)] <- 0 

Augu <- CH4$gridded_data[8, , ]
Augu[is.na(Augu)] <- 0 

Sep <- CH4$gridded_data[9, , ]
Sep[is.na(Sep)] <- 0 

Oct <- CH4$gridded_data[10, , ]
Oct[is.na(Oct)] <- 0 

Nov <- CH4$gridded_data[11, , ]
Nov[is.na(Nov)] <- 0 

Dec <- CH4$gridded_data[12, , ]
Dec[is.na(Dec)] <- 0 

CH4_YEAR <- Jan + Feb + Mar + Apr+ May+ Jun+ July+ Augu+ Sep+ Oct+ Nov+ Dec

AVG_CH4 <- CH4_YEAR/12

AVG_CH4 <- c(AVG_CH4)
LAT <- repmat(LAT_CH4,1,72)
LAT <- t(LAT)
LAT <- as.numeric(LAT)
LON = repmat(LON_CH4,27,1)
LON = c(LON)


CH4 <- cbind(LON, LAT, AVG_CH4)
CH4 <- as.data.frame(CH4)
write.csv(CH4, file = "CH4_AVG.csv", row.names=FALSE)

CH4 <-  subset(CH4, !is.na(AVG_CH4) & AVG_CH4>0)

crs <- projection(shp) ### get projections from shp file

CH4 <- SpatialPointsDataFrame(CH4[,1:2], CH4, proj4string=CRS(crs)) 
plot(CH4)


#### Points inside polygon (UK)
pts.poly <- over(CH4, shp[,"GADMID"])
CH4$id <- pts.poly$GADMID

# Aggregate by zone/polygon
###  Make a dataframe ###
data_points <- CH4@data

#### Sum of emission in UK polygon [ppm]
data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(CH4_sum = sum(AVG_CH4))  


CH4_sum_UK <- (data_points$CH4_sum[1]) ### ppbV
Surface_UK <- (shp@data$SQKM)*1e+10 ### from km2 to cm2
CH4_MASS <- 16.04 ### g/mol
N_Avogadro <- 6.022140857e+23


# Total_mass_grams <- (CH4_sum_UK * Surface_UK * CH4_MASS)/ N_Avogadro
# Total_mass_ktonnes <- Total_mass_grams/1000000000  #### converison in Ktonnes
# Total_mass_CO2_eq <-  Total_mass_ktonnes/25 


 
 