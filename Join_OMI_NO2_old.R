
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
Jan_2013_SAT <- read.csv("Jan_2013.csv", header = TRUE)
Jan_2013_SAT <- subset(Jan_2013_SAT, !is.na(JAN2013) & JAN2013>0)

Feb_2013_SAT <- read.csv("Feb_2013.csv", header = TRUE)
Feb_2013_SAT <- subset(Feb_2013_SAT, !is.na(FEB2013) & FEB2013>0)

Mar_2013_SAT <- read.csv("Mar_2013.csv", header = TRUE)
Mar_2013_SAT <- subset(Mar_2013_SAT, !is.na(MAR2013) & MAR2013>0)

Apr_2013_SAT <- read.csv("Apr_2013.csv", header = TRUE)
Apr_2013_SAT <- subset(Apr_2013_SAT, !is.na(APR2013) & APR2013>0)

May_2013_SAT <- read.csv("May_2013.csv", header = TRUE)
May_2013_SAT <- subset(May_2013_SAT, !is.na(MAY2013) & MAY2013>0)

June_2013_SAT <- read.csv("June_2013.csv", header = TRUE)
June_2013_SAT <- subset(June_2013_SAT, !is.na(JUN2013) & JUN2013>0)

July_2013_SAT <- read.csv("July_2013.csv", header = TRUE)
July_2013_SAT <- subset(July_2013_SAT, !is.na(JULY2013) & JULY2013>0)

August_2013_SAT <- read.csv("Aug_2013.csv", header = TRUE)
August_2013_SAT <- subset(August_2013_SAT, !is.na(AUG2013) & AUG2013>0)

Sept_2013_SAT <- read.csv("Sept_2013.csv", header = TRUE)
Sept_2013_SAT <- subset(Sept_2013_SAT, !is.na(SEPT2013) & SEPT2013>0)

October_2013_SAT <- read.csv("Oct_2013.csv", header = TRUE)
October_2013_SAT <- subset(October_2013_SAT, !is.na(OCT2013) & OCT2013>0)

November_2013_SAT <- read.csv("Nov_2013.csv", header = TRUE)
November_2013_SAT <- subset(November_2013_SAT, !is.na(NOV2013) & NOV2013>0)

December_2013_SAT <- read.csv("Dec_2013.csv", header = TRUE)
December_2013_SAT <- subset(December_2013_SAT, !is.na(DIC2013) & DIC2013>0)


### Interpolate data on the same grid 
#### Jan 2013 #######################

Jan_2013_SAT$x <- Jan_2013_SAT$X # define x & y as longitude and latitude
Jan_2013_SAT$y <- Jan_2013_SAT$Y

coordinates(Jan_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Jan_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Jan_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = JAN2013 ~ 1, locations = Jan_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "JAN2013")  # give names to the modelled variables

write.csv(idw.output, file = "Jan_2013_interp.csv", row.names=FALSE)

# ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = JAN2013))


### Interpolate data on the same grid 
#### Feb 2013 #######################

Feb_2013_SAT$x <- Feb_2013_SAT$X # define x & y as longitude and latitude
Feb_2013_SAT$y <- Feb_2013_SAT$Y

coordinates(Feb_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Feb_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Feb_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = FEB2013 ~ 1, locations = Feb_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "FEB2013")  # give names to the modelled variables

write.csv(idw.output, file = "Feb_2013_interp.csv", row.names=FALSE)


### Interpolate data on the same grid 
#### March 2013 #######################

Mar_2013_SAT$x <- Mar_2013_SAT$X # define x & y as longitude and latitude
Mar_2013_SAT$y <- Mar_2013_SAT$Y

coordinates(Mar_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Mar_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Mar_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = MAR2013 ~ 1, locations = Mar_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "MAR2013")  # give names to the modelled variables

write.csv(idw.output, file = "Mar_2013_interp.csv", row.names=FALSE)




### Interpolate data on the same grid 
#### April 2013 #######################

Apr_2013_SAT$x <- Apr_2013_SAT$X # define x & y as longitude and latitude
Apr_2013_SAT$y <- Apr_2013_SAT$Y

coordinates(Apr_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Apr_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Apr_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = APR2013 ~ 1, locations = Apr_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "APR2013")  # give names to the modelled variables

write.csv(idw.output, file = "Apr_2013_interp.csv", row.names=FALSE)



### Interpolate data on the same grid 
#### May 2013 #######################

May_2013_SAT$x <- May_2013_SAT$X # define x & y as longitude and latitude
May_2013_SAT$y <- May_2013_SAT$Y

coordinates(May_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(May_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(May_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = MAY2013 ~ 1, locations = May_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "MAY2013")  # give names to the modelled variables

write.csv(idw.output, file = "May_2013_interp.csv", row.names=FALSE)



### Interpolate data on the same grid 
#### June 2013 #######################

June_2013_SAT$x <- June_2013_SAT$X # define x & y as longitude and latitude
June_2013_SAT$y <- June_2013_SAT$Y

coordinates(June_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(June_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(June_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = JUN2013 ~ 1, locations = June_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "JUN2013")  # give names to the modelled variables

write.csv(idw.output, file = "June_2013_interp.csv", row.names=FALSE)




### Interpolate data on the same grid 
#### July 2013 #######################

July_2013_SAT$x <- July_2013_SAT$X # define x & y as longitude and latitude
July_2013_SAT$y <- July_2013_SAT$Y

coordinates(July_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(July_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(July_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = JULY2013 ~ 1, locations = July_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "JULY2013")  # give names to the modelled variables

write.csv(idw.output, file = "July_2013_interp.csv", row.names=FALSE)




### Interpolate data on the same grid 
#### August 2013 #######################

August_2013_SAT$x <- August_2013_SAT$X # define x & y as longitude and latitude
August_2013_SAT$y <- August_2013_SAT$Y

coordinates(August_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(August_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(August_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = AUG2013 ~ 1, locations = August_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "AUG2013")  # give names to the modelled variables

write.csv(idw.output, file = "August_2013_interp.csv", row.names=FALSE)



### Interpolate data on the same grid 
#### September 2013 #######################

Sept_2013_SAT$x <- Sept_2013_SAT$X # define x & y as longitude and latitude
Sept_2013_SAT$y <- Sept_2013_SAT$Y

coordinates(Sept_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Sept_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Sept_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = SEPT2013 ~ 1, locations = Sept_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "SEPT2013")  # give names to the modelled variables

write.csv(idw.output, file = "Sept_2013_interp.csv", row.names=FALSE)




### Interpolate data on the same grid 
#### October 2013 #######################

October_2013_SAT$x <- October_2013_SAT$X # define x & y as longitude and latitude
October_2013_SAT$y <- October_2013_SAT$Y

coordinates(October_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(October_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(October_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = OCT2013 ~ 1, locations = October_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "OCT2013")  # give names to the modelled variables

write.csv(idw.output, file = "October_2013_interp.csv", row.names=FALSE)




### Interpolate data on the same grid 
#### November 2013 #######################

November_2013_SAT$x <- November_2013_SAT$X # define x & y as longitude and latitude
November_2013_SAT$y <- November_2013_SAT$Y

coordinates(November_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(November_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(November_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = NOV2013 ~ 1, locations = November_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "NOV2013")  # give names to the modelled variables

write.csv(idw.output, file = "November_2013_interp.csv", row.names=FALSE)




### Interpolate data on the same grid 
#### December 2013 #######################

December_2013_SAT$x <- December_2013_SAT$X # define x & y as longitude and latitude
December_2013_SAT$y <- December_2013_SAT$Y

coordinates(December_2013_SAT) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(December_2013_SAT)

x.range <- as.numeric(c(-10.34, 3.22))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(49.05, 61.84))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(December_2013_SAT, pch = 1, col = "red", cex = 1)

idw <- idw(formula = DIC2013 ~ 1, locations = December_2013_SAT, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "DIC2013")  # give names to the modelled variables

write.csv(idw.output, file = "December_2013_interp.csv", row.names=FALSE)


############################################################################
###### Average data data by column (each year year) #######################################

Jan_2013_SAT <- read.csv("Jan_2013_interp.csv", header = TRUE) [,1:3]
Feb_2013_SAT <- read.csv("Feb_2013_interp.csv", header = TRUE) [,1:3]
Mar_2013_SAT <- read.csv("Mar_2013_interp.csv", header = TRUE) [,1:3]
Apr_2013_SAT <- read.csv("Apr_2013_interp.csv", header = TRUE) [,1:3]
May_2013_SAT <- read.csv("May_2013_interp.csv", header = TRUE) [,1:3]
June_2013_SAT <- read.csv("June_2013_interp.csv", header = TRUE) [,1:3]
July_2013_SAT <- read.csv("July_2013_interp.csv", header = TRUE) [,1:3]
August_2013_SAT <- read.csv("August_2013_interp.csv", header = TRUE) [,1:3]
Sept_2013_SAT <- read.csv("Sept_2013_interp.csv", header = TRUE) [,1:3]
October2013_SAT <- read.csv("October_2013_interp.csv", header = TRUE) [,1:3]
November_2013_SAT <- read.csv("November_2013_interp.csv", header = TRUE) [,1:3]
December_2013_SAT <- read.csv("December_2013_interp.csv", header = TRUE) [,1:3]

NO2_2013_SAT <- cbind(Jan_2013_SAT, Feb_2013_SAT[,3], Mar_2013_SAT[,3],
                      Apr_2013_SAT[,3], May_2013_SAT[,3], June_2013_SAT[,3],
                      July_2013_SAT[,3], August_2013_SAT[,3], Sept_2013_SAT[,3],
                      October2013_SAT[,3], November_2013_SAT[,3], December_2013_SAT[,3])
colnames(NO2_2013_SAT) <- c("Lon", "Lat", "Jan_2013", "Feb_2013", "Mar_2013",
                            "Apr_2013", "May_2013", "June_2013", "July_2013",
                            "Augu_2013", "Sept_2013", "Oct_2013", "Nov_2013", "Dec_2013")

AAA <- NO2_2013_SAT[,3:14]
AVG <- rowMeans(NO2_2013_SAT[3:14], na.rm=FALSE)
# SUM <- rowSums(NO2_2013_SAT[3:14], na.rm=FALSE)
AVG_2013 <- cbind(Jan_2013_SAT[,1:2],AVG)


#### define projection for Satellite dataframe data ##############

crs <- projection(shp) ### get projections from shp file

AVG_2013 <- SpatialPointsDataFrame(AVG_2013[,1:2], AVG_2013, 
                                      proj4string=CRS(crs)) 

# plot(shp)
# points(AVG_2013, pch=10)

#### Points into inside polygon (UK)
pts.poly <- over(AVG_2013, shp[,"GADMID"])
AVG_2013$id <- pts.poly$GADMID

# Aggregate by zone/polygon
###  Make a dataframe ###
data_points <- AVG_2013@data

#### Sum of emission in UK polygon [molecules/cm2]
data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(NO2_sum = sum(AVG))


NO2_sum_UK <- (data_points$NO2_sum[1])*1e+13 ### molecules/cm2
Surface_UK <- (shp@data$SQKM)*1e+10 ### from km2 to cm2
NO2_MASS <- 46.055 ### g/mol
N_Avogadro <- 6.022140857e+23
Total_mass_grams <- (NO2_sum_UK * Surface_UK * NO2_MASS)/ N_Avogadro
Total_mass_ktonnes <- Total_mass_grams/1000000000


