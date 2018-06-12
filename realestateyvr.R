#realestateyvr.r
library(shiny)
library(sf)
library(sp)
library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(readr)
library(plyr)
library(ggplot2)
library(dplyr)
library(utils)
library(leaflet)
library(raster)
library(reshape2)
library(data.table)
library(foreign)

#Set Working Directory
getwd()


#2016 - Application

#Read CT Shapefile for the whole of Canada
CensusTract.shape2016 <- readOGR(dsn="./data/lct_000b16a_e", layer = "lct_000b16a_e")

#Grab just Vancouver's CTs
CensusTractVan16 <- CensusTract.shape2016[CensusTract.shape2016$CMANAME == "Vancouver",]

#Project shapefile into WGS
CensusTractVan16@proj4string
CensusTractVan16wgs <- spTransform(CensusTractVan16,"+proj=longlat +datum=WGS84 +no_defs")
CTVan16 <- CensusTractVan16wgs
CTVan16@proj4string

#Clean attribute data
CensusTractVan16df <-read.table("./data/CensusTractVan2016df0.csv", sep=",", dec = ".", stringsAsFactors = FALSE)
CensusTractVan16dfsemiclean <- CensusTractVan16df[,-c(1:9)]
names(CensusTractVan16dfsemiclean) <- c("geo_uid",	"ct_name",	"Average Rent",	"Average Dwelling Cost",	"Median Total Income in Private Households",	"Average Income per Month","Percentage of Income Spent on Rent",	"Years Needed to Save to Pay for Downpayment",	"Number of Avocados Equivalent to Average Rent")
CensusTractVan16dfclean<- CensusTractVan16dfsemiclean[-c(1),-c(9)] 
CensusTractVan16dfclean[,c(3:8)] <- lapply(CensusTractVan16dfclean[,c(3:8)], as.numeric)


#join attribute data to boundaries
CTVan16@data <- data.frame(CTVan16@data, CensusTractVan16dfclean[match(CTVan16@data[,"CTUID"], CensusTractVan16dfclean[,"geo_uid"]),])
names(CTVan16@data)
summary(CTVan16@data)

#Set data from factor to numeric data
CTVan16@data[,c(11:16)] <- lapply(CTVan16@data[,c(11:16)], as.numeric)
names(CTVan16@data)

#2011


#Read CT Shapefile for the whole of Canada
CensusTract.shape2011 <- readOGR(dsn="./data/gct_000b11a_e", layer="gct_000b11a_e")

#Grab just Vancouver's CTs
CensusTractVan11 <- CensusTract.shape2011[CensusTract.shape2011$CMANAME == "Vancouver",]

#Project shapefile into WGS
CensusTractVan11@proj4string
CensusTractVan11wgs <- spTransform(CensusTractVan11,"+proj=longlat +datum=WGS84 +no_defs")
CTVan11 <- CensusTractVan11wgs
CTVan11@proj4string

#Clean attribute data
CensusTractVan11df <-read.table("./data/2011_housing_data.csv", sep=",", dec = ".", stringsAsFactors = FALSE)
names(CensusTractVan11df) <- c("geo_uid",	"ct_name", "Average Dwelling Cost", "Average Rent",	"Average Income of Individuals",	"Average Income per Month","Percentage of Income Spent on Rent",	"Years Needed to Save",	"Number of Avocados Equivalent to Average Rent")
CensusTractVan11dfsemiclean <-  CensusTractVan11df[-c(1),-c(273)]
names(CensusTractVan11df)
CensusTractVan11dfsemiclean <- CensusTractVan11dfsemiclean[,-c(9)]

#Set Factor into Numeric Data
CensusTractVan11dfsemiclean[,c(3:8)] <- lapply(CensusTractVan11dfsemiclean[,c(3:8)], as.numeric)

#join attribute data to boundaries
CTVan11@data <- data.frame(CTVan11@data, CensusTractVan11dfsemiclean[match(CTVan11@data[,"CTUID"], CensusTractVan11dfsemiclean[,"geo_uid"]),])
names(CTVan11@data)
summary(CTVan11@data)

#2006

#Read CT Shapefile for the whole of Canada
CensusTract.shape2006 <- readOGR(dsn="./data/gct_000b06a_e", layer= "gct_000b06a_e")

#Grab just Vancouver's CTs
CensusTractVan06 <- CensusTract.shape2006[CensusTract.shape2006$CMAUID == "933",]

#Project shapefile into WGS
CensusTractVan06@proj4string
plot(CensusTractVan06)
CensusTractVan06wgs <- spTransform(CensusTractVan06,"+proj=longlat +datum=WGS84 +no_defs")
CTVan06 <- CensusTractVan06wgs
CTVan06@proj4string

#Clean attribute data
CensusTractVan06df <-read.table("./data/2006_housing_data.csv", sep=",", dec = ".", stringsAsFactors = FALSE)
names(CensusTractVan06df) <- c("geo_uid",	"ct_name", "Average Income", "Average Rent",	"Average Dwelling Cost",	"Years Needed to Save","Average Income per Month",	"Percentage of Income Spent on Rent",	"Number of Avocados Equivalent to Average Rent")
CensusTractVan06dfsemiclean <- CensusTractVan06df[-c(1),]
CensusTractVan06dfclean<- CensusTractVan06dfsemiclean[,-c(9) ] 
summary(CensusTractVan06dfclean)

#Set Factor into Numeric Data
CensusTractVan06dfclean[,c(3:8)] <- lapply(CensusTractVan06dfclean[,c(3:8)], as.numeric)


#join attribute data to boundaries
CTVan06@data <- data.frame(CTVan06@data, CensusTractVan06dfclean[match(CTVan06@data[,"CTUID"], CensusTractVan06dfclean[,"geo_uid"]),])









