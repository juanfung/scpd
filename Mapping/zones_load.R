## Pre-processing for zones*.R files

## Packages for data wrangling
library(data.table)
library(sqldf)
library(plyr)
library(dplyr)

## Packages for GIS
library(rgdal)
library(rgeos)
library(maptools)

## --------------------------------------------------------------------------- #

## Path to data; change as necessary
path.to.data <- "../Data/"
path.to.code <- "../Functions/"

## --------------------------------------------------------------------------- #

## load functions 
source(paste0(path.to.code,"makeShapes.R")) # for making polygons

source(paste0(path.to.code,"mungefun.R")) # misc functions

## --------------------------------------------------------------------------- #

## Construct school district boundaries from shapefiles

## --------------------------------------------------------------------------- #

## --------------------------------------------------------------------------- #
## 1. Get coordinates for all road intersections

## (option a)
## In R: (not run)
## ildot <- importShp(paste0(path.to.data,"Shapefiles/ILDOT/2010"),"HWY2010",layer=T)
## coords <- coordinates(gIntersection(ildot,ildot))
## ildot <- SpatialPoints(coords)

## save(ildot,file = paste0(path.to.data,"Maps/hwy_lc.Rda"))

## --------------------------------------------------------------------------- #

## (option b)
## pre-processed in QGIS; see ... for details

## save(ildot,file = paste0(path.to.data,"Maps/hwy_lc.Rda"))

## --------------------------------------------------------------------------- #

## Once processed:
## load(paste0(path.to.data,"Maps/hwy_lc.Rda"),verbose=TRUE)

## --------------------------------------------------------------------------- #

## 2. Create shapefiles based on road intersections
## see zones*.R files

## 3. Combine with school district shapefiles
## see "sdboundaries.R" for details

## Path to Census shapefiles
census.dir <- paste0(path.to.data,"Census/")
shp.dir <- paste0(census.dir,"Shapefiles/2010/")

shp.files <- list.files(shp.dir)
shp.files <- shp.files[grep("\\.shp$",shp.files)]

## Load school district shapefiles

## load(paste0(path.to.data,"Maps/unsd.Rda"),verbose=T)
## load(paste0(path.to.data,"Maps/roads.Rda"),verbose=T)

## change projection to match usd116 Census shapefile...
## ildot <-
