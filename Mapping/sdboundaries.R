# Packages for data wrangling
library(LaF)
library(data.table)
library(sqldf)
#library(plyr)
library(dplyr)

# Packages for GIS
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggmap)
library(PBSmapping)
library(spatstat)
library(McSpatial)

# --------------------------------------------------------------------------- #

# Working path
home.dir <- "/home/juanfung/Dropbox/"

# Path to data; change as necessary
path.to.data <- paste0(home.dir,"Data")
path.to.code <- paste0(home.dir,"Projects/School choice/Code")

# --------------------------------------------------------------------------- #

# load functions #
source(paste0(path.to.code,"/mungefun.R"))

# --------------------------------------------------------------------------- #

# Construct school district boundaries from shapefiles

# --------------------------------------------------------------------------- #

# Path to Census shapefiles
census.dir <- paste0(path.to.data,"/Census")
shp.dir <- paste0(census.dir,"/TIGER Shapefiles/2010 Tiger Shapefiles")

shp.files <- list.files(shp.dir)
shp.files <- shp.files[grep("\\.shp$",shp.files)]

# --------------------------------------------------------------------------- #
importShp <- function(dir,fname) {
    # directory: path to shapefiles
    # fname: keyword or full name of desired shapefile
    require(rgdal)
    shp.files <- list.files(dir)
    shp.files <- shp.files[grep("\\.shp$",shp.files)]
    layer <- shp.files[grep(fname,shp.files)]
    layer <- substr(layer,1,nchar(layer)-4)
    shp <- readOGR(shp.dir,layer)
    return(shp)
}

# CENSUS 2010 #
# 1a. census tracts 2010 - champaign county
tract10 <- importShp(shp.dir,"17019_tract10")
# tract10.geom <- fortify(tract10,region="GEOID10")

# 1b. census blocks 2010 - champaign county
block10 <- importShp(shp.dir,"17019_tabblock10")

# 2. unified school districts 2010 - IL
unsd10 <- importShp(shp.dir,"unsd10")

# 3. elementary school districts 2010 - IL
elsd10 <- importShp(shp.dir,"elsd10")

# 4. secondary school districts 2010 - IL
slsd10 <- importShp(shp.dir,"slsd10")

# 5. Roads, edges, and feature names
roads10 <- importShp(shp.dir,"17019_roads")
# [+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0]

# --------------------------------------------------------------------------- #
# 5'. includes all linear features: roads, railroads, hydrography...
# edges10 <- importShp(shp.dir,"17019_edges")
# --------------------------------------------------------------------------- #

# 6. Subset
# Using sd.codes - N.B.: is projected=FALSE for both objects
# Import champaign county boundary codes
sd.codes.path <- paste(census.dir,"2010 Census School District Reference Maps",sep="/")
sd.codes <- list.files(sd.codes.path,pattern="\\.txt",full.names=TRUE)
sd.codes <- read.table(sd.codes,header=TRUE,sep=";",colClasses=c("factor","factor","character","integer"),stringsAsFactors=FALSE)
sd.geoid <- as.factor(paste("17",sd.codes$CODE,sep=""))
sd.geoid.cu <- sd.geoid[grep("09420|39960",sd.geoid)] # champaign|urbana

# 6b. Subset unified school districts to CUSD4 and USD116 #
unsd10 <- unsd10[unsd10$GEOID10 %in% sd.geoid.cu,] 

# Overlay roads and school districts #
roads10df <- roads10@data # Save road attributes
#unsd10 <- gBuffer(spgeom=unsd10, width=1)
roads <- gIntersection(roads10,unsd10) # This loses road attributes (e.g., names)
roads <- SpatialLinesDataFrame(roads10,roads10df) # Comine roads subset with attributes

#-------------- ALTERNATIVE -------------#
roads10df <- roads10@data # Save road attributes
roads.ps <- SpatialLines2PolySet(roads10)
roads <- bbox(unsd10)
roads <- clipLines(roads.ps,xlim=roads[1,],ylim=roads[2,],keepExtra=FALSE)
roads <- PolySet2SpatialLines(roads)
roads <- spTransform(roads, CRSobj = CRS(proj4string(unsd10))) #re-project!
roads <- SpatialLinesDataFrame(roads,roads10df)

# --------------------------------------------------------------------------- #
# 6c. Subset roads within USD 116; gIntersection loses attributes; over fails #
# Save road attributes
roads10df <- roads@data
# Convert roads to PolySet for clipping
roads.ps <- SpatialLines2PolySet(roads)

# Subset USD116, label by district
usd116 <- unsd10[grep("1739960",unsd10$GEOID10),] 
rusd116 <- bbox(usd116)
rusd116 <- clipLines(roads.ps,xlim=rusd116[1,],ylim=rusd116[2,],keepExtra=FALSE)
rusd116 <- PolySet2SpatialLines(rusd116)
rusd116 <- spTransform(rusd116, CRSobj = CRS(proj4string(usd116))) # re-project!
rusd.df <- data.frame(roads10df,"GEOID10"=rep(usd116$GEOID10,nrow(roads10df)))
rusd116 <- SpatialLinesDataFrame(rusd116,rusd.df)

# Subset CUSD4, label by district
cusd4 <- unsd10[grep("1709420",unsd10$GEOID10),] 
rcusd4 <- bbox(cusd4)
rcusd4 <- clipLines(roads.ps,xlim=rcusd4[1,],ylim=rcusd4[2,],keepExtra=FALSE)
rcusd4 <- PolySet2SpatialLines(rcusd4)
rcusd4 <- spTransform(rcusd4, CRSobj = CRS(proj4string(cusd4)))
rusd.df <- data.frame(roads10df,"GEOID10"=rep(cusd4$GEOID10,nrow(roads10df)))
rcusd4 <- SpatialLinesDataFrame(rcusd4,rusd.df)

# Assign unique row.names, spRbind
row.names(rusd116) <- paste(row.names(rusd116),rusd116$GEOID10,sep=".")
row.names(rcusd4) <- paste(row.names(rcusd4),rcusd4$GEOID10,sep=".")
rcu <- spRbind(rcusd4,rusd116)

# Save!
save(roads,rcusd4,rusd116,rcu,file=paste0(path.to.data,"/Dataquick/Maps/roads.Rda"))
save(cusd4,usd116,unsd10,file=paste0(path.to.data,"/Dataquick/Maps/unsd.Rda"))

# --------------------------------------------------------------------------- #
# Explore
plot(gDifference(rusd116,as(usd116,"SpatialLines")))
plot(gSymdifference(rusd116,as(usd116,"SpatialLines")))
plot(gDifference(as(usd116,"SpatialLines"),rusd116))
plot(gIntersection(rusd116,as(usd116,"SpatialLines")))

# --------------------------------------------------------------------------- #
# Alternative (not run): overlay roads with census tracts, subset by municipality
# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# VII. Compute distances to schools using lon/lat points; see merge.R

# load "basic." files; see process_isbe.R for details
# broken up as follows
# basic: 2001-2012
# basic.rc.2000: 2000
# basic.98: 1998-1999
# basic.rc.1997: 1997
# load(paste0(path.to.data,"/Dataquick/rc_school_level.Rda"),verbose=T)

# Subset: Not run #
#dt.ha.1 <- dt.ha.1[grep("champaign|urbana",Agency.Name,ignore.case=T),]


load(paste0(path.to.data,"/Dataquick/Backups/dt_ha1.Rda"),verbose=T)
load(paste0(path.to.data,"/Dataquick/rc_school_level_COUNTY.Rda"),verbose=T)
load(paste0(path.to.data,"/Dataquick/dt_schools.Rda"),verbose=T)

load(paste0(path.to.data,"/Dataquick/Maps/unsd.Rda"),verbose=T)
load(paste0(path.to.data,"/Dataquick/Maps/roads.Rda"),verbose=T)

# Create SpatialPointsDataFrame from school coordinates
xy.schools <- dt.schools[grep("champaign|urbana",Agency.Name,ignore.case=T),]
xy.schools <- xy.schools[!is.na(School.Latitude),]
xy.schools <- SpatialPointsDataFrame(xy.schools[,.(School.Longitude,School.Latitude)],xy.schools[,.(School.Name,Agency.Name,Year)])
proj4string(xy.schools) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") 
xy.schools <- spTransform(xy.schools,CRSobj=CRS(proj4string(unsd10)))

# Convert spatial polygons to spatial points
usd116.lines <- as(usd116,"SpatialLines")

# --------------------------------------------------------------------------- #
# Use IL DOT highway, railroad, structure (e.g., overpass) shapefiles
ildot <- importShp(paste0(path.to.data,"/Champaign-Urbana/ILDOT/2010"),"hwy_lc",layer=T)
names(ildot) <- c("ROAD.1","ROAD.2")

ildot <- spTransform(ildot, CRSobj = CRS(proj4string(roads))) # re-project

# example: I-74 and US-45
plot(ildot[grep("74|45",ildot$MARKED_RT,ignore.case=T),])

## Find coordinates of road intersections
# coords <- coordinates(gIntersection(ildot[grep("74",ildot$MARKED_RT),],ildot[grep("45",ildot$MARKED_RT),]))

# Test:

# ?-2002 (see "current_district_072001.pdf")

# 2002-2012 (see "Scanned from a Xerox*.pdf")

# Wiley:
# Windsor and Philo; Windsor and Race; Curits and Philo; Curtis and Race
wiley.1 <- spRbind(ildot[(ildot$ROAD.1 %in% c("WINDSOR RD") & ildot$ROAD.2 %in% c("PHILO RD","RACE ST")),],ildot[(ildot$ROAD.1 %in% c("PHILO RD") & ildot$ROAD.2 %in% c("CURTIS RD")),])
w.wiley <- spRbind(w.wiley,ildot[(ildot$ROAD.2 %in% c("1350 E") & ildot$ROAD.1 %in% c("CURTIS RD")),])

# ALTERNATIVE - only extract coordinates
wiley.2 <- rbind(w1,w2,w3)
plot(SpatialPoints(wiley.2,CRS(proj4string(ildot))))

# Create Line
# Combine List of (Line)-objects into Lines-object
# Convert to Polygon-object
# Combine List of (Polygon)-objects into Polygons-object
# Convert to SpatialPolygonsDataFrame

# EXAMPLE #
points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
         Polygon(coordinates(df[df$id==ID & df$group==group,]))
         }
  get.spPoly  <- function(ID,df) {
         Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
         }
  spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
  SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
}
spDF <- points2polygons(df,data)
plot(spDF,col=spDF$box_id+1)

library(rgdal)
writeOGR(spDF,dsn=".",layer="myShapefile", driver="ESRI Shapefile")
# END EXAMPLE #

wiley.1.data <- wiley.1@data
wiley.1l <- as(wiley.1,"SpatialPolygons")
wiley.1p <- Polygons(list(wiley.1p),ID="WILEY")
wiley.1p <- SpatialPolygons(list(wiley.1p))

# Label

# Montclair and Race; Florida and Race; Florida and Vine; Colorado and Vine; Washington and Vine; Waghington and Philo; Fairlawn and Philo; Colorado and Philo

# 2013 (see "Urbana 2013*.pdf")


# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# One option: Euclidean distance

# More options: package sp has functions
# a. create neighborhood around grid obj: 
# gridIndex2nb(obj, maxdist = sqrt(2), fullMat = TRUE, ...)
# b. create spatial lines data frame:
# SpatialLines()
# c. find point pairs with equal spatial coordinates:
# zerodist()
# d. Euclidean or great circle distance in km between set of points and another point(s):
# spDistsN1(pts, pt, longlat = FALSE) # FALSE is Euclidean
# spDistsN1(pts=as.matrix(dt.ha.1[x,.(Lon.00,Lat.00)]),pt=as.matrix(dt.ha.1[y[1],.(Lon.00,Lat.00)]),longlat=TRUE) # TRUE is great circle in km
# spDists(x, y = x, longlat = FALSE) # computes matrix

# library(McSpatial)
# e. Great circle distance in miles
# geoshape(longvar=dt.ha.1[x,Lon.00],latvar=dt.ha.1[x,Lat.00],coormatrix=as.matrix(dt.ha.1[y[1],.(Lon.00,Lat.00)]))*1.60934

# VIIa. Step 1: closest school by year, by district
# Champaign: closest within 1.5 miles in general, or closest school if none within 1.5 miles
# ALT: for each school, define 1.5 mile radius

# CONVERSIONS #
# 1 mi = 1.60934 km #
# 1 km = 0.621371 mi #

closestSchool <- function(xid,x1,x2,xyear,xsd,y) {
    # x=(x1,x2)=(long,lat): source coordinates
    # xyear: year for observation x
    # xsd: school district for observation x
    # y: data.frame w/ (target name, target year, target sd, target coordinates)
    # for all schools in xsd in xyear, with School.Name, School.Distance
    #require(data.table)
    #y <- data.table(y)
    z <- y[Year==xyear&Agency.Name==xsd,
           .(School.Name,School.Longitude,School.Latitude)]
    z <- z[!is.na(School.Longitude),]
    x <- c(x1,x2)
    z$School.Distance <- spDistsN1(
        pts=as.matrix(z[,.(School.Longitude,School.Latitude)]),
        pt=x,longlat=TRUE)*0.621371

    #z <- z[which.min(z$School.Distance),list(School.Name,School.Distance)]
        # # 1.5mi = 2.414km: which(z$School.Distance<=2.414)
    z <- cbind("ID"=xid,
                   "Year"=xyear,
                   z[which.min(z$School.Distance),.(School.Name,School.Distance)])

    return(z)
}

dt.ha.1[Agency.Name=="CHAMPAIGN CUSD 4",`:=`(
            Closest.School.Name=closestSchool(
                x1=Lon.00,
                x2=Lat.00,
                xyear=Year,
                xsd=Agency.Name,
                y=dt.schools)$School.Name,
            Closest.School.Distance=closestSchool(
                x1=Lon.00,
                x2=Lat.00,
                xyear=Year,
                xsd=Agency.Name,
                y=dt.schools)$School.Distance)]

y <- basic[Agency.Name=="CHAMPAIGN CUSD 4" & Year=="2012",
           list(School.Name,School.Longitude,School.Latitude)]
y <- y[grep("ELEM",School.Name),]
x <- dqlar[,sample(grep("CHAMPAIGN",Agency.Name),5)]

dqlar[x[1],`:=`(School.ID=closestSchool(x1=Lon.00,
                    x2=Lat.00,
                    xyear="2012",
                    xsd="CHAMPAIGN CUSD 4",
                    y=y)$School.ID,
                School.Name=closestSchool(
                    x1=Lon.00,
                    x2=Lat.00,
                    xyear="2012",
                    xsd="CHAMPAIGN CUSD 4",
                    y=y)$School.Name,
                School.Distance=closestSchool(
                    x1=Lon.00,
                    x2=Lat.00,
                    xyear="2012",
                    xsd="CHAMPAIGN CUSD 4",
                    y=y)$School.Distance)]
# --------------------------------------------------------------------------- #
# Duplicate?
closestSchool <- function(x1,x2,xyear,xsd,y) {
    # x=(x_1,x_2)=(long,lat): source coordinates
    # xyear: year for observation x
    # xsd: school district for observation x
    # y: data.table with target coordinates (y_{i1},y_{i2})=(long,lat)
    # for all schools in xsd in xyear, with School.ID and School.Name
    z <- y[Year==xyear&District:=xsd,
           list(School.ID,School.Name,School.Longitude,School.Latitude)]
    x <- c(x1,x2)
    z$School.Distance <- spDistsN1(
        pts=as.matrix(z[,list(School.Longitude,School.Latitude)]),
        pt=x,
        longlat=TRUE)
    z <- z[which.min(z$School.Distance),
           list(School.ID,
                School.Name,
                School.Distance)] # 1.5mi = 2.414km: which(z$School.Distance<=2.414)
    # z <- cbind(x,z[which.min(z$Distance)])
    return(z)
}

y <- basic[Agency.Name=="CHAMPAIGN CUSD 4" & Year=="2012",
           list(School.Name,School.Longitude,School.Latitude)]
y <- y[grep("ELEM",School.Name),]
x <- dqlar[,sample(grep("CHAMPAIGN",Agency.Name),5)]

dqlar[x[1],`:=`(School.ID=closestSchool(x1=Lon.00,
                    x2=Lat.00,
                    xyear="2012",
                    xsd="CHAMPAIGN CUSD 4",
                    y=y)$School.ID,
                School.Name=closestSchool(
                    x1=Lon.00,
                    x2=Lat.00,
                    xyear="2012",
                    xsd="CHAMPAIGN CUSD 4",
                    y=y)$School.Name,
                School.Distance=closestSchool(
                    x1=Lon.00,
                    x2=Lat.00,
                    xyear="2012",
                    xsd="CHAMPAIGN CUSD 4",
                    y=y)$School.Distance)]

# --------------------------------------------------------------------------- #
# better to return data.table for merge? 

closestSchool <- function(xx,yy) {
    # xx: source with year, agency, ID, lon/lat
    # yy: target with year, agency, school, lon/lat
    # Returns (ID,Year,School.District,School.Name,School.Distance in miles) 
    require(sp)
    require(data.table)
    # check is.data.table(xx)
    # check is.data.table(yy)    
    xxx <- NULL
    if (nrow(xx)>0) {
        zz <- spDists(x=as.matrix(xx[,.(Lon.00,Lat.00)]),
                      y=as.matrix(yy[,.(School.Longitude,School.Latitude)]),
                      longlat=TRUE)*0.621371 # conversion from km to miles
        #dis <- do.call(pmin, lapply(1:ncol(zz), function(i) zz[,i]))
        dis <- sapply(1:nrow(zz),function(i) dis <- zz[i,which.min(zz[i,])] )
        school <- yy[sapply(1:nrow(zz),function(i) which.min(zz[i,])),.(School.Name,Agency.Name)]
        xxx <- rbind(xxx,cbind(xx,
                               School.Name=school$School.Name,
                               School.Distance=dis,
                               School.District=school$Agency.Name))
    #
    } else {
        xxx <- cbind(xx,School.Name=NA,School.Distance=NA,School.District=NA)
    }
    return(xxx)
}

dt.ha.1[,My.ID:=seq_len(nrow(dt.ha.1))]
XX <- dt.ha.1[!is.na(Lon.00),.(My.ID,SR_PROPERTY_ID,Year,Agency.Name,Lon.00,Lat.00)]
YY <- dt.schools[!is.na(School.Latitude),]
YY <- YY[grep("\\b(|CU|CC)SD\\b",Agency.Name),]

# dq.schools <- closestSchool(XX,YY)

# Apply by district, by year; for 2013,2014 - use 2012 #
dq.schools <- NULL
for (agency in YY[,unique(Agency.Name)]) {
    for (yr in c(1996:2014)) {
        if (yr < 2013) {
            if (nrow(XX[Year==yr&Agency.Name==agency,]) == 0) next
            if (nrow(YY[Year==yr&Agency.Name==agency,]) == 0) next
            dq.schools <- rbind(dq.schools,
                                closestSchool(
                                    XX[Year==yr&Agency.Name==agency,],
                                    YY[Year==as.character(yr)&Agency.Name==agency,]),
                                use.names=TRUE)
        } else {
            if (nrow(XX[Year==yr&Agency.Name==agency,]) == 0) next
            if (nrow(YY[Year==as.character(2012)&Agency.Name==agency,]) == 0) next
            dq.schools <- rbind(dq.schools,
                                closestSchool(
                                    XX[Year==yr&Agency.Name==agency,],
                                    YY[Year==as.character(2012)&Agency.Name==agency,]),
                                use.names=TRUE)
        }
    }
}

# What to do about the rest? Some districts NA, some districts not in CCD #
agency <- setdiff(XX[!is.na(Agency.Name),unique(Agency.Name)],YY[!is.na(Agency.Name),unique(Agency.Name)])
XX.a <- XX[Agency.Name %in% agency,]
XX.a <- rbind(XX.a,XX[is.na(Agency.Name),],use.names=TRUE)

for (yr in c(1996:2014)) {
    if (yr < 2013) {
        if (nrow(XX.a[Year==yr,]) == 0) next
        if (nrow(YY[Year==yr,]) == 0) next
        dq.schools <- rbind(dq.schools,
                            closestSchool(
                                XX.a[Year==yr,],
                                YY[Year==as.character(yr),]),
                            use.names=TRUE)
    } else {
        if (nrow(XX.a[Year==yr,]) == 0) next
        if (nrow(YY[Year==as.character(2012),]) == 0) next
        dq.schools <- rbind(dq.schools,
                            closestSchool(
                                XX.a[Year==yr,],
                                YY[Year==as.character(2012),]),
                            use.names=TRUE)
    }
}

# Flag for school district match
dq.schools[Agency.Name %in% YY[,unique(Agency.Name)],School.District.Match:=1L]
dq.schools[!(Agency.Name %in% YY[,unique(Agency.Name)]),School.District.Match:=0L]

save(dq.schools,file=paste0(path.to.data,"/Dataquick/Backups/closestSchools.Rda"))

dt.ha.1 <- merge(dt.ha.1,dq.schools[,.(School.Name,School.Distance)],by=c("My.ID"),all.x=TRUE)


#zz <- geoshape(longvar=xx[,Lon.00],latvar=[,Lat.00],coormatrix=as.matrix(yy[,School.Longitude,School.Latitude]))


# --------------------------------------------------------------------------- #
# MISC:
# Making school district boundaries; see usd116zones.R                 
# School district shapefile
# Path to Census shapefiles
census.dir <- paste0(path.to.data,"Census/")
shp.dir <- paste0(census.dir,"TIGER Shapefiles/2010 Tiger Shapefiles/")

shp.files <- list.files(shp.dir)
shp.files <- shp.files[grep("\\.shp$",shp.files)]

load(paste0(path.to.data,"Dataquick/Maps/unsd.Rda"),verbose=T)
load(paste0(path.to.data,"Dataquick/Maps/roads.Rda"),verbose=T)

# --------------------------------------------------------------------------- #
# Use IL DOT highway shapefiles to create attendance zone shapefile

# 1. Get coordinates for all road intersections

# In R: (not run)
# ildot <- importShp(paste0(path.to.data,"/Champaign-Urbana/ILDOT/2010"),"HWY2010",layer=T)
# coords <- coordinates(gIntersection(ildot,ildot))

# Done in QGIS: hwy_lc.shp
ildot <- importShp(paste0(path.to.data,"Champaign-Urbana/ILDOT/2010/"),"hwy_lc",layer=T)
names(ildot) <- c("ROAD.1","ROAD.2")

ildot <- spTransform(ildot, CRSobj = CRS(proj4string(roads))) # re-project wrt district shapefiles

save(ildot,file=paste0(path.to.data,"/Dataquick/Maps/hwy_lc.Rda"))

# railroads
#rrx <- importShp(paste0(path.to.data,"Champaign-Urbana/ILDOT/2010/"),"RRX2010",layer=T)
#rrx <- spTransform(rrx,CRSobj = CRS(proj4string(ildot)))
#ildot.rr <- intersect(coordinates(ildot),coordinates(rrx))

# --------------------------------------------------------------------------- #
# scratch: functions for makeShapes.R
makeLine <- function(pts,names) {
    # pts: a matrix of ordered coordinates
    require(sp)
    
    n <- nrow(pts)
    lines <- list()
    for (i in 2:n) {
        lines[i-1] <- Line(rbind(pts[i,],pts[i-1,]))
    }
    lines[i] <- Line(rbind(pts[i,],pts[1,]))
    lines <- setNames(lines,names)
    return(lines)
}

# connect ordered lines, return SpatialLinesDataFrame object
makePolygon <- function(lines,poly.id,proj) {
    # lines: a list of Line-objects
    # poly.id :  character name to ID polygon
    
    require(sp)
    
    poly <- Lines(lines,ID=poly.id)
    poly <- SpatialLines(list(poly),CRS(proj))
    poly.data <- data.frame(Agency.Name="USD 116",
                            School.Name=gsub("(.*)\\.\\d+$","\\1",poly.id),
                            row.names=poly.id)
    poly <- SpatialLinesDataFrame(poly,data=poly.data)
    return(poly)
}

# Clipping and joinging polygons

# Convert to polygons and dissolve: doesn't work on line2poly object; need intial polygon
#zones2001 <- line2poly(zones2001)
#zones2001 <- unionSpatialPolygons(zones2001,IDs=row.names(zones2001))

# Generate IDs for grouping
zones <- line2poly(zones2001)
zones.coords <- coordinates(zones)
zones.id <- cut(zones.coords[,1], quantile(zones.coords[,1]), include.lowest=TRUE)
zones <- unionSpatialPolygons(zones, zones.id)

#king <- line2poly(king)
#prairie <- line2poly(prairie)
#zones2001 <- spRbind(king,prairie)
#zones2001 <- gUnionCascaded(zones2001)

# Clip district using bounding box
library(raster)
# Function for clipping via bounding box
clip_spdf <- function(shp, bb, byid = T) {
    if(class(bb)[1] == "Extent") e <- bbox(bb) else e <- bb
    e <- rgeos::readWKT(paste('POLYGON((',e[1,1],e[2,1],',',e[1,1],e[2,2],',',e[1,2],e[2,2],',',e[1,2],e[2,1],',',e[1,1],e[2,1],'))',collapse=' '))
    e@proj4string <- shp@proj4string
    rgeos::gIntersection(shp, e, byid=byid)
}

# 1. clip south of Washington and west of Highcross
clip <- getPoint(ildot,"PFEFFER RD","WASHINGTON ST") # shift lon: tatman/highcross
clip <- shiftPoint(clip,shift=list(1,getPoint(ildot,"TATMAN DR","HIGHCROSS RD")[1]))

b <- bbox(usd116)
b[,1] <- clip
#plot(poly, xlim = b[1, ], ylim = b[2, ])

paine.1 <- clip_spdf(usd116,b)

# 2. clip north of Washington and east of Highcross
b <- bbox(usd116)
b[,2] <- clip

paine.2 <- clip_spdf(usd116,b)

paine <- gDifference(usd116,paine.2)
paine <- SpatialPolygons2PolySet(paine)
paine <- PolySet2SpatialLines(paine)
paine <- spTransform(paine, CRSobj = CRS(proj4string(usd116)))


# Crop district based on extent of other zones. Note: creates bounding box...
paine.3 <- crop(usd116,extent(zones2001))
paine.3 <- gDifference(usd116,paine.3)

paine.4 <- usd116[zones2001,]

# Convert district polygons to lines
rnames <- row.names(poly)
poly <- SpatialPolygons2PolySet(usd116)
poly <- PolySet2SpatialLines(poly)
paine <- gDifference(poly,zones2001)
paine <- poly[paine,]


# Another function for clipping via bounding box
gClip <- function(shp, bb) {
    if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    else b_poly <- as(extent(bb), "SpatialPolygons")
    gIntersection(shp, b_poly, byid = T)
}

paine <- gClip(poly, b)

