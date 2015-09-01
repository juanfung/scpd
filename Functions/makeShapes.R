## For dataframes containing street intersection coordinates:
## df[[1]] = street 1
## df[[2]] = street 2

## --------------------------------------------------------------------------- #
## load packages
library(sp)
library(maptools)

## --------------------------------------------------------------------------- #

## find a street name by grep
findStreet <- function(df,street) {
    unique(c(grep(street,df[[1]],value=T,ignore.case=T),
           grep(street,df[[2]],value=T,ignore.case=T)))
}

## find a street's intersections; ignore case by default
showCrossing <- function(df,street,road1=TRUE) {
    if (road1) {
        table(as.character(df[df[[1]] %in% toupper(street),][[2]]))
    } else {
        table(as.character(df[df[[2]] %in% toupper(street),][[1]]))
    }
}

## find coordinates for a single point, defined by the intersection of s1 and s2
## N.B.: intersections may not be unique. 
getPoint <- function(df,s1,s2) {
    
    pt <- coordinates(df[(df[[1]] %in% s1 & df[[2]] %in% s2),])
    return(pt)
}

## collect ordered cross-street coordinates into a matrix
## N.B.: order so as to connect into a polygon
makePoints <- function(df,s) {
    ## df: data.frame with df[1]=s1 and df[2]=s2
    ## s: character matrix with cross-street names (in order wrt df)
    ## coordinates determined by (street.we,street.ns) pairs    
    
    n <- nrow(s)
    pts <- matrix(nrow=n,ncol=2)
    ## lines <- list()
    for (i in 1:n) {
        pts[i,] <- coordinates(df[(df[[1]] %in% s[i,1] & df[[2]] %in% s[i,2]),])
    }
    return(pts)
}

## shift coordinates, as needed (in case desired intersection is missing)
shiftPoint <- function(pts,shift,lon=TRUE) {
    ## pts : a matrix of ordered coordinates
    ## shift : a list, shift[[1]]=index is row to shift, shift[[2]]=coordinate to be inserted
    ## lon : default shift is longitude
    

    if (lon) {
        pts[shift[[1]],1] <- shift[[2]]
    } else {
        pts[shift[[1]],2] <- shift[[2]]
    }
    return(pts)
}

## create SpatialPolygons object from ordered coordinates
makePolygon <- function(pts,poly.id,proj) {
    ## pts: matrix of ordered coordinates
    ## poly.id: character name to ID polygon
    ## proj: CRS proj for SpatialPolygons object
    
    poly <- Polygon(pts)
    poly <- Polygons(list(poly),ID=poly.id)
    poly <- SpatialPolygons(Srl=list(poly),proj4string=CRS(proj))
    return(poly)
}

## convert SpatialPolygons to SpatialPolygonsDataFrame
makeSPDF <- function(poly,df) {
    ## poly: SpatialPolygons object
    ## df: data.frame
    n <- length(row.names(poly))
    if (length(unique(row.names(poly)))<n) {
        row.names(poly) <- paste(row.names(poly),c(1:n),sep=".")
        df <- data.frame(df,ID=row.names(poly))
    }
    row.names(df) <- row.names(poly)
    poly <- SpatialPolygonsDataFrame(poly,data=df)
    return(poly)
}

## connect ordered coordinates, return SpatialLines object
makeLine <- function(pts,names,poly.id,proj) {
    ## pts: a matrix of ordered coordinates
    ## poly.id: character name to ID polygon
    ## proj : CRS proj for SpatialLines object    
    
    n <- nrow(pts)
    lines <- list()
    for (i in 2:n) {
        lines[i-1] <- Line(rbind(pts[i,],pts[i-1,]))
    }
    lines[i] <- Line(rbind(pts[i,],pts[1,]))
    lines <- setNames(lines,names)
    lines <- Lines(lines,ID=poly.id)
    poly.lines <- SpatialLines(list(lines),CRS(proj))
    return(poly.lines)
}

## convert SpatialLines to SpatialLinesDataFrame
makeSLDF <- function(poly.lines,df) {
    ## poly: SpatialPolygons object
    ## df: data.frame
    
    n <- length(row.names(poly.lines))
    if (length(unique(row.names(poly.lines)))<n) {
        row.names(poly.lines) <- paste(row.names(poly.lines),c(1:n),sep=".")
        df <- data.frame(df,ID=row.names(poly.lines))
    }
    row.names(df) <- row.names(poly.lines)
    poly.lines <- SpatialLinesDataFrame(poly.lines,data=df)
    return(poly.lines)
}

## convert SpatialLines* to SpatialPolygons*, indirectly using maptools
## more direct: as(poly,"SpatialPolygons*")
line2poly <- function(poly,data=TRUE){
    ## poly: a SpatialLines object
    ## if data=TRUE :  SpatialLinesDataFrame -> SpatialPolygonsDataFrame
    
    require(maptools)
    if (data) poly.data <- poly@data
    rnames <- row.names(poly)
    poly <- SpatialLines2PolySet(poly)
    poly <- PolySet2SpatialPolygons(poly)
    row.names(poly) <- rnames
    if (data) poly <- SpatialPolygonsDataFrame(poly,poly.data)
    return(poly)
}

## like line2poly
## more direct: as(poly,"SpatialLines*")
poly2line <- function(poly,data=TRUE){
    ## poly: a SpatialLines object
    ## if data=TRUE :  SpatialLinesDataFrame -> SpatialPolygonsDataFrame    
    
    if (data) poly.data <- poly@data
    rnames <- row.names(poly)
    poly <- SpatialPolygons2PolySet(poly)
    poly <- PolySet2SpatialLines(poly)
    row.names(poly) <- rnames
    if (data) poly <- SpatialLinesDataFrame(poly,poly.data)
    return(poly)
}
