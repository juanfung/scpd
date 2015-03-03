# For dataframes containing street intersection coordinates:
# df[[1]] = street 1
# df[[2]] = street 2
# --------------------------------------------------------------------------- #

# find a street name by grep
findStreet <- function(df,street) {
    unique(c(grep(street,df[[1]],value=T,ignore.case=T),
           grep(street,df[[2]],value=T,ignore.case=T)))
}

# find a street's intersections
showCrossing <- function(df,street,road1=TRUE) {
    if (road1) {
        table(as.character(df[df[[1]] %in% street,][[2]]))
    } else {
        table(as.character(df[df[[2]] %in% street,][[1]]))
    }
}

# find coordinates for a single point, defined by the intersection of s1 and s2
# N.B.: intersections may not be unique. 
getPoint <- function(df,s1,s2) {
    require(sp)
    pt <- coordinates(df[(df[[1]] %in% s1 & df[[2]] %in% s2),])
    return(pt)
}

# collect ordered cross-street coordinates into a matrix
# N.B.: order so as to connect into a polygon
makePoints <- function(df,s) {
    # df: data.frame with df[1]=s1 and df[2]=s2
    # s: character matrix with cross-street names (in order wrt df)
    # coordinates determined by (street.we,street.ns) pairs
    require(sp)
    
    n <- nrow(s)
    pts <- matrix(nrow=n,ncol=2)
    #lines <- list()
    for (i in 1:n) {
        pts[i,] <- coordinates(df[(df[[1]] %in% s[i,1] & df[[2]] %in% s[i,2]),])
    }
    return(pts)
}

# shift coordinates, as needed (in case desired intersection is missing)
shiftPoint <- function(pts,shift,lon=TRUE) {
    # pts : a matrix of ordered coordinates
    # shift : a list, shift[[1]]=index is row to shift, shift[[2]]=coordinate to be inserted
    # lon : default shift is longitude
    require(sp)

    if (lon) {
        pts[shift[[1]],1] <- shift[[2]]
    } else {
        pts[shift[[1]],2] <- shift[[2]]
    }
    return(pts)
}

# collect ordered coordinates into a list of ordered lines
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
    # proj : CRS proj for SpatialLines object
    require(sp)
    
    poly <- Lines(lines,ID=poly.id)
    poly <- SpatialLines(list(poly),CRS(proj))
    poly.data <- data.frame(Agency.Name="USD 116",
                            School.Name=gsub("(.*)\\.\\d+$","\\1",poly.id),
                            row.names=poly.id)
    poly <- SpatialLinesDataFrame(poly,data=poly.data)
    return(poly)
}

# convert SpatialLines* to SpatialPolygons*
line2poly <- function(poly,data=TRUE){
    # poly: a SpatialLines object
    # if data=TRUE :  SpatialLinesDataFrame -> SpatialPolygonsDataFrame
    require(sp)
    require(maptools)
    if (data) poly.data <- poly@data
    rnames <- row.names(poly)
    poly <- SpatialLines2PolySet(poly)
    poly <- PolySet2SpatialPolygons(poly)
    row.names(poly) <- rnames
    if (data) poly <- SpatialPolygonsDataFrame(poly,poly.data)
    return(poly)
}

