## Some useful data processing functions for use with
## "../Prep/*.R" files

## Extract year from POSIX date
## To set date class for data.table:
## dt[,date:=as.IDate(date,"%Y%m%d")]
getYear <- function(d) as.POSIXlt(d)$year + 1900

## Custom descriptive statistics, for use in call to data.table
mysummary <- function(x) {
    list("Mean"=mean(x,na.rm=T),
         "Median"=median(x,na.rm=T),
         "St dev"=sd(x,na.rm=T),
         "N"=sum(x,na.rm=T))
}

## For converting factors to numeric
as.numeric.factor <- function(x) {x <- as.numeric(as.character(x))}

## For converting character "dollars" to numeric
deDollarize <- function(x) {
    x <- as.numeric(gsub("\\$|,","",x))
}

## combine preceding two:
##as.numeric.factor <- function(x) {
##    x <- as.numeric(as.character(gsub("\\$|,","",x)))
##}

## Subset data.frame "df" with respect to ALL cases of duplicates by "var"
find.dupes <- function(df,var) {
    ## inputs: df = data.frame, var = character
    ## output: data.frame with only dupes by "var"
    n.occur <- data.frame(table(df[[var]]))
    dupes <- n.occur[n.occur[["Freq"]]>1,]
    dupes <- df[df[[var]] %in% dupes[["Var1"]],]
    return(dupes)
}

## Use GEOID to match geocoded houses to Census tracts #
## creates data.frame with ID's (i.e., Property ID)
assign.geo.2 <- function(id,dfc,layer,projdfc=CRS("+proj=longlat +datum=WGS84")) {
    ## id = 1 column data.frame or data.table of observation IDs
    ## dfc = 2 column data.frame or data.table of longitude, latitude coordinates
    ## layer = SpatialPolygonsDataFrame object, i.e., shapefile imported by rgdal
    ## projdfc = CRS projection info for df=[id,dfc]
    ## Will not work on NA long/lat    
    require(rgdal)
    pts <- SpatialPoints(dfc,proj4string=projdfc)
    pts <- spTransform(pts,CRS(proj4string(layer)))
    over.pts <- over(pts,layer)
    return(cbind(id,pts,over.pts))
}

## ALT: works well as function call INSIDE a data.table
assign.geo <- function(long,lat,layer,projdfc=CRS("+proj=longlat +datum=WGS84")) {
    ## long = longitude
    ## lat = latitude
    ## layer = SpatialPolygonsDataFrame object, i.e., shapefile imported by rgdal
    ## projdfc = CRS projection info for df=[id,dfc]
    ## Will not work on NA long/lat
    require(rgdal)
    dfc <- data.frame(cbind(long,lat))
    pts <- SpatialPoints(dfc,proj4string=projdfc)
    pts <- spTransform(pts,CRS(proj4string(layer)))
    over.pts <- over(pts,layer)
    return(cbind(pts,over.pts))
}

## import shapefiles using rgdal
importShp <- function(dir,fname,layer=FALSE) {
    ## dir: (character) path to shapefiles
    ## fname: (character) keyword or full name of desired shapefile
    require(rgdal)
    if (layer) {
        layer <- fname
    } else {
        shp.files <- list.files(dir,pattern=fname)
        shp.files <- shp.files[grep("\\.shp$",shp.files)]
        layer <- shp.files[grep(fname,shp.files)]
        layer <- substr(layer,1,nchar(layer)-4)
    }
    shp <- readOGR(dir,layer)
    return(shp)
}

## Using package stringdist to find approximate string matches #
## N.B.: Specifically written for lender data
## compute distance matrix (dqlenders X lenders), min each row...huge, unnecessary?
strdisMatch <- function(x,y,mymethod="jw",mydist=Inf,myp=0,myq=1) {
    require(stringdist)
    zz <- data.frame("Lender.Raw"="","Lender.Match"="","Lender.Match.Dist"=as.numeric(""),stringsAsFactors=F)
    for (i in 1:length(x)) {
        xx <- sapply(y,stringdist,b=x[i],method=mymethod,maxDist=mydist,p=myp,q=myq)
        xx <- xx[which.min(xx)]
        zz[i,] <- cbind(x[i],names(xx),as.numeric(xx))
    }
    return(zz)
}

## Directly using the function stringdist::stringdistmatrix
strmatMin <- function(x,y,mymethod="jw",mydist=Inf,myp=0,myq=1) {
    require(stringdist)
    ## x = character vector source
    ## y = character vector target (lookup table)
    zz <- stringdistmatrix(a=x,b=y,method=mymethod,maxDist=mydist,q=myq,p=myp)
    ## rownames(zz) <- x
    ## colnames(zz) <- y
    ## min each row, preserving names... simplify this: 
    zz <- data.frame("Lender.Raw"=x,"Lender.Match"=y[apply(zz[,1:ncol(zz)],1,which.min)],"Lender.Match.Distance"=apply(zz[,1:ncol(zz)],1,min),stringsAsFactors=FALSE)
    return(zz)
}

## Special function for cleaning up lender names in dq and hmda data
## Remove punctuation, replace "&" with "and", delete excess space, set all lower case
## SPECIAL CHANGES:
## "UNIV" to "UNIVERSITY", "BK" to "BANK", "NAT|NATL" to "NATIONAL", "AMER" to "AMERICA", "MTG" to "MORTGAGE" ... "ST" to "STATE"?
cleanUp <- function(x,special=TRUE,arts=FALSE,corp=FALSE,spaces=FALSE) {
    if (special) {
        ## SPECIAL CHANGES - remove or modify for future use #
        x <- gsub("U\\.S\\.|U\\.S\\.A\\.","USA",x) # standardize USA
        x <- gsub("\\bAMER\\b","AMERICA",x) # standardize America
        x <- gsub("\\bNAT\\b|\\bNATL\\b|\\bNAT'L\\b","NATIONAL",x) # standardize National        
        x <- gsub("\\bUNIV\\b|\\bINIVERSITY\\b","UNIVERSITY",x) # standardize University
        x <- gsub("\\bIL\\b","ILLINOIS",x) # standardize Illinois
        x <- gsub("\\bEMP(|S|L|LOY|LOYE|LOYEE)\\b","EMPLOYEES",x) # tandardize Employees        
        x <- gsub("\\bBK\\b","BANK",x) # standardize Bank
        x <- gsub("\\bB&T\\b","BANK & TRUST",x) # standardize Bank and Trust
        x <- gsub("\\bB&TC\\b","BANK & TRUST CO",x) # ditto
        x <- gsub("\\bH&R\\b","H & R",x) # H&R Block
        x <- gsub("\\bFNC\\b","FINANCE",x) # standardize Finance
        x <- gsub("\\bFIN(L|'L)\\b|\\bFNCL\\b","FINANCIAL",x) # standardize Financial
        ## x <- gsub("\\bSVC\\b|\\bSVCS\\b","SERVICES",x) # standardize Services
        x <- gsub("\\bMTG\\b","MORTGAGE",x) # standardize Mortgage
        x <- gsub("\\bORTGAGE\\b","MORTGAGE",x) # Catch errors        
        x <- gsub("\\bCORP.*\\b","CORP",x) # standardize Corp
        x <- gsub("\\bST\\sB(\\w|\\W)+","STATE",x) # standardize State 
        x <- gsub("\\bNB\\b","NATIONAL BANK",x) # tandardize National Bank
        ## VERY SPECIAL, ONLY A FEW #
        x <- gsub("\\bEQT\\b","EQUITY",x) # standardize Equity
        x <- gsub("\\bLL\\b","LLC",x) # standardize LLC
        x <- gsub("\\bPRK\\b","PARK",x) # standardize Park
        ## x <- gsub("\\bFOUNDAT\\b","FOUNDATION",x) # standardize Foundation
        ## END SPECIAL CHANGES #
    }
    x <- gsub("-|/"," ",x) # replace hyphen with space
    x <- gsub("'","",x) # remove apostrophe
    x <- gsub("([[:alnum:]+[:space:]])&([[:alnum:]+[:space:]])","\\1 AND \\3",x) # replace "&"
    x <- gsub("[^[:alnum:][:space:]']", "",x) # remove remaining puncuation, except '
   ## also works: gsub("(.*?)($|'|&|[^[:punct:]]+?)(.*?)", "\\2",x)
    if (arts) {
        x <- gsub("\\b(AND|IN|OF|THE)\\b"," ",x) # remove articles
    }
    if (corp) {
        x <- gsub("CO(|RP)|INC|LLC|NA"," ",x) # remove Corporate Designations
    }
    x <- gsub("\\s+"," ",x) # Remove excess space
    x <- gsub("\\s$","",x) # Remove trailing space
    x <- tolower(x) # change to lower case
    if (spaces) { # order alphabetically and concatenate into single string
        x <- sapply(x,function(i) unlist(strsplit(i,split="\\s+")))
        x <- sapply(x,function(i) paste0(i[order(i)],collapse=""))
        ## x <- gsub("\\s+","",x)
            }
    return(x)
}

## function to clean names for matching
cleanName <- function(x,arts=FALSE,spaces=FALSE) {
    ## rearange if x ="LAST, FIRST M"
    x <- sapply(x, function(i) unlist(strsplit(i,split=",")))
    x <- sapply(x, function(i) ifelse(length(i)>1,
                                      paste(i[2],i[1],sep=" ",collapse=" "),i[1]) )
    ## replace "&" with AND
    x <- gsub("([[:alnum:]+[:space:]])&([[:alnum:]+[:space:]])","\\1 AND \\3",x)
    ## peculiar typo: "7" should be "&"
    x <- gsub("\\b7\\b","AND",x)
    ## remove ALL puncuation
    x <- gsub("[^[:alnum:][:space:]]", " ",x)
    ## also works: gsub("(.*?)($|'|&|[^[:punct:]]+?)(.*?)", "\\2",x)
    if (arts) {
        ## remove articles
        x <- gsub("\\b(AND|IN|OF|THE)\\b"," ",x) 
    }
    ## Remove excess space
    if (spaces) {
        x <- gsub("\\s+","",x)
    } else {
        x <- gsub("\\s+"," ",x)
    }
    ## change to lower case
    x <- tolower(x)
    return(x)
}

## Function to subset strings by keywords; see parse_mls.R
mySubstring <- function(key1,key2="",string,case=FALSE) {
    ## key1 = a regexp such as PIN:\n.*
    ## key2 = an optional regexp such as Year (assumed preceded by literal "\n"
    ## string = a string (vector) input
    x <- gsub(paste0(".*(",key1,")(\n",key2,").*"),"\\1",string,ignore.case=case)
    x <- gsub("(\n | )+|\\s+"," ",x) # clean up
    x <- gsub("^(.*:\\s)","",x) # drop name
    return(x)
}

## Function to subset PIN by keywords; see parse_county.R
pinSubstring <- function(key1,key2="",string,case=FALSE,unique=TRUE) {
    ## key1 = a regexp such as PIN:\n.*
    ## key2 = an optional regexp such as Year
    ## string = a string (vector) input
    if (unique) {
        x <- gsub(paste0(".*(",key1,".*)(",key2,").*"),"\\1",string,ignore.case=case)
        x <- gsub("(\n | )+|\\s+"," ",x) # clean up
        x <- gsub("^(.*:\\s)","",x) # drop name
        x <- gsub("\\s+","",x)
    } else {
        x <- gsub(paste0(".*(",key1,".*)(",key2,").*$"),"\\1",string,ignore.case=case)
        x <- gsub("(\n | )+|\\s+"," ",x) # clean up
        x <- gsub("^(.*:\\s)","",x) # drop name
        x <- strsplit(x,split=gsub("\n","",key2))[[1]][1]
        x <- gsub(paste0("^",key1,"(.*)(\\s\\d+-\\d+-\\d+-\\d+-\\d+.*)$"),"\\2",x)
        x <- gsub("\\s+","",x)
    }
    ## x <- gsub("-","",x)
    return(x)
}
