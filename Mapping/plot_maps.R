## Plot attendance area maps
## see "zones_*.R" for details

## --------------------------------------------------------------------------- #
## map in ggplot
library(ggplot2)
library(ggmap)
library(ggthemes)
library(Cairo)
library(gridExtra)
library(ggsn)

## --------------------------------------------------------------------------- #

## Load data
load(file=paste0(path.to.data,"Maps/CUSD4/champaign1989.Rda"),verbose=TRUE) # CUSD 4 circa 1998

load(file=paste0(path.to.data,"Maps/USD116/1989/urbana1989.Rda"),verbose=TRUE) # USD 116 circa 1998

load(paste0(path.to.data,"Processed/xy_schools.Rda"),verbose=TRUE) # geocoded school addresses

## prepare school coordinates for plotting

## drop na, pick elementary schools, grab unique coords, rename (for convenience)
xy.schools <- xy.schools[!is.na(School.Longitude),]
xy.schools <- xy.schools[grep("ELEMENTARY",School.Name),]

xy.schools.89 <- unique(xy.schools,by=c("School.Name","School.Longitude.Latitude"))
setnames(xy.schools.89,old=c("School.Longitude","School.Latitude"),new=c("long","lat"))

## convert to SpatialPointsDataFrame
coordinates(xy.schools) <- ~long+lat

## project
proj4string(xy.schools) <- CRS("+proj=longlat +datum=NAD83")
xy.schools <- spTransform(xy.schools,CRS(proj4string(champaign.1989)))

## back to data.frame for ggplot
xy.schools <- data.frame(xy.schools)
xy.schools <- data.table(xy.schools)

## get champaign schools for relevant period
xy.schools.champaign <- xy.schools[grep("CHAMPAIGN",Agency.Name),]
xy.schools.champaign <- xy.schools.champaign[Year<1999,]
## xy.schools.champaign[,Agency.Name:=as.factor("CUSD 4")]

xy <- xy.schools.champaign[grep("WASHINGTON|STRATTON|BARKSTALL",School.Name,invert=TRUE),
                           .(School.Name,long,lat)]
magnet <- xy.schools.champaign[grep("WASHINGTON|STRATTON|BARKSTALL",School.Name),
                               .(School.Name,long,lat)]

## get urbana schools for relevant period
xy.schools.urbana <- xy.schools[grep("URBANA",Agency.Name),]

uv <- xy.schools.urbana[Year<1999,.(School.Name,long,lat)]


## --------------------------------------------------------------------------- #
## prepare data.frame for ggplot

champaign.schools <- c("BOTTENFIELD","CARRIE.BUSEY","COLUMBIA","HOWARD","GARDEN.HILLS","KENWOOD","ROBESON","SOUTH.SIDE","WASHINGTON","WESTVIEW")

czones <- fortify(champaign.1989,region="School.Name")

for (school in champaign.schools) {
    czones[grep(school,czones$group),"School"] <- school
}

czones$School <- as.factor(czones$School)
czones$Agency <- as.factor("CUSD 4")



## remark: may be able to skip this with
##czones <- fortify(champaign.1989,region="School.Name")
##czones$School.Name <- czones$id
##czones.2 <- join(czones, champaign.1989@data, by="School.Name")

## to add other data, e.g., for fill, try (example from stackoverflow)
## muni.df <- merge(muni.df, temp.data, by.x="CODIGOINE", by.y="CODINE", all.x=TRUE, all.y=FALSE)




## make map
q <- ggplot(czones,aes(long,lat)) +
    geom_polygon(aes(group=group,fill=School)) +
        geom_path(color="white",aes(group=group)) +
            coord_map()

q <- q + labs(x = "", y = "", fill = "") +
     theme_nothing(legend = TRUE)
    ##theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # drop x ticks/text
    ##      axis.ticks.x = element_blank(),axis.text.x = element_blank(), # drop y ticks/text
    ##      plot.title = element_text(lineheight=.8, face="bold", vjust=1)) # title bold, add space


## add schools to plot;
## NOTE: only schools active pre-choice (and no magnet, i.e., Washington)
q <- q + geom_point(data=xy,aes(long,lat),alpha=3/4,size=3,color="grey20")+ # to get outline
    geom_point(data=xy,aes(long,lat),alpha=3/4,size=2,color="white") #

## can also add point labels, but seems unnecessary:
## NB: add label=School.Name to geom_point::aes
## q <- q + geom_text(aes(label=School.Name),hjust=0, vjust=0)

## save pdf
cairo_pdf("../Paper/Figs/champaign1989.pdf",width=6,height=4.5)
print(q)
dev.off()

ggsave(q, file = "../Paper/Figs/champaign1989.png", width = 6, height = 4.5, type = "cairo-png")

## --------------------------------------------------------------------------- #




## TURN THIS^ INTO A FUNCTION!! ##
map.zones <- function(shp,shp.id,r,save.it=NA) {
    ## Map a shapefile in ggplot2
    ## Inputs:
    ## shp = (Spatial*) shapefile object to be mapped
    ## shp.id = (string) id var for data.frame
    ## r = (string) region for grouping shapefile pieces together
    ## save.it = (string) path to save output (optional)
    ## -----------------------------------------
    require(ggplot2)
    ## prepare data.frame
    df <- fortify(shp,region=r)
    df[[shp.id]] <- df[["id"]]
    df <- join(df, shp@data, by = shp.id)
    ## base map
    q <- ggplot(df, aes(long,lat)) +
        geom_polygon(aes(group = group, fill = id)) +
            coord_map() +
                labs(x = "", y = "", fill = "") +
                    theme_nothing(legend = TRUE)
    if (!is.na(save.it)) {
        cairo_pdf(paste0(save.it,".pdf"), width = 6, height = 4.5)
        print(q)
        dev.off()
    }
    return(q)
    ## return(list("map"=q,"data"=df))
}
                
add.points <- function(q,pts) {
    ## Add points to plotted shapefile
    ## Inputs:
    ## q = ggplot object
    ## pts = (data.frame) with lon/lat coordinates
    ## NB: coordinates must be projected wrt q
    ## -------------------
    q <- q + geom_point(data=pts,aes(long,lat),alpha=3/4,size=3,color="grey20") +
        geom_point(data=pts,aes(long,lat),alpha=3/4,size=2,color="white")
    return(q)
}


## --------------------------------------------------------------------------- #

## prepare data.frame for ggplot
uzones <- fortify(urbana.1989,region="School.Name")

## group by school
urbana.schools <- c("KING","LEAL","PAINE","PRAIRIE","WILEY","YANKEE.RIDGE")

for (school in urbana.schools) {
    uzones[grep(school,uzones$group),"School"] <- school
}

uzones$School <- as.factor(uzones$School)
uzones$Agency <- as.factor("USD 116")

## make map
p <- ggplot(uzones,aes(long,lat)) +
    geom_polygon(aes(group=group,fill=School)) +
        geom_path(aes(group=group),color="white") +
            coord_map()

p <- p + labs(x = "", y = "", fill = "") +
    theme_nothing(legend = TRUE)
    ##theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # drop x ticks/text
    ##      axis.ticks.x = element_blank(),axis.text.x = element_blank(), # drop y ticks/text
    ##      plot.title = element_text(lineheight=.8, face="bold", vjust=1)) # title bold, add space


## add schools to plot
p <- p + geom_point(data=uv,aes(long,lat),alpha=3/4,size=3,color="grey20")+ # to get outline
    geom_point(data=uv,aes(long,lat),alpha=3/4,size=2,color="white") #

## save pdf
cairo_pdf("../Paper/Figs/urbana1989.pdf",width=6,height=4.5)
print(p)
dev.off()

ggsave(p, file = "../Paper/Figs/urbana1989.png", width = 6, height = 4.5, type = "cairo-png")

## --------------------------------------------------------------------------- #

## combine champaign and urbana
cu98.df <- rbind(czones,uzones)

( pq <- ggplot(cu98.df, aes(long,lat)) +
      geom_polygon(aes(group=interaction(group,Agency), fill=Agency)) +
      geom_path(aes(group=interaction(group,Agency)), color="white") +
      coord_map() )

##cu.map <- get_map(location=c(bb1["x","min"],bb1["y","min"],bb2["x","max"],bb2["y","max"]))
##cu.map <- get_map(location="Champaign-Urbana", zoom=11)
##ggmap(cu.map) + geom_polygon(data=cu98.df, aes(x=long, y=lat, group=group, fill=Agency)) + geom_path(data=cu98.df, aes(group=group), color="white") 

( pq <- pq + labs(x = "", y = "", fill = "") +
      theme_nothing(legend = TRUE)
    ##theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # drop x ticks/text
    ##      axis.ticks.x = element_blank(),axis.text.x = element_blank(), # drop y ticks/text
    ##      plot.title = element_text(lineheight=.8, face="bold", vjust=1)) # title bold, add space
)

## add school points
( pq <- pq + geom_point(data=xy, aes(long,lat),color="black", alpha=3/5) +
      geom_point(data=uv,aes(long,lat),color="black",alpha=3/5) )

## add scale bar:

## (i) using ggsn
##pq + ggsn::scalebar(data=cu98.df, dist=1.61, dd2km=TRUE, model="GRS80", st.size=2.5)

## (ii) using custom function
bb1 <- bbox(champaign.1989)
bb2 <- bbox(urbana.1989)

( pq <- pq +
      scaleBar(lon=bb2["x","min"], lat=bb1["y","min"], distanceLon=2, distanceLat=0.25, distanceLegend=0.5, dist.unit="mi", orientation=FALSE) )

##( gg <- ggplot() +
##      geom_map(map=cu98.df, data=cu98.df, aes(x=long, y=lat, map_id=id)) +
##      geom_point(data=xy, aes(long,lat),color="black", alpha=3/5) +
##      geom_point(data=uv,aes(long,lat),color="black",alpha=3/5) )
##      #geom_polygon(data=cu98.df, aes(group=interaction(group, Agency), fill=Agency)) )

## save pdf
cairo_pdf("../Paper/Figs/cu1998.pdf",width=6,height=4.5)
print(pq)
dev.off()

ggsave(pq, file = "../Paper/Figs/cu1998.png", width = 6, height = 4.5, type = "cairo-png")

## --------------------------------------------------------------------------- #

## The following is unnecessary, though instructive

schools <- c(champaign.schools,urbana.schools)

fzones <- spRbind(merged,zones)
## prepare data.frame for ggplot
fzones <- fortify(fzones,region="School.Name")

for (school in schools) {
    fzones[grep(school,fzones$group),"School"] <- school
}

fzones$School <- as.factor(fzones$School)

fzones[fzones$School %in% champaign.schools,"Agency"] <- "CUSD 4"
fzones[fzones$School %in% urbana.schools,"Agency"] <- "USD 116"
fzones$Agency <- as.factor(fzones$Agency)

## make map
pq <- ggplot(fzones,aes(long,lat)) +
    geom_polygon(aes(group=interaction(group,Agency),fill=Agency)) +
        geom_path(color="white") +
            coord_map()

pq <- pq + theme_nothing(legend = TRUE) +
    labs(title = "Champaign and Urbana attendance areas, circa 1998",
         fill = "")

ggsave(pq, file = "../Paper/Figs/cu1998.png", width = 6, height = 4.5, type = "cairo-png")

## alt:
pq <- ggplot(fzones) +
    aes(long,lat,group=group,fill=School)

## --------------------------------------------------------------------------- #

## compare unsd10 vs unsd00


## --------------------------------------------------------------------------- #

## Plot Champaign post-choice + urbana.1989 (i.e, period 1998-2002)

## Load district shapefiles
load(paste0(path.to.data,"Maps/unsd.Rda"),verbose=T)

## use urbana.1989 as geom_polygon
## use cusd4 shapefile as another geom_polygon()
## add schools as geom_point()
## draw 1.5 mile radius for Prox A homes for each school
## **may need to do this directly on data.frame of long/lat points
## find "closest school" for homes not in Prox A radius

## Options for plotting radius:

## spatstat::disc
## geosphere::spDists
## sampSurf::spCircle

## Then clip circles to "cusd4" shapefile
## drop na, pick elementary schools, grab unique coords, rename (for convenience)


## rgeos::gBuffer -
## NOPE! this draws buffer around ALL points
## apply one point at a time
xyz <- xy.schools.champaign[Year>1999,]
xyz <- unique(xyz,by=c("School.Name","School.Longitude.Latitude"))
setnames(xyz,old=c("School.Longitude","School.Latitude"),new=c("long","lat"))

## convert to SpatialPointsDataFrame
coordinates(xyz) <- ~long+lat

## NB: CRS must be projected for gBuffer
proj4string(xyz) <- CRS("+proj=longlat +datum=NAD83")
xyz <- spTransform(xyz,CRS("+init=epsg:3347"))
zones <- gBuffer(xyz[1,],width = 1.5*1.60934*1000, byid = TRUE)

## works...? seems small

##zones <- spTransform(cusd4,CRS("+init=epsg:3347"))
zones <- spTransform(zones,CRS(proj4string(cusd4)))
xyz <- spTransform(zones,CRS(proj4string(cusd4)))

plot(cusd4)
plot(zones,add=TRUE)
plot(xyz[1,],add=TRUE)

##merc <- "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft+no_defs +ellps=GRS80 +towgs84=0,0,0"
##merc <- "+proj=tmerc +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
##zones <- lapply(1:nrow(xyz), function(i) gBuffer(xyz[i,], width = 1.5*1.60934*100, byid = TRUE))

