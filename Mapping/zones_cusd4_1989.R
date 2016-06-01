## Attendance Zones in CUSD4, 1989-1998
## 1989 map: see "cusd4map*.pdf"

## load packages, etc.
source("zones_load.R")

## load roads shapefile
load(paste0(path.to.data,"Maps/hwy_lc.Rda"),verbose=TRUE)

## Load district shapefiles
load(paste0(path.to.data,"Maps/unsd.Rda"),verbose=T)

## Approach:
## 1. Create matrix of ordered cross-street coordinates
## 2. Collect coordinates into list of ordered lines
## 3. Connect ordered lines into SpatialPolygons, with district  and attendance zone labels
## 4. Convert to SpatialPolygonsDataFrame
## 5. Export as shapefile

## basic df for SpatialPolygonsDataFrame
poly.data <- data.frame(Agency.Name="CUSD 4",Year="1989")

## --------------------------------------------------------------------------- #

## schools:
champaign.schools = c("BOTTENFIELD","CARRIE.BUSEY","COLUMBIA","HOWARD","GARDEN.HILLS","KENWOOD","ROBESON","SOUTH.SIDE","WASHINGTON","WESTVIEW")

## South Side

## area between Kirby and Windsor
s <- rbind(c("KIRBY AVE","GLENSHIRE DR"), # kirby (aka 1500 N) and staley; SHIFT LON
           c("STALEY RD","WINDSOR RD"), # windsor (aka 1400 N) and staley
           c("LAKEWOOD DR","WINDSOR RD"), # windsor and duncan (aka 900 E); SHIFT LON
           c("DUNCAN RD (900 E)","KIRBY AVE")) # kirby and duncan

ss.1 <- makePoints(df=ildot,s=s)
staley <- getPoint(ildot,"STALEY RD","AMHERST DR")[1]
ss.1 <- shiftPoint(pts=ss.1,shift=list(1,staley))
duncan <- getPoint(ildot,"CRESTRIDGE DR","DUNCAN RD (900 E)")[1]
ss.1 <- shiftPoint(pts=ss.1,shift=list(3,duncan))

ss.1 <- makePolygon(ss.1,
                     poly.id="SOUTH.SIDE.1",
                     proj=proj4string(ildot))

ss.1 <- makeSPDF(ss.1,data.frame(poly.data,School.Name="SOUTH.SIDE"))

## area between University and Kirby, west of RR
s <- rbind(c("SPRINGFIELD AVE","PROSPECT AVE"), # springfield and prospect
           c("ARMORY AVE","PROSPECT AVE"), # armory and prospect; NB: crossing over park
           c("KIRBY AVE","PROSPECT AVE"), # kirby and prospect
           c("KIRBY AVE","NEIL ST"), # kirby and neil
           c("STANAGE AVE","NEIL ST"), # stanage and neil
           c("UNIVERSITY AVE","WATER ST"), # university and water; SHIFT LON WEST
           c("UNIVERSITY AVE","PROSPECT AVE") # university and prospect
           )

ss.2 <- makePoints(ildot,s)
west <- getPoint(ildot,"CHESTER ST","MARKET ST")[1]
west <- west*(1/3) + (getPoint(ildot,"UNIVERSITY AVE","WATER ST")[1])*(2/3)
ss.2 <- shiftPoint(ss.2,shift=list(6,west))

ss.2 <- makePolygon(ss.2,
                     poly.id="SOUTH.SIDE.2",
                     proj=proj4string(ildot))

ss.2 <- makeSPDF(ss.2,data.frame(poly.data,School.Name="SOUTH.SIDE"))

## small area near Bradley, west of RR
s <- rbind(c("CHAMPAIGN ST","CHAMPAIGN ST"), # "juniper" and champaign; SHIFT LAT
           c("NORTH ST","CHAMPAIGN ST"), # "rr" & champaign st; SHIFT LAT
           c("CLOCK ST","BRADLEY AVE"), # "rr" & "clock"; SHIFT LAT
           c("BRADLEY AVE","CHESTNUT ST"), # bradley & "rr"; SHIFT LON??
           c("CHESTNUT ST","BELLEFONTAINE ST"), # bellefontaine and "rr"; SHIFT LON
           c("BRADLEY AVE","CHESTNUT ST") # "juniper" & oak; SHIFT LON/LAT
           ## NB: I shifted east to Oak, industrial area in between
           )

ss.3 <- makePoints(ildot,s)
juniper <- getPoint(ildot,"KENYON RD","NEIL ST")[2]
juniper <- (juniper + getPoint(ildot,"LEICHNER DR","HICKORY ST")[2])/2
ss.3 <- shiftPoint(ss.3,shift=list(1,juniper),lon=FALSE)
rr <- getPoint(ildot,"NEIL ST","MAPLE ST")[2]
ss.3 <- shiftPoint(ss.3,shift=list(2,rr),lon=FALSE)
rr <- getPoint(ildot,"NORTH ST","CHAMPAIGN ST")[2]
ss.3 <- shiftPoint(ss.3,shift=list(3,rr),lon=FALSE)
## interpolate between kenyon/oak and bradley/chestnut
north <- getPoint(ildot,"KENYON RD","OAK ST")
south <- getPoint(ildot,"BRADLEY AVE","CHESTNUT ST")
interp <- approx(rbind(north,south))
## match desired latitude: juniper (see above)
index <- which(abs(juniper-interp[[2]])<=0.00005)
interp.lon <- interp[[1]][index]
ss.3 <- shiftPoint(ss.3,shift=list(6,interp.lon))
ss.3 <- shiftPoint(ss.3,shift=list(6,juniper),lon=FALSE)
bf <- getPoint(ildot,"CHESTNUT ST","BELLEFONTAINE ST")[2]
index <- which(abs(bf - interp[[2]])<=0.00005)
bf.lon <- interp[[1]][index]
ss.3 <- shiftPoint(ss.3,shift=list(5,bf.lon))

ss.3 <- makePolygon(ss.3,
                    poly.id="SOUTH.SIDE.3",
                    proj=proj4string(ildot))

ss.3 <- makeSPDF(ss.3,data.frame(poly.data,School.Name="SOUTH.SIDE"))

ss <- spRbind(ss.1,spRbind(ss.2,ss.3))

## Save
save(ss.1,ss.2,ss.3,file=paste0(path.to.data,"Maps/CUSD4/ss.Rda"))


## Columbia

## area between 74 and University, west of RR
s <- rbind(c("UNIVERSITY AVE","PROSPECT AVE"), # university and prospect
           c("UNIVERSITY AVE","WATER ST"), # university and water; SHIFT LON WEST
           c("COLUMBIA AVE","MARKET ST"), # "railroad crossing"; SHIFT LON; can shift LAT north...
           c("CLOCK ST","BRADLEY AVE"), # "rr" & "clock"; SHIFT LAT
           c("NORTH ST","CHAMPAIGN ST"), # "rr" & champaign st; SHIFT LAT
           c("CHAMPAIGN ST","CHAMPAIGN ST"), # "juniper" and champaign; SHIFT LAT
           c("BRADLEY AVE","CHESTNUT ST"), # "juniper" & oak; SHIFT LON/LAT
           c("KENYON RD","OAK ST"), # kenyon and oak; SHIFT LAT NORTH
           c("I-74EB TO PROSPECT","PROSPECT AVE"), # 74 and prospect; SHIFT LON/LAT
           c("BRADLEY AVE","PROSPECT AVE") # bradley and "rr"; SHIFT LAT
           )

col.1 <- makePoints(ildot,s)
west <- getPoint(ildot,"CHESTER ST","MARKET ST")[1]
west <- west*(1/3) + (getPoint(ildot,"UNIVERSITY AVE","WATER ST")[1])*(2/3)
col.1 <- shiftPoint(col.1,shift=list(2,west))
first <- getPoint(ildot,"FIRST ST","WASHINGTON ST")[1]
col.1 <- shiftPoint(col.1,shift=list(3,first))
rr <- round(getPoint(ildot,"COLUMBIA AVE","MARKET ST")[2],4)
col.1 <- shiftPoint(col.1,shift=list(3,rr),lon=FALSE)
rr <- getPoint(ildot,"NORTH ST","CHAMPAIGN ST")[2]
col.1 <- shiftPoint(col.1,shift=list(4,rr),lon=FALSE)
rr <- getPoint(ildot,"NEIL ST","MAPLE ST")[2]
col.1 <- shiftPoint(col.1,shift=list(5,rr),lon=FALSE)
juniper <- getPoint(ildot,"KENYON RD","NEIL ST")[2]
juniper <- (juniper + getPoint(ildot,"LEICHNER DR","HICKORY ST")[2])/2
col.1 <- shiftPoint(col.1,shift=list(6,juniper),lon=FALSE)
## interpolated longitude: see ss.3 above
col.1 <- shiftPoint(col.1,shift=list(7,interp.lon))
col.1 <- shiftPoint(col.1,shift=list(7,juniper),lon=FALSE)
## "north" latitude (kenyon and oak): see ss.3 above
i74 <- (north[2] + getPoint(ildot,"OAK ST","ANTHONY DR")[2])/2
col.1 <- shiftPoint(col.1,shift=list(8,i74),lon=FALSE)
prospect.1 <- getPoint(ildot,"I-74EB TO PROSPECT","PROSPECT AVE")
prospect.1 <- (prospect.1 + getPoint(ildot,"PROSPECT TO I-74EB","PROSPECT TO I-74EB"))/2
col.1 <- shiftPoint(col.1,shift=list(9,prospect.1[1]))
## prospect.2 <- getPoint(ildot,"PROSPECT TO I-74WB","PROSPECT TO I-74WB")
## prospect.2 <- (prospect.2 + getPoint(ildot,"I-74WB TO PROSPECT","I-74WB TO PROSPECT"))/2
## prospect.2 <- (prospect.2[2] + prospect.1[2])/2
## col.1 <- shiftPoint(col.1,shift=list(8,prospect.2),lon=FALSE)
col.1 <- shiftPoint(col.1,shift=list(9,i74),lon=FALSE)
rr <- getPoint(ildot,"BRADLEY AVE","PROSPECT AVE")[2]
rr <- (rr + getPoint(ildot,"DENNISON DR","PROSPECT AVE")[2])/2
col.1 <- shiftPoint(col.1,shift=list(10,rr),lon=FALSE)


col.1 <- makePolygon(col.1,
                    poly.id="COLUMBIA.1",
                    proj=proj4string(ildot))

col.1 <- makeSPDF(col.1,data.frame(poly.data,School.Name="COLUMBIA"))

## Wilbur Heights:
## small area north of 74, between Market and RR
## REMARK: commercial area, near mall. OK if imprecise
s <- rbind(c("WILBUR AVE","MARKET ST"), # wilbur and market
           c("WILBUR AVE","FIFTH ST"), # wilbur and fifth; SHIFT LON? unnecessary
           c("WALLACE AVE","FIFTH ST"), # wallace and fifth
           c("WALLACE AVE","MARKET ST")
           )

col.2 <- makePoints(ildot,s)
## wilbur <- getPoint(ildot,"WILBUR AVE","FIFTH ST")[1]
## wilbur <- wilbur*(2/3) + getPoint(ildot,"WILBUR RD","LINCOLN AVE")[1]*(1/3)
## col.2 <- shiftPoint(col.2,shift=list(2,wilbur))

col.2 <- makePolygon(col.2,
                    poly.id="COLUMBIA.2",
                    proj=proj4string(ildot))

col.2 <- makeSPDF(col.2,data.frame(poly.data,School.Name="COLUMBIA"))

col <- spRbind(col.1,col.2)

save(col.1,col.2,file=paste0(path.to.data,"Maps/CUSD4/col.Rda"))


## Robeson

## area between Kirby and Windsor, east of SS
s <- rbind(c("DUNCAN RD (900 E)","KIRBY AVE"), # kirby and duncan
           c("LAKEWOOD DR","WINDSOR RD"), # windsor and duncan; SHIFT LON
           c("MATTIS AVE","WINDSOR RD"), # windsor and mattis
           c("MATTIS AVE","SOUTHWOOD DR"), # southwood and mattis
           c("PARKSIDE TERR","SOUTHWOOD DR"), # southwood and parkside terr
           c("CLOVER LN","BROADMOOR DR"), # southwood and clover; SHIFT LAT
           c("SOUTHWOOD CT","SOUTHWOOD DR"), # southwood and southwood ct
           c("CRESCENT DR","SOUTHWOOD DR"), # southwood and crescent
           c("CRESCENT DR","RODNEY DR"), # rodney and crescent
           c("CRESCENT DR","PARKDALE DR"), # parkdale and crescent
           c("CRESCENT DR","KIRBY AVE") # kirby and crescent
           )

rob.1 <- makePoints(df=ildot,s=s)
duncan <- getPoint(ildot,"CRESTRIDGE DR","DUNCAN RD (900 E)")[1]
rob.1 <- shiftPoint(pts=rob.1,shift=list(2,duncan))
swood <- getPoint(ildot,"PARKSIDE TERR","SOUTHWOOD DR")[2]
rob.1 <- shiftPoint(pts=rob.1,shift=list(6,swood),lon=FALSE)

rob.1 <- makePolygon(rob.1,
                     poly.id="ROBESON.1",
                     proj=proj4string(ildot))

rob.1 <- makeSPDF(rob.1,data.frame(poly.data,School.Name="ROBESON"))

## small area north of Bradley, east of Mattis
s <- rbind(c("MATTIS AVE","GLEN BURNIE DR"), # bradley and mattis; SHIFT LAT
           c("MATTIS AVE","HEDGEROAD DR"), # "rr" and mattis; SHIFT LAT
           c("HEDGEROAD DR","HEDGEROAD DR"), # "rr" and hedge ct; SHIFT LON/LAT
           c("BRADLEY AVE","REDWOOD DR") # bradley and "hedge ct"; SHIFT LON
           )

rob.2 <- makePoints(ildot,s)
## brad <- getPoint(ildot,"BRADLEY AVE","GARDEN HILLS DR")[2]
## find lat with lon matched to Mattis
mattis <- getPoint(ildot,"MATTIS AVE","GLEN BURNIE DR")[1]
brad <- getPoint(ildot,"BRADLEY AVE","BRADLEY AVE")
index <- which(abs(mattis - brad[,1])<=0.00005)
bradley.mattis <- brad[index,]
rob.2 <- shiftPoint(pts=rob.2,shift=list(1,bradley.mattis[2]),lon=FALSE)
## shift lat closer to RR
rr <- getPoint(ildot,"MATTIS AVE","HEDGEROAD DR")[2]
rr <- rr*(3/4) + getPoint(ildot,"MATTIS AVE","PAULA DR")[2]*(1/4)
rob.2 <- shiftPoint(pts=rob.2,shift=list(2,rr),lon=FALSE)
rr <- getPoint(ildot,"HEDGE CT","HEDGEROAD DR")
## two points - pick highest one
rr <- rr[which.max(rr[,2]),]
## shift lat closer to RR
rr.lat <- (rr[2])*(3/4) + (getPoint(ildot,"LARKSPUR LN","PAULA DR")[2])*(1/4)
rob.2 <- shiftPoint(pts=rob.2,shift=list(3,rr.lat),lon=FALSE)
## split lon btwn hedge ct and redwood dr to catch houses on east side of hedge ct
rr.lon <- (rr[1] + getPoint(ildot,"REDWOOD DR","NORTHWOOD DR")[1])/2
rob.2 <- shiftPoint(pts=rob.2,shift=list(c(3,4),rr.lon))


rob.2 <- makePolygon(rob.2,
                     poly.id="ROBESON.2",
                     proj=proj4string(ildot))

rob.2 <- makeSPDF(rob.2,data.frame(poly.data,School.Name="ROBESON"))

## area west of Staley, bwn Kirby and Windsor
## REMARK: once GH done, can clip west border wrt cusd4 shapefile (see below)
s <- rbind(c("KIRBY AVE","GLENSHIRE DR"), # kirby (aka 1500 N) and staley; SHIFT LON
           c("RISING RD (700 E)","KIRBY AVE (1500 N)"), # kirby and rising
           c("WINDSOR RD","RISING RD (700 E)"), # windsor and rising
           c("STALEY RD","WINDSOR RD") # windsor (aka 1400 N) and staley
           )

rob.3 <- makePoints(ildot,s)
staley <- getPoint(ildot,"STALEY RD","AMHERST DR")[1]
rob.3 <- shiftPoint(pts=rob.3,shift=list(1,staley))

rob.3 <- makePolygon(rob.3,
                    poly.id="ROBESON.3",
                    proj=proj4string(ildot))

rob.3 <- makeSPDF(rob.3,data.frame(poly.data,School.Name="ROBESON"))

rob <- spRbind(rob.1,spRbind(rob.2,rob.3))

save(rob.1,rob.2,rob.3,file=paste0(path.to.data,"Maps/CUSD4/rob.Rda"))


## Carrie Busey

## area near Centennial HS
s <- rbind(c("MATTIS AVE","WINDSOR RD"), # windsor and mattis
           c("MATTIS AVE","SOUTHWOOD DR"), # southwood and mattis
           c("PARKSIDE TERR","SOUTHWOOD DR"), # southwood and parkside terr
           c("CLOVER LN","BROADMOOR DR"), # southwood and clover; SHIFT LAT
           c("SOUTHWOOD CT","SOUTHWOOD DR"), # southwood and southwood ct
           c("CRESCENT DR","SOUTHWOOD DR"), # southwood and crescent
           c("CRESCENT DR","RODNEY DR"), # rodney and crescent
           c("CRESCENT DR","PARKDALE DR"), # parkdale and crescent
           c("CRESCENT DR","KIRBY AVE"), # kirby and crescent
           c("CRESCENT DR","LAWNDALE DR"), # lawndale and crescent
           c("CRESCENT DR","GREENDALE DR"), # greendale and crescent
           c("CRESCENT DR","SANGAMON DR"), # sangamon and crescent
           c("MATTIS AVE","SANGAMON DR"), # sangamon and mattis
           c("MATTIS AVE","SANGAMON DR"), # sangamon and mayfair rd; SHIFT LON/LAT
           c("MAYWOOD DR","MAYFAIR RD"), # maywood and mayfair
           c("MAYWOOD DR","MAYFAIR RD"), # maywood and maywood; SHIFT LON
           c("BELMEADE DR","MAYWOOD DR"), # maywood and belmeade
           c("BELMEADE DR","PARKVIEW DR"), # belmeade and belmeade; SHIFT LAT
           c("BELMEADE DR","PARKVIEW DR"), # parkview and belmeade
           c("PARKVIEW DR","WAVERLY DR"), # parkview and "russell"; SHIFT LON
           c("CARRIAGE WAY","KIRBY AVE"), # "parkview" and "carriage way"; SHIFT LAT
           c("CARRIAGE WAY","KIRBY AVE"), # kirby and carriage way
           c("KIRBY AVE","LINCOLN RD"), # kirby and lincoln
           c("KIRBY AVE","LINCOLN RD"), # lincholnshire and lincoln; shift LON/LAT
           c("KIRBY AVE","LINCOLN RD"), # lincholnshire and lincoln; shift LON/LAT
           c("LINCOLN RD","LINCOLN RD"), # lincoln and lincoln
           c("FOOT HILL DR","LINCOLN RD"), # foothill and lincoln
           c("BROADMOOR DR","LINCOLN RD"), # broadmoor and lincoln
           ## c("ROBERT DR","BROADMOOR DR"), # broadmoor and rober
           c("ROBERT DR","BROADMOOR DR"), # broadmoor and robert; SHIFT LON?
           c("STILLWATER DR","WINDSOR RD") # windsor and stillwater; SHIFT LON?
           )

cb.1 <- makePoints(ildot,s)
swood <- getPoint(ildot,"PARKSIDE TERR","SOUTHWOOD DR")[2]
cb.1 <- shiftPoint(cb.1,shift=list(4,swood),lon=FALSE)
mayfair <- getPoint(ildot,"MAYFAIR RD","MAYFAIR RD")[2,]
cb.1 <- shiftPoint(cb.1,shift=list(14,mayfair[1]))
cb.1 <- shiftPoint(cb.1,shift=list(14,mayfair[2]),lon=FALSE)
maywood <- getPoint(ildot,"BELMEADE DR","MAYWOOD DR")[1]
cb.1 <- shiftPoint(cb.1,shift=list(16,maywood))
cb.1 <- shiftPoint(cb.1,shift=list(18,mayfair[2]),lon=FALSE)
russ <- getPoint(ildot,"RUSSELL ST","COUNTRY LN")[1]
cb.1 <- shiftPoint(cb.1,shift=list(20,russ))
park <- getPoint(ildot,"PARKVIEW DR","WAVERLY DR")[2]
cb.1 <- shiftPoint(cb.1,shift=list(21,park),lon=FALSE)
shire <- getPoint(ildot,"LINCOLNSHIRE DR","LINCOLN RD")
cb.1 <- shiftPoint(cb.1,shift=list(24,shire[2,1]))
cb.1 <- shiftPoint(cb.1,shift=list(24,shire[2,2]),lon=FALSE)
cb.1 <- shiftPoint(cb.1,shift=list(25,shire[1,1]))
cb.1 <- shiftPoint(cb.1,shift=list(25,shire[1,2]),lon=FALSE)
still <- getPoint(ildot,"STILLWATER DR","WINDSOR RD")[1]
still <- (still + getPoint(ildot,"ROBERT DR","BROADMOOR DR")[1])/2
cb.1 <- shiftPoint(cb.1,shift=list(c(29,30),still))

cb.1 <- makePolygon(cb.1,
                    poly.id="CARRIE.BUSEY.1",
                    proj=proj4string(ildot))

cb.1 <- makeSPDF(cb.1,data.frame(poly.data,School.Name="CARRIE.BUSEY"))

## area south of Bradley, east of RR
## REMARK: can clip east border later
s <- rbind(c("CLOCK ST","BRADLEY AVE"), # "rr" & "clock"; SHIFT LAT
           c("HOLTS DR","FOURTH ST"), # "north" and fourth; SHIFT LAT
           c("EUREKA ST","FOURTH ST"), # eureka and fourth
           c("SIXTH ST","EUREKA ST"), # eureka and "wright"; SHIFT LON
           ## c("EUREKA ST","GOODWIN AVE"), # eureka and goodwin
           c("HILL ST","WRIGHT ST"), # W hill and wright; can interp bwn church and wright
           c("COLUMBIA AVE","MARKET ST") # "railroad crossing"; SHIFT LON; can shift LAT north...
           )

cb.2 <- makePoints(ildot,s)
rr <- getPoint(ildot,"NORTH ST","CHAMPAIGN ST")[2]
cb.2 <- shiftPoint(cb.2,shift=list(c(1,2),rr),lon=FALSE)
wright <- getPoint(ildot,"GROVE ST","WRIGHT ST")[1]
cb.2 <- shiftPoint(cb.2,shift=list(4,wright))
first <- getPoint(ildot,"FIRST ST","WASHINGTON ST")[1]
cb.2 <- shiftPoint(cb.2,shift=list(6,first))
rr <- round(getPoint(ildot,"COLUMBIA AVE","MARKET ST")[2],4)
cb.2 <- shiftPoint(cb.2,shift=list(6,rr),lon=FALSE)

cb.2 <- makePolygon(cb.2,
                    poly.id="CARRIE.BUSEY.2",
                    proj=proj4string(ildot))

cb.2 <- makeSPDF(cb.2,data.frame(poly.data,School.Name="CARRIE.BUSEY"))

## area south of Windsor, west of Dunlap
s <- rbind(c("SUNFLOWER ST","S FIRST ST"), # old chursh and first; SHIFT LAT
           c("CHURCH ST","DUNLAP AVE"), # old church and neil (dunlap RD)
           c("COLLEGE PK CT","DUNLAP AVE"), # "park ln" and dunlap; SHIFT LAT
           c("PARK LANE DR","PROSPECT AVE"), # park ln and prospect
           c("KENT DR","PROSPECT AVE"), # windsor and prospect; SHIFT LAT
           c("STILLWATER DR","WINDSOR RD"), # windsor and stillwater; SHIFT LON?
           c("STILLWATER DR","WINDSOR RD"), # 1000 N and "stillwater"; SHIFT LON/LAT           
           c("SUNFLOWER ST","S FIRST ST") # 100 N and first; SHIFT LAT
           )

cb.3 <- makePoints(ildot,s)
church <- getPoint(ildot,"OLD CHURCH RD","PARSLEY DR")[2,2]
cb.3 <- shiftPoint(cb.3,shift=list(c(1,2),church),lon=FALSE)
park <- getPoint(ildot,"PARK LANE DR","PROSPECT AVE")[2]
cb.3 <- shiftPoint(cb.3,shift=list(3,park),lon=FALSE)
wind <- getPoint(ildot,"MELROSE DR","WINDSOR RD")[2]
cb.3 <- shiftPoint(cb.3,shift=list(5,wind),lon=FALSE)
still <- getPoint(ildot,"STILLWATER DR","WINDSOR RD")[1]
still <- (still + getPoint(ildot,"ROBERT DR","BROADMOOR DR")[1])/2
cb.3 <- shiftPoint(cb.3,shift=list(6,still))
max.s <- bbox(cusd4)[2,1] # lowest lat
cb.3 <- shiftPoint(cb.3,shift=list(c(7,8),max.s),lon=FALSE)

cb.3 <- makePolygon(cb.3,
                    poly.id="CARRIE.BUSEY.3",
                    proj=proj4string(ildot))

cb.3 <- makeSPDF(cb.3,data.frame(poly.data,School.Name="CARRIE.BUSEY"))

cb <- spRbind(cb.3,spRbind(cb.1,cb.2))

save(cb.1,cb.2,cb.3,file=paste0(path.to.data,"Maps/CUSD4/cb.Rda"))


## Westview

## area east of Centennial HS
s <- rbind(c("CRESCENT DR","SANGAMON DR"), # sangamon and crescent
           c("MATTIS AVE","SANGAMON DR"), # sangamon and mattis
           c("MATTIS AVE","SANGAMON DR"), # sangamon and mayfair rd; SHIFT LON/LAT
           c("MAYWOOD DR","MAYFAIR RD"), # maywood and mayfair
           c("MAYWOOD DR","MAYFAIR RD"), # maywood and maywood; SHIFT LON
           c("BELMEADE DR","MAYWOOD DR"), # maywood and belmeade
           c("BELMEADE DR","PARKVIEW DR"), # belmeade and belmeade; SHIFT LAT
           c("BELMEADE DR","PARKVIEW DR"), # parkview and belmeade
           c("PARKVIEW DR","WAVERLY DR"), # parkview and "russell"; SHIFT LON
           c("CARRIAGE WAY","KIRBY AVE"), # "parkview" and "carriage way"; SHIFT LAT
           c("ARMORY AVE","PROSPECT AVE"), # armory and prospect; NB: crossing over park
           c("SPRINGFIELD AVE","PROSPECT AVE"), # springfield and prospect
           c("ROUND BARN RD","COUNTRY FAIR DR"), # springfield and country fair; SHIFT LAT
           c("JOHN ST","COUNTRY FAIR DR"), # john and country fair
           c("CRESCENT DR","JOHN ST") # john and crescent
           )

wv.1 <- makePoints(ildot,s)

mayfair <- getPoint(ildot,"MAYFAIR RD","MAYFAIR RD")[2,]
wv.1 <- shiftPoint(wv.1,shift=list(3,mayfair[1]))
wv.1 <- shiftPoint(wv.1,shift=list(3,mayfair[2]),lon=FALSE)
maywood <- getPoint(ildot,"BELMEADE DR","MAYWOOD DR")[1]
wv.1 <- shiftPoint(wv.1,shift=list(5,maywood))
wv.1 <- shiftPoint(wv.1,shift=list(7,mayfair[2]),lon=FALSE)
russ <- getPoint(ildot,"RUSSELL ST","COUNTRY LN")[1]
wv.1 <- shiftPoint(wv.1,shift=list(9,russ))
park <- getPoint(ildot,"PARKVIEW DR","WAVERLY DR")[2]
wv.1 <- shiftPoint(wv.1,shift=list(10,park),lon=FALSE)
spring <- getPoint(ildot,"SPRINGFIELD AVE","KENWOOD RD")[2]
wv.1 <- shiftPoint(wv.1,shift=list(13,spring),lon=FALSE)

wv.1 <- makePolygon(wv.1,
                    poly.id="WESTVIEW.1",
                    proj=proj4string(ildot))

wv.1 <- makeSPDF(wv.1,data.frame(poly.data,School.Name="WESTVIEW"))

## area south of Windsor
## REMARK: once GH done, can clip west/south borders wrt cusd 4 shapefile (see below)
s <- rbind(c("BARKER RD (600 E)","WINDSOR RD"), # windsor and barker
           ## c("WINDSOR RD","RISING RD (700 E)"), # windsor and rising
           c("STALEY RD","WINDSOR RD"), # windsor (aka 1400 N) and staley
           c("LAKEWOOD DR","WINDSOR RD"), # windsor and duncan (aka 900 E); SHIFT LON
           c("MATTIS AVE","WINDSOR RD"), # windsor and mattis
           c("STILLWATER DR","WINDSOR RD"), # windsor and stillwater; SHIFT LON?
           c("STILLWATER DR","WINDSOR RD"), # 1000 N and "stillwater"; SHIFT LON/LAT
           c("1100 N","BARKER RD (600 E)") # 110 N and barker; SHIFT LAT
           ## c("1100 N","RISING RD (700 E)") # 1000 N and rising; SHIFT LAT
           )

wv.2 <- makePoints(ildot,s)
duncan <- getPoint(ildot,"CRESTRIDGE DR","DUNCAN RD (900 E)")[1]
wv.2 <- shiftPoint(pts=wv.2,shift=list(3,duncan))
still <- getPoint(ildot,"STILLWATER DR","WINDSOR RD")[1]
still <- (still + getPoint(ildot,"ROBERT DR","BROADMOOR DR")[1])/2
wv.2 <- shiftPoint(wv.2,shift=list(c(5,6),still))
max.s <- bbox(cusd4)[2,1] # lowest lat
wv.2 <- shiftPoint(wv.2,shift=list(c(6,7),max.s),lon=FALSE)

wv.2 <- makePolygon(wv.2,
                    poly.id="WESTVIEW.2",
                    proj=proj4string(ildot))

wv.2 <- makeSPDF(wv.2,data.frame(poly.data,School.Name="WESTVIEW"))

## small area north of Bradley, east of RR
## REMARK: clip later
s <- rbind(c("BRADLEY AVE","FOURTH ST"), # bradley and fourth
           c("BRADLEY AVE","CHESTNUT ST"), # bradley & "rr";           
           c("CHESTNUT ST","BELLEFONTAINE ST"), # bellefontaine and "rr"; SHIFT LON
           ## c("CRISPUS DR","SIXTH ST"), # sixth and crispus; SHIFT LAT
           c("CHESTNUT ST","BELLEFONTAINE ST"), # "bellefontaine" and sixth; SHIFT LON(west)
           c("BRADLEY AVE","SIXTH ST"), # bradley and sixth; SHIFT LON(west)
           c("BRADLEY AVE","CARVER DR"), # bradley and "wright"; SHIFT LON
           c("SIXTH ST","EUREKA ST"), # eureka and "wright"; SHIFT LON
           c("EUREKA ST","FOURTH ST") # eureka and fourth
           )

wv.3 <- makePoints(ildot,s)
## interpolated between kenyon/oak and bradley/chestnut; see ss.3 above
bf <- getPoint(ildot,"CHESTNUT ST","BELLEFONTAINE ST")[2]
index <- which(abs(bf - interp[[2]])<=0.00005)
bf.lon <- interp[[1]][index]
wv.3 <- shiftPoint(wv.3,shift=list(3,bf.lon))
burch <- getPoint(ildot,"BRADLEY AVE","SIXTH ST")[1]
burch <- burch*(3/4) + getPoint(ildot,"BRADLEY AVE","FIFTH ST")[1]*(1/4)
wv.3 <- shiftPoint(wv.3,shift=list(c(4,5),burch))
wright <- getPoint(ildot,"GROVE ST","WRIGHT ST")[1]
wv.3 <- shiftPoint(wv.3,shift=list(c(6,7),wright))

wv.3 <- makePolygon(wv.3,
                    poly.id="WESTVIEW.3",
                    proj=proj4string(ildot))

wv.3 <- makeSPDF(wv.3,data.frame(poly.data,School.Name="WESTVIEW"))

wv <- spRbind(wv.1,spRbind(wv.2,wv.3))

save(wv.1,wv.2,wv.3,file=paste0(path.to.data,"Maps/CUSD4/wv.Rda"))


## Bottenfield

## south-east region (including Campus town + west of RR)
## REMARK: can extend east wrt bbox, then clip using cusd4 map
s <- rbind(c("STILLWATER DR","WINDSOR RD"), # windsor and stillwater; SHIFT LON?
           c("ROBERT DR","BROADMOOR DR"), # broadmoor and robert; SHIFT LON?
           c("BROADMOOR DR","LINCOLN RD"), # broadmoor and lincoln
           c("FOOT HILL DR","LINCOLN RD"), # foothill and lincoln
           c("LINCOLN RD","LINCOLN RD"), # lincoln and lincoln
           c("KIRBY AVE","LINCOLN RD"), # lincholnshire and lincoln; shift LON/LAT
           c("KIRBY AVE","LINCOLN RD"), # lincholnshire and lincoln; shift LON/LAT
           c("KIRBY AVE","LINCOLN RD"), # kirby and lincoln
           c("CARRIAGE WAY","KIRBY AVE"), # kirby and carriage way
           c("CARRIAGE WAY","KIRBY AVE"), # "parkview" and "carriage way"; SHIFT LAT
           c("ARMORY AVE","PROSPECT AVE"), # armory and prospect; NB: crossing over park
           c("KIRBY AVE","PROSPECT AVE"), # kirby and prospect
           c("KIRBY AVE","NEIL ST"), # kirby and neil
           c("STANAGE AVE","NEIL ST"), # stanage and neil
           c("UNIVERSITY AVE","WATER ST"), # university and water; SHIFT LON WEST
           c("COLUMBIA AVE","MARKET ST"), # "railroad crossing"; SHIFT LON; can shift LAT north...
           c("HILL ST","WRIGHT ST"), # W hill and wright; can interp bwn church and wright
           c("KIRBY AVE","FLORIDA AVE"), # florida and "wright"
           c("BUSEY AVE","FLORIDA AVE"), # florida and lincoln; SHIFT LON
           c("LINCOLN AVE","ST MARY'S RD"), # windsor and lincoln; SHIFT LAT
           c("CURTIS RD","1350 E"), # curtis and lincon; SHIFT LON
           c("CURTIS RD","1350 E"), # curtis and "wright"; SHIFT LON
           c("OLD CHURCH RD","OLD CHURCH RD(1200N)"), # old church and "wright"; SHIFT LON
           c("SUNFLOWER ST","S FIRST ST"), # old chursh and first; SHIFT LAT
           c("CHURCH ST","DUNLAP AVE"), # old church and neil (dunlap RD)
           c("COLLEGE PK CT","DUNLAP AVE"), # "park ln" and dunlap; SHIFT LAT
           c("PARK LANE DR","PROSPECT AVE"), # park ln and prospect
           c("KENT DR","PROSPECT AVE") # windsor and prospect; SHIFT LAT
           )

bot.1 <- makePoints(ildot,s)
still <- getPoint(ildot,"STILLWATER DR","WINDSOR RD")[1]
still <- (still + getPoint(ildot,"ROBERT DR","BROADMOOR DR")[1])/2
bot.1 <- shiftPoint(bot.1,shift=list(c(1,2),still))
shire <- getPoint(ildot,"LINCOLNSHIRE DR","LINCOLN RD")
bot.1 <- shiftPoint(bot.1,shift=list(6,shire[1,1]))
bot.1 <- shiftPoint(bot.1,shift=list(6,shire[1,2]),lon=FALSE)
bot.1 <- shiftPoint(bot.1,shift=list(7,shire[2,1]))
bot.1 <- shiftPoint(bot.1,shift=list(7,shire[2,2]),lon=FALSE)
park <- getPoint(ildot,"PARKVIEW DR","WAVERLY DR")[2]
bot.1 <- shiftPoint(bot.1,shift=list(10,park),lon=FALSE)
west <- getPoint(ildot,"CHESTER ST","MARKET ST")[1]
west <- west*(1/3) + (getPoint(ildot,"UNIVERSITY AVE","WATER ST")[1])*(2/3)
bot.1 <- shiftPoint(bot.1,shift=list(15,west))
first <- getPoint(ildot,"FIRST ST","WASHINGTON ST")[1]
bot.1 <- shiftPoint(bot.1,shift=list(16,first))
rr <- round(getPoint(ildot,"COLUMBIA AVE","MARKET ST")[2],4)
bot.1 <- shiftPoint(bot.1,shift=list(16,rr),lon=FALSE)
linc <- getPoint(ildot,"DELAWARE AVE","LINCOLN AVE")[1]
bot.1 <- shiftPoint(bot.1,shift=list(19,linc))
linc <- getPoint(ildot,"WINDSOR RD","RACE ST")[2]
bot.1 <- shiftPoint(bot.1,shift=list(20,linc),lon=FALSE)
linc <- getPoint(ildot,"LINCOLN AVE","ST MARY'S RD")[1]
bot.1 <- shiftPoint(bot.1,shift=list(21,linc))
wright <- getPoint(ildot,"WRIGHT ST","ARMORY AVE")[1]
bot.1 <- shiftPoint(bot.1,shift=list(c(22,23),wright))
church <- getPoint(ildot,"OLD CHURCH RD","PARSLEY DR")[2,2]
bot.1 <- shiftPoint(bot.1,shift=list(c(24,25),church),lon=FALSE)
park <- getPoint(ildot,"PARK LANE DR","PROSPECT AVE")[2]
bot.1 <- shiftPoint(bot.1,shift=list(26,park),lon=FALSE)
wind <- getPoint(ildot,"MELROSE DR","WINDSOR RD")[2]
bot.1 <- shiftPoint(bot.1,shift=list(28,wind),lon=FALSE)

bot.1 <- makePolygon(bot.1,
                    poly.id="BOTTENFIELD.1",
                    proj=proj4string(ildot))

bot.1 <- makeSPDF(bot.1,data.frame(poly.data,School.Name="BOTTENFIELD"))

save(bot.1,file=paste0(path.to.data,"Maps/CUSD4/bot.Rda"))


## Howard

## area bwn Bradley and Springfield, west of Prospect
s <- rbind(c("DUNCAN RD (900 E)","RIDGEWOOD CT"), # springfield and duncan; SHIFT LAT
           c("MITCHELL CT","S MITCHELL CT"), # "o'malley's alley" and mitchell ct
           c("SPRINGFIELD AVE","KENWOOD RD"), # austin and kenwood; SHIFT LAT
           c("SPRINGFIELD AVE","KENWOOD RD"), # springfield and kenwood
           c("ROUND BARN RD","COUNTRY FAIR DR"), # springfield and country fair; SHIFT LAT
           c("SPRINGFIELD AVE","PROSPECT AVE"), # springfield and prospect
           c("UNIVERSITY AVE","PROSPECT AVE"), # university and prospect
           c("BRADLEY AVE","PROSPECT AVE"), # bradley and "rr"; SHIFT LAT
           c("HEDGEROAD DR","HEDGEROAD DR"), # "rr" and hedge ct; SHIFT LON/LAT
           c("BRADLEY AVE","REDWOOD DR"), # bradley and "hedge ct"; SHIFT LON
           c("MATTIS AVE","GLEN BURNIE DR"), # bradley and mattis; SHIFT LAT
           c("BRADLEY AVE","CLAYTON BLVD"), # bradley and "copper slough"; SHIFT LON/LAT
           c("CLARK RD","HUNDMAN DR") # lat ~ clark and hundman; SHIFT LON
           ## REMARK: from here to springfield/duncan too flat but area commercial; could shift LAT..
           )

howard <- makePoints(ildot,s)
## remark: not exact but area commercial + apts...
spring <- getPoint(ildot,"SPRINGFIELD AVE","KENWOOD RD")[2]
howard <- shiftPoint(howard,shift=list(1,spring),lon=FALSE)
austin <- getPoint(ildot,"MITCHELL CT","S MITCHELL CT")[2]
howard <- shiftPoint(howard,shift=list(3,austin),lon=FALSE)
spring <- getPoint(ildot,"SPRINGFIELD AVE","KENWOOD RD")[2]
howard <- shiftPoint(howard,shift=list(5,spring),lon=FALSE)
rr <- getPoint(ildot,"BRADLEY AVE","PROSPECT AVE")[2]
rr <- (rr + getPoint(ildot,"DENNISON DR","PROSPECT AVE")[2])/2
howard <- shiftPoint(howard,shift=list(8,rr),lon=FALSE)
rr <- getPoint(ildot,"HEDGE CT","HEDGEROAD DR")
## two points - pick highest one
rr <- rr[which.max(rr[,2]),]
## shift lat closer to RR
rr.lat <- (rr[2])*(3/4) + (getPoint(ildot,"LARKSPUR LN","PAULA DR")[2])*(1/4)
howard <- shiftPoint(pts=howard,shift=list(9,rr.lat),lon=FALSE)
## split lon btwn hedge ct and redwood dr to catch houses on east side of hedge ct
rr.lon <- (rr[1] + getPoint(ildot,"REDWOOD DR","NORTHWOOD DR")[1])/2
howard <- shiftPoint(pts=howard,shift=list(c(9,10),rr.lon))
## bradley and mattis lon: see rob.2 above
howard <- shiftPoint(pts=howard,shift=list(11,bradley.mattis[2]),lon=FALSE)
brad <- (bradley.mattis[2] + getPoint(ildot,"BRADLEY AVE","CLAYTON BLVD")[2])/2
howard <- shiftPoint(pts=howard,shift=list(12,brad),lon=FALSE)
brad <- getPoint(ildot,"SPRINGFIELD AVE","KENWOOD RD")[1]
howard <- shiftPoint(pts=howard,shift=list(c(12,13),brad))

howard <- makePolygon(howard,
                     poly.id="HOWARD.1",
                     proj=proj4string(ildot))

howard <- makeSPDF(howard,data.frame(poly.data,School.Name="HOWARD"))

save(howard,file=paste0(path.to.data,"Maps/CUSD4/howard.Rda"))


## Kenwood

## area west of Centennial HS to Rising (clip later), btwn Springfield and Kirby
## REMARK: once GH done, clip west border wrt cusd4 shapefile (see below)
s <- rbind(c("DUNCAN RD (900 E)","RIDGEWOOD CT"), # springfield and duncan; SHIFT LAT
           c("MITCHELL CT","S MITCHELL CT"), # "o'malley's alley" and mitchell ct
           c("SPRINGFIELD AVE","KENWOOD RD"), # austin and kenwood; SHIFT LAT
           c("SPRINGFIELD AVE","KENWOOD RD"), # springfield and kenwood
           c("ROUND BARN RD","COUNTRY FAIR DR"), # springfield and country fair; SHIFT LAT
           c("JOHN ST","COUNTRY FAIR DR"), # john and country fair
           c("CRESCENT DR","JOHN ST"), # john and crescent
           c("CRESCENT DR","SANGAMON DR"), # sangamon and crescent
           c("CRESCENT DR","KIRBY AVE"), # kirby and crescent
           c("DUNCAN RD (900 E)","KIRBY AVE"), # kirby and duncan
           c("KIRBY AVE","GLENSHIRE DR"), # kirby (aka 1500 N) and staley; SHIFT LON
           c("RISING RD (700 E)","KIRBY AVE (1500 N)"), # kirby and rising
           c("GLEN ABBEY DR","OAKDALE DR"), # oakdale and glenn abbey
           c("TIFFANY CT","SPRINGFIELD AVE") # springfield and staley; SHIFT LON
           )

kw.1 <- makePoints(ildot,s)
## remark: not exact but area commercial + apts...
spring <- getPoint(ildot,"SPRINGFIELD AVE","KENWOOD RD")[2]
kw.1 <- shiftPoint(kw.1,shift=list(1,spring),lon=FALSE)
austin <- getPoint(ildot,"MITCHELL CT","S MITCHELL CT")[2]
kw.1 <- shiftPoint(kw.1,shift=list(3,austin),lon=FALSE)
spring <- getPoint(ildot,"SPRINGFIELD AVE","KENWOOD RD")[2]
kw.1 <- shiftPoint(kw.1,shift=list(5,spring),lon=FALSE)
staley <- getPoint(ildot,"STALEY RD","AMHERST DR")[1]
kw.1 <- shiftPoint(pts=kw.1,shift=list(11,staley))
staley <- getPoint(ildot,"STALEY RD","KEARNS DR (1580N)")[1]
kw.1 <- shiftPoint(pts=kw.1,shift=list(14,staley))


kw.1 <- makePolygon(kw.1,
                    poly.id="KENWOOD.1",
                    proj=proj4string(ildot))

kw.1 <- makeSPDF(kw.1,data.frame(poly.data,School.Name="KENWOOD"))


## small area south of Bradley, east of RR
s <- rbind(c("CLOCK ST","BRADLEY AVE"), # "rr" & "clock"; SHIFT LAT
           c("BRADLEY AVE","CHESTNUT ST"), # bradley & "rr"; SHIFT LON??
           c("BRADLEY AVE","FOURTH ST"), # bradley and fourth
           c("EUREKA ST","FOURTH ST"), # eureka and fourth
           c("HOLTS DR","FOURTH ST") # "north" and fourth; SHIFT LAT
           )

kw.2 <- makePoints(ildot,s)
rr <- getPoint(ildot,"NORTH ST","CHAMPAIGN ST")[2]
kw.2 <- shiftPoint(kw.2,shift=list(c(1,5),rr),lon=FALSE)

kw.2 <- makePolygon(kw.2,
                    poly.id="KENWOOD.2",
                    proj=proj4string(ildot))

kw.2 <- makeSPDF(kw.2,data.frame(poly.data,School.Name="KENWOOD"))

## small area north of Bradley, east of RR
## REMARK: Clip east border later later
s <- rbind(c("BRADLEY AVE","GOODWIN AVE"), # bradley and goodwin
           c("BRADLEY AVE","GOODWIN AVE"), # "bellefontaine" and goodwin; SHIFT LAT
           c("GROVE ST","WRIGHT ST"), # "bellefontaine" and "wright"; SHIFT LAT
           c("GROVE ST","WRIGHT ST"), # "juniper" and "wright"; LAT
           c("BRADLEY AVE","SIXTH ST"), # "juniper" and "sixth"; SHIFT LON/LAT
           c("CHESTNUT ST","BELLEFONTAINE ST"), # "bellefontaine" and sixth; SHIFT LON(west)
           c("BRADLEY AVE","SIXTH ST"), # bradley and sixth; SHIFT LON(west)
           c("BRADLEY AVE","CARVER DR") # bradley and "wright"; SHIFT LON
           )

kw.3 <- makePoints(ildot,s)
bf <- getPoint(ildot,"CHESTNUT ST","BELLEFONTAINE ST")[2]
kw.3 <- shiftPoint(kw.3,shift=list(c(2,3),bf),lon=FALSE)
juniper <- getPoint(ildot,"KENYON RD","NEIL ST")[2]
juniper <- (juniper + getPoint(ildot,"LEICHNER DR","HICKORY ST")[2])/2
kw.3 <- shiftPoint(kw.3,shift=list(c(4,5),juniper),lon=FALSE)
burch <- getPoint(ildot,"BRADLEY AVE","SIXTH ST")[1]
burch <- burch*(3/4) + getPoint(ildot,"BRADLEY AVE","FIFTH ST")[1]*(1/4)
kw.3 <- shiftPoint(kw.3,shift=list(c(5,6,7),burch))
wright <- getPoint(ildot,"GROVE ST","WRIGHT ST")[1]
kw.3 <- shiftPoint(kw.3,shift=list(8,wright))

kw.3 <- makePolygon(kw.3,
                    poly.id="KENWOOD.3",
                    proj=proj4string(ildot))

kw.3 <- makeSPDF(kw.3,data.frame(poly.data,School.Name="KENWOOD"))


kw <- spRbind(kw.1,spRbind(kw.2,kw.3))

save(kw.1,kw.2,kw.3,file=paste0(path.to.data,"Maps/CUSD4/kw.Rda"))

## --------------------------------------------------------------------------- #
## Use rgeos::gDifference to fix borders
## first, GH is set diff from cusd4 and the preceding areas
## then fix the rest

## Garden Hills

merged <- spRbind(howard,spRbind(bot.1,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb)))))))

merged <- gUnaryUnion(merged)

gh.1 <- gDifference(cusd4,merged)
gh.1 <- coordinates(as(gh.1,"SpatialLines"))[[1]]

index <- unlist(lapply(gh.1,length))
index <- which.max(index)
gh.1 <- gh.1[[index]]

gh.1 <- makePolygon(gh.1,poly.id="GARDEN.HILLS.1",proj=proj4string(ildot))
gh.1 <- makeSPDF(gh.1,data.frame(poly.data,School.Name="GARDEN.HILLS"))

## Fix borders

## Bottenfield, revised east and south borders

merged <- spRbind(howard,spRbind(gh.1,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb)))))))

merged <- gUnaryUnion(merged)

bot.2 <- gDifference(cusd4,merged)
bot.2 <- coordinates(as(bot.2,"SpatialLines"))[[1]]

index <- unlist(lapply(bot.2,length))
index <- which.max(index)
bot.2 <- bot.2[[index]]

bot.2 <- makePolygon(bot.2,poly.id="BOTTENFIELD.2",proj=proj4string(ildot))
bot.2 <- makeSPDF(bot.2,data.frame(poly.data,School.Name="BOTTENFIELD"))

save(bot.2,file=paste0(path.to.data,"Maps/CUSD4/bot.Rda"))

## Robeson, revised west borders

rob <- spRbind(rob.1,rob.2)

merged <- spRbind(gh.1,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))

merged <- gUnaryUnion(merged)

rob.4 <- gDifference(cusd4,merged)
rob.4 <- coordinates(as(rob.4,"SpatialLines"))[[1]]

index <- unlist(lapply(rob.4,length))
index <- which.max(index)
rob.4 <- rob.4[[index]]

rob.4 <- makePolygon(rob.4,poly.id="ROBESON.4",proj=proj4string(ildot))
rob.4 <- makeSPDF(rob.4,data.frame(poly.data,School.Name="ROBESON"))

rob <- spRbind(rob,rob.4)
save(rob.1,rob.2,rob.4,file=paste0(path.to.data,"Maps/CUSD4/rob.Rda"))

## Westview, revised west and south borders

wv <- spRbind(wv.1,wv.3)

merged <- spRbind(gh.1,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))

merged <- gUnaryUnion(merged)

wv.4 <- gDifference(cusd4,merged)
wv.4 <- coordinates(as(wv.4,"SpatialLines"))[[1]]

index <- unlist(lapply(wv.4,length))
index <- which.max(index)
wv.4 <- wv.4[[index]]

wv.4 <- makePolygon(wv.4,poly.id="WESTVIEW.4",proj=proj4string(ildot))
wv.4 <- makeSPDF(wv.4,data.frame(poly.data,School.Name="WESTVIEW"))

wv <- spRbind(wv,wv.4)
save(wv.1,wv.3,wv.4,file=paste0(path.to.data,"Maps/CUSD4/wv.Rda"))

## Carrie Busey, revised east and south borders

cb <- spRbind(cb.1,cb.2)

merged <- spRbind(gh.1,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))

merged <- gUnaryUnion(merged)

cb.4 <- gDifference(cusd4,merged)
cb.4 <- coordinates(as(cb.4,"SpatialLines"))[[1]]

index <- unlist(lapply(cb.4,length))
index <- which.max(index)
cb.4 <- cb.4[[index]]

cb.4 <- makePolygon(cb.4,poly.id="CARRIE.BUSEY.4",proj=proj4string(ildot))
cb.4 <- makeSPDF(cb.4,data.frame(poly.data,School.Name="CARRIE.BUSEY"))

cb <- spRbind(cb,cb.4)
save(cb.1,cb.2,cb.4,file=paste0(path.to.data,"Maps/CUSD4/cb.Rda"))

## Kenwood, revised west borders

kw <- spRbind(kw.2,kw.3)

merged <- spRbind(gh.1,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))

merged <- gUnaryUnion(merged)

kw.4 <- gDifference(cusd4,merged)
kw.4 <- coordinates(as(kw.4,"SpatialLines"))[[1]]

index <- unlist(lapply(kw.4,length))
index <- which.max(index)
kw.4 <- kw.4[[index]]

kw.4 <- makePolygon(kw.4,poly.id="KENWOOD.4",proj=proj4string(ildot))
kw.4 <- makeSPDF(kw.4,data.frame(poly.data,School.Name="KENWOOD"))

kw <- spRbind(kw,kw.4)
save(kw.2,kw.3,kw.4,file=paste0(path.to.data,"Maps/CUSD4/kw.Rda"))

## Garden Hills 2
## grab missing square on north east corner

merged <- spRbind(gh.1,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))

merged <- gUnaryUnion(merged)

gh.2 <- gDifference(cusd4,merged)
gh.2 <- coordinates(as(gh.2,"SpatialLines"))[[1]]

index <- unlist(lapply(gh.2,length))
index <- which.max(index)
gh.2 <- gh.2[[index]]

gh.2 <- makePolygon(gh.2,poly.id="GARDEN.HILLS.2",proj=proj4string(ildot))
gh.2 <- makeSPDF(gh.2,data.frame(poly.data,School.Name="GARDEN.HILLS"))

gh <- spRbind(gh.1,gh.2)
save(gh.1,gh.2,file=paste0(path.to.data,"Maps/CUSD4/gh.Rda"))

## Kenwood, revised east border

kw <- spRbind(kw.2,kw.4)

merged <- spRbind(gh,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))

merged <- gUnaryUnion(merged)

kw.5 <- gDifference(cusd4,merged)
kw.5 <- coordinates(as(kw.5,"SpatialLines"))[[1]]

index <- unlist(lapply(kw.5,length))
index <- which.max(index)
kw.5 <- kw.5[[index]]

kw.5 <- makePolygon(kw.5,poly.id="KENWOOD.5",proj=proj4string(ildot))
kw.5 <- makeSPDF(kw.5,data.frame(poly.data,School.Name="KENWOOD"))

kw <- spRbind(kw,kw.5)
save(kw.2,kw.4,kw.5,file=paste0(path.to.data,"Maps/CUSD4/kw.Rda"))

## Carrie Busey, revised east border

cb <- spRbind(cb.1,cb.4)

merged <- spRbind(gh,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))
merged <- gUnaryUnion(merged)

cb.5 <- gDifference(cusd4,merged)
cb.5 <- coordinates(as(cb.5,"SpatialLines"))[[1]]

index <- unlist(lapply(cb.5,length))
index <- which.max(index)
cb.5 <- cb.5[[index]]

cb.5 <- makePolygon(cb.5,poly.id="CARRIE.BUSEY.5",proj=proj4string(ildot))
cb.5 <- makeSPDF(cb.5,data.frame(poly.data,School.Name="CARRIE.BUSEY"))

cb <- spRbind(cb,cb.5)
save(cb.1,cb.4,cb.5,file=paste0(path.to.data,"Maps/CUSD4/cb.Rda"))

## Westview, revised east border

wv <- spRbind(wv.1,wv.4)

merged <- spRbind(gh,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))
merged <- gUnaryUnion(merged)

wv.5 <- gDifference(cusd4,merged)
wv.5 <- coordinates(as(wv.5,"SpatialLines"))[[1]]

index <- unlist(lapply(wv.5,length))
index <- which.max(index)
wv.5 <- wv.5[[index]]

wv.5 <- makePolygon(wv.5,poly.id="WESTVIEW.5",proj=proj4string(ildot))
wv.5 <- makeSPDF(wv.5,data.frame(poly.data,School.Name="WESTVIEW"))

wv <- spRbind(wv,wv.5)
save(wv.1,wv.4,wv.5,file=paste0(path.to.data,"Maps/CUSD4/wv.Rda"))

## --------------------------------------------------------------------------- #
## Combine all polygons into single shapefile

## combine
champaign.1989 <- spRbind(gh,spRbind(howard,spRbind(bot.2,spRbind(wv,spRbind(ss,spRbind(col,spRbind(rob,spRbind(kw,cb))))))))

save(champaign.1989,file=paste0(path.to.data,"Maps/CUSD4/champaign1989.Rda"))

## writeOGR to shapefile
writeOGR(champaign.1989,
         dsn=paste0(path.to.data,"Maps/CUSD4"),
         layer="champaign1989",
         driver="ESRI Shapefile")

## --------------------------------------------------------------------------- #
