## Attendance Zones in USD 116, 2012-present
## 2012-present map: see "Urbana 2013*.pdf"
## N.B.: Modified version of usd116zones2002.R

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
poly.data <- data.frame(Agency.Name="USD 116",Year="2012")

## --------------------------------------------------------------------------- #

## MODIFICATIONS from 2002:
## 1. montclair - florida between race and vine: from wiley.2 to yr.1
## 2. part of helmet: from wiley.3, leal.2 to prairie.3
## 3. part below helment: from wiley.3 to paine.4

## ONLY NEED TO RE-DO THE FOLLOWING:
## wiley.2
## wiley. 3
## yr.1
## leal.2
## load the rest AS IS from 2002

## --------------------------------------------------------------------------- #
## WILEY
load(file=paste0(path.to.data,"Maps/2002/wiley2002.Rda"),verbose=T)

## 2. {Montclair and Race; Montclair and Vine; Colorado and Vine; Colorado and Philo; Fairlawn and Philo; Washington and Philo; Washington and Vine; Florida and Vine; Florida and Race}
s <- rbind(c("COLORADO AVE","ANDERSON ST"), # shift lon: Vine/Montclair
           c("PHILO RD","COLORADO AVE"),
           c("PHILO RD","FAIRLAWN DR"),
           c("WASHINGTON ST","PHILO RD"),
           c("WASHINGTON ST","VINE ST"),
           c("FLORIDA AVE","ANDERSON ST")) # shift lon: Penn and Vine
           ## c("BROADWAY AVE","FLORIDA AVE")) # shift lon: Race and Delaware

## showCrossing(ildot,street="FLORIDA AVE",road1=TRUE)

## vine <- getPoint(ildot,"MICHIGAN AVE","VINE ST")[1]
vine <- getPoint(ildot,"VINE ST","MONTCLAIR RD")[1]
wiley.2 <- shiftPoint(makePoints(ildot,s),shift=list(1,vine))

vine <- getPoint(ildot,"VINE ST","PENNSYLVANIA AVE")[1]
wiley.2 <- shiftPoint(wiley.2,shift=list(6,vine))

## race <- getPoint(ildot,"DELAWARE AVE","RACE ST")[1]
## wiley.2 <- shiftPoint(wiley.2,shift=list(7,race))

wiley.2 <- makePolygon(wiley.2,
                       poly.id="WILEY.2",
                       proj=proj4string(ildot))

wiley.2 <- makeSPDF(wiley.2,data.frame(poly.data,School.Name="WILEY"))

## 3. see usd116zones2002.R for details
## for changes: see paine.4 below
## N.B.: barr may be too far south? looks okay...

s <- rbind(c("RACE ST","UNIVERSITY AVE"), # shift lat: BROADWAY AVE and CUNNINGHAM AVE
           c("PERKINS RD","RICHARD DR"), # shift lat: as above
           c("PERKINS RD","RICHARD DR"), # *shift lat: cunningham/barr
           c("CUNNINGHAM AVE","US 45 TO I-74EB"), # *shift lat: cunningham/barr
           c("CUNNINGHAM AVE","US 45 TO I-74EB"), # *shift lat: cunningham/thompson
           c("CUNNINGHAM AVE","THOMPSON ST"), # *shift lon: kerr/chief shemauger[2,]
           c("PERKINS RD","CARROLL AVE"), # *shift lon: kerr/chief shemauger[2,]
           c("PERKINS RD","CARROLL AVE"), # *shift lon: interpolate along cunningham
           c("CUNNINGHAM AVE","US 45 TO I-74EB"), # shift lat; see below
           c("HAGEN BLVD","KENYON RD"), # shift lat: as 74+Lincoln?
           c("LINCOLN TO I-74WB","LINCOLN TO I-74WB"), # shift lon: see next line; i-74WBtolin+lin
           c("BROADWAY AVE","COUNTRY CLUB RD"), # shift lon: (-88.21053,40.12884)
           c("BROADWAY AVE","FRANKLIN ST"), # shift lon: as above
           c("RACE ST","UNIVERSITY AVE"))

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
wiley.3 <- shiftPoint(makePoints(ildot,s),shift=list(1,race),lon=FALSE)
wiley.3 <- shiftPoint(wiley.3,shift=list(2,race),lon=FALSE)

## rich <- getPoint(ildot,"OLIVER ST","CLIFFORD ST")[2]
barr <- getPoint(ildot,"CUNNINGHAM AVE","BARR AVE")[2]
wiley.3 <- shiftPoint(wiley.3,shift=list(3,barr),lon=FALSE)
wiley.3 <- shiftPoint(wiley.3,shift=list(4,barr),lon=FALSE)

thom <- getPoint(ildot,"CUNNINGHAM AVE","THOMPSON ST")[2]
wiley.3 <- shiftPoint(wiley.3,shift=list(5,thom),lon=FALSE)

perk <- getPoint(ildot,"KERR AVE","CHIEF SHEMAUGER PARK")[2,1]
wiley.3 <- shiftPoint(wiley.3,shift=list(6,perk))
wiley.3 <- shiftPoint(wiley.3,shift=list(7,perk))

## interpolate
north <- getPoint(ildot,"CUNNINGHAM AVE","KENYON RD")
south <- getPoint(ildot,"CUNNINGHAM AVE","THOMPSON ST")
interp <- approx(rbind(north,south))

## match desired latitude
lat <- getPoint(ildot,"PERKINS RD","CARROLL AVE")[2]
index <- which(abs(lat-interp[[2]])<=0.00005)
interp.lon <- interp[[1]][index]
wiley.3 <- shiftPoint(wiley.3,shift=list(8,interp.lon))

us45 <- getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74WB")[2]
us45 <- (us45+getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74EB")[2])/2
wiley.3 <- shiftPoint(wiley.3,shift=list(9,us45),lon=FALSE)

i74 <- getPoint(ildot,"LINCOLN TO I-74WB","LINCOLN TO I-74WB")[2]
wiley.3 <- shiftPoint(wiley.3,shift=list(10,i74),lon=FALSE)

club <- getPoint(ildot,"COUNTRY CLUB RD","COUNTRY CLUB RD")[7,]
wiley.3 <- shiftPoint(wiley.3,shift=list(11,club[1]))
wiley.3 <- shiftPoint(wiley.3,shift=list(12,club[1]))
wiley.3 <- shiftPoint(wiley.3,shift=list(13,club[1]))

wiley.3 <- makePolygon(wiley.3,
                       poly.id="WILEY.3",
                       proj=proj4string(ildot))

wiley.3 <- makeSPDF(wiley.3,data.frame(poly.data,School.Name="WILEY"))

## change year to 2012
wiley.1$Year <- as.factor("2012")

## Combine
wiley <- spRbind(spRbind(wiley.1,wiley.2),wiley.3)

## save
save(wiley.1,wiley.2,wiley.3,file=paste0(path.to.data,"Maps/USD116/2012/wiley2012.Rda"))

## --------------------------------------------------------------------------- #
## KING
load(file=paste0(path.to.data,"Maps/2002/king2002.Rda"),verbose=T)

## change year to 2012
king.1$Year <- as.factor("2012")
king.2$Year <- as.factor("2012")

## combine
king <- spRbind(king.1,king.2)

## save
save(king.1,king.2,file=paste0(path.to.data,"Maps/USD116/2012/king2012.Rda"))

## --------------------------------------------------------------------------- #
## YANKEE RIDGE
load(file=paste0(path.to.data,"Maps/2002/yr2002.Rda"),verbose=T)

## 1. {Windsor and Philo; Windsor and Race; Montclair and Race; Montclair and Vine; Colorado and Vine; Colorado and Philo; Windsor and Philo}
s <- rbind(c("WINDSOR RD","PHILO RD"),
           c("WINDSOR RD","RACE ST"),
           c("BROADWAY AVE","FLORIDA AVE"), # REPLACE: race/florida
           c("FLORIDA AVE","ANDERSON ST"), # REPLACE: vine/florida
           c("COLORADO AVE","ANDERSON ST"), # shift lon: Vine/Montclair; see wiley.2
           c("PHILO RD","COLORADO AVE"))

## vine <- getPoint(ildot,"MICHIGAN AVE","VINE ST")[1]
vine <- getPoint(ildot,"VINE ST","MONTCLAIR RD")[1]
yr.1 <- shiftPoint(makePoints(ildot,s),shift=list(5,vine))

race <- getPoint(ildot,"DELAWARE AVE","RACE ST")[1]
yr.1 <- shiftPoint(yr.1,shift=list(3,race))

vine <- getPoint(ildot,"VINE ST","PENNSYLVANIA AVE")[1]
yr.1 <- shiftPoint(yr.1,shift=list(4,vine))
           
yr.1 <- makePolygon(yr.1,
                    poly.id="YANKEE.RIDGE.1",
                    proj=proj4string(ildot))

yr.1 <- makeSPDF(yr.1,data.frame(poly.data,School.Name="YANKEE.RIDGE"))

## change year to 2012
yr.2.1.2$Year <- as.factor("2012")

## Combine
yr <- spRbind(yr.1,yr.2.1.2)

## save
save(yr.1,yr.2.1.2,file=paste0(path.to.data,"Maps/USD116/2012/yr2012.Rda"))

## --------------------------------------------------------------------------- #
## LEAL
load(file=paste0(path.to.data,"Maps/2002/leal2002.Rda"),verbose=T)

## 2. see usd116zones2002.R for details
## for changes: 
s <- rbind(c("PERKINS RD","RICHARD DR"), # shift lat: UNIVERSITY/lieman
           c("PERKINS RD","RICHARD DR"), # *shift lat: cunningham/barr
           c("PERKINS RD","RICHARD DR"), 
           c("BROWNFIELD RD","PERKINS RD"), # shift: getPoint(ildot,"PERKINS RD","PERKINS RD")[2,]
           c("SCHEMAUGER TRAIL","POTAWATOMI TRAIL"), # shift to anthony/anthony 
           c("SCHEMAUGER TRAIL","POTAWATOMI TRAIL"), # shift lon: elizabeth/mcgee
           c("SCHEMAUGER TRAIL","KICKAPOO TRAIL"), # shift lon: slayback/mcgee; shift lat: coachman/carriage?
           c("COACHMAN DR","CARRIAGE PL"), # shift lon: smith/carriage
           c("BROWNFIELD RD","PERKINS RD") # shift lat: UNIVERSITY
           )

leal.2 <- makePoints(ildot,s)

uni <- getPoint(ildot,"LIEMAN DR","UNIVERSITY AVE")
leal.2 <- shiftPoint(leal.2,shift=list(1,uni[2]),lon=FALSE)

## rich <- getPoint(ildot,"OLIVER ST","CLIFFORD ST")
barr <- getPoint(ildot,"CUNNINGHAM AVE","BARR AVE")[2]
leal.2 <- shiftPoint(leal.2,shift=list(2,barr),lon=FALSE)

perk <- getPoint(ildot,"PERKINS RD","PERKINS RD")[2,]
leal.2 <- shiftPoint(leal.2,shift=list(4,perk[1]))
leal.2 <- shiftPoint(leal.2,shift=list(4,perk[2]),lon=FALSE)

loop <- getPoint(ildot,"ANTHONY DR","ANTHONY DR")[1,]
leal.2 <- shiftPoint(leal.2,shift=list(5,loop[1]))
leal.2 <- shiftPoint(leal.2,shift=list(5,loop[2]),lon=FALSE)

loop <- getPoint(ildot,"ELIZABETH ST","MCGEE RD")
leal.2 <- shiftPoint(leal.2,shift=list(6,loop[1]))

loop <- getPoint(ildot,"SLAYBACK DR","MCGEE RD")
leal.2 <- shiftPoint(leal.2,shift=list(7,loop[1]))
## loop <- getPoint(ildot,"COACHMAN DR","CARRIAGE PL")
## leal.2 <- shiftPoint(leal.2,shift=list(8,loop[2]),lon=FALSE)

loop <- getPoint(ildot,"SMITH RD","SLAYBACK DR")
leal.2 <- shiftPoint(leal.2,shift=list(8,loop[1]))

uni <- getPoint(ildot,"COTTAGE GROVE AVE","UNIVERSITY AVE")
leal.2 <- shiftPoint(leal.2,shift=list(9,uni[2]),lon=FALSE)

leal.2 <- makePolygon(leal.2,
                      poly.id="LEAL.2",
                      proj=proj4string(ildot))

leal.2 <- makeSPDF(leal.2,data.frame(poly.data,School.Name="LEAL"))

## change year to 2012
leal.1.1$Year <- as.factor("2012")

## Combine
leal <- spRbind(leal.1.1,leal.2)

## save
save(leal.1.1,leal.2,file=paste0(path.to.data,"Maps/USD116/2012/leal2012.Rda"))

## --------------------------------------------------------------------------- #
## PRAIRIE
load(file=paste0(path.to.data,"Maps/2002/prairie2002.Rda"),verbose=T)

## prairie.3: part of helmet from wiley.3 and leal.2
## for details, see references
s <- rbind(c("PERKINS RD","CARROLL AVE"), # shift lon; see wiley.3 (2012)
           c("PERKINS RD","CARROLL AVE"), # *shift lon: kerr/chief shemauger[2,]; see wiley.3 (2012)
           c("PERKINS RD","RICHARD DR"),
           c("BROWNFIELD RD","PERKINS RD"), # shift: see leal.2 (2012)
           c("BROWNFIELD RD","PERKINS RD"), # shift: see leal.2 (2002)
           c("PERKINS RD","CEDRIC DR"), # shift lat: see leal.2 (2002)
           c("PERKINS RD","RICHARD DR"), # shift lat: see leal.2 (2002)
           c("CUNNINGHAM AVE","US 45 TO I-74EB") # shift lat; see wiley.3 (2012)
           )

prairie.3 <- makePoints(ildot,s)

prairie.3 <- shiftPoint(prairie.3,shift=list(1,interp.lon))

perk <- getPoint(ildot,"KERR AVE","CHIEF SHEMAUGER PARK")[2,1]
prairie.3 <- shiftPoint(prairie.3,shift=list(2,perk))

perk <- getPoint(ildot,"PERKINS RD","PERKINS RD")[2,]
prairie.3 <- shiftPoint(prairie.3,shift=list(4,perk[1]))
prairie.3 <- shiftPoint(prairie.3,shift=list(4,perk[2]),lon=FALSE)

brown <- getPoint(ildot,"BROWNFIELD RD","BROWNFIELD RD")[4,]
prairie.3 <- shiftPoint(prairie.3,shift=list(5,brown[1]))
prairie.3 <- shiftPoint(prairie.3,shift=list(5,brown[2]),lon=FALSE)

rich <- getPoint(ildot,"OLIVER ST","CLIFFORD ST")
prairie.3 <- shiftPoint(prairie.3,shift=list(7,rich[2]),lon=FALSE)

cedric <- getPoint(ildot,"COLUMBIA BLVD","COLUMBIA BLVD")
cedric <- (cedric[2]+rich[2])/2
prairie.3 <- shiftPoint(prairie.3,shift=list(6,cedric),lon=FALSE)

us45 <- getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74WB")[2]
us45 <- (us45+getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74EB")[2])/2
prairie.3 <- shiftPoint(prairie.3,shift=list(8,us45),lon=FALSE)

prairie.3 <- makePolygon(prairie.3,
                         poly.id="PRAIRIE.3",
                         proj=proj4string(ildot))

prairie.3 <- makeSPDF(prairie.3,data.frame(poly.data,School.Name="PRAIRIE"))

## change year to 2012
prairie.1$Year <- as.factor("2012")
prairie.2$Year <- as.factor("2012")

## Combine
prairie <- spRbind(prairie.1,spRbind(prairie.2,prairie.3))

## save
save(prairie.1,prairie.2,file=paste0(path.to.data,"Maps/USD116/2012/prairie2012.Rda"))

## --------------------------------------------------------------------------- #
## THOMAS PAINE
load(file=paste0(path.to.data,"Maps/2002/paine2002.Rda"),verbose=T)

## paine.4: from wiley.3
## {Perkins and Richard; "Barr" and Richard; "Barr" and "Town & Country East"; "Thompson" and T&C E; "Thompson" and T&C W; Perkins and T&C W}
## N.B.:
## i. Northwest part may extend too far, but not residential
## ii. Missing apartment complex along T&C E; not important for now
s <- rbind(c("PERKINS RD","RICHARD DR"),
           c("PERKINS RD","RICHARD DR"), # shift lat: cunningham/barr
           c("CUNNINGHAM AVE","US 45 TO I-74EB"), # shift lat: cunningham/barr
           c("CUNNINGHAM AVE","US 45 TO I-74EB"), # shift lat: cunningham/thompson
           c("CUNNINGHAM AVE","THOMPSON ST"), # shift lon: kerr/chief shemauger[2,]
           c("PERKINS RD","CARROLL AVE") # shift lon: kerr/chief shemauger[2,]; see above
           )

paine.4 <- makePoints(ildot,s)

barr <- getPoint(ildot,"CUNNINGHAM AVE","BARR AVE")[2]
paine.4 <- shiftPoint(paine.4,shift=list(2,barr),lon=FALSE)
paine.4 <- shiftPoint(paine.4,shift=list(3,barr),lon=FALSE)

thom <- getPoint(ildot,"CUNNINGHAM AVE","THOMPSON ST")[2]
paine.4 <- shiftPoint(paine.4,shift=list(4,thom),lon=FALSE)

perk <- getPoint(ildot,"KERR AVE","CHIEF SHEMAUGER PARK")[2,1]
paine.4 <- shiftPoint(paine.4,shift=list(5,perk))
paine.4 <- shiftPoint(paine.4,shift=list(6,perk))

paine.4 <- makePolygon(paine.4,
                       poly.id="PAINE.4",
                       proj=proj4string(ildot))

paine.4 <- makeSPDF(paine.4,data.frame(poly.data,School.Name="PAINE"))

## change year to 2012
paine.1$Year <- as.factor("2012")
paine.2.1$Year <- as.factor("2012")
paine.3$Year <- as.factor("2012")

## Combine
paine <- spRbind(paine.4,spRbind(paine.1,spRbind(paine.2.1,paine.3)))

## save
save(paine.1,paine.2.1,paine.3,paine.4,
     file=paste0(path.to.data,"Maps/USD116/2012/paine2012.Rda"))

## --------------------------------------------------------------------------- #
## Combine all polygons and make a shapefile

## 1. convert character to factor to numeric?

## 2. *apply spRbind to collection of polygons, label YEAR
urbana.2012 <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal),yr),prairie),king)

save(urbana.2012,file=paste0(path.to.data,"Maps/USD116/2012/urbana2012.Rda"))

## 3. writeOGR to 
writeOGR(urbana.2012,
         dsn=paste0(path.to.data,"Maps/USD116/2012"),
         layer="urbana2012",
         driver="ESRI Shapefile")


## --------------------------------------------------------------------------- #

