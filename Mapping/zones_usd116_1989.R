## Attendance Zones in USD 116, 1989-2002
## map: see photocopy

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
poly.data <- data.frame(Agency.Name="USD 116",Year="1989")

## --------------------------------------------------------------------------- #

## MODIFICATIONS from 2002:
## 1. part of leal.1 (south of University) goes to king.1
## 2. part of leal.1 (west of lierman, north of washington) goes to prairie.1
## prairie.1 add: triangle at michigan/fairlawn/philo
## prairie.1 subtract: "Savannah Green" subdivision (roughly florida/smith)

## ONLY NEED TO RE-DO THE FOLLOWING:
## wiley.3 (border with leal.1.1)
## wiley.2 (border with prairie.1)
## king.1
## prairie.1
## paine.3 (border with prairie.1)
## leal.1.1 - via gDifference

## --------------------------------------------------------------------------- #
## WILEY
load(file=paste0(path.to.data,"Maps/2002/wiley2002.Rda"),verbose=T)

## 2. see usd116zones2002.R for details
## added points long north/east border with prairie.1; see below
s <- rbind(c("RACE ST","MONTCLAIR RD"),
           c("VINE ST","MONTCLAIR RD"),
           c("COLORADO AVE","ANDERSON ST"), # shift lon: Vine/montclair
           c("PHILO RD","COLORADO AVE"),
           c("MICHIGAN AVE","JACKSON RD"), # shift lon: philo/fairlawn; see prairie.1
           c("PHILO RD","FAIRLAWN DR"),
           c("WASHINGTON ST","PHILO RD"),
           c("LYNN ST","CALIFORNIA AVE"), # shift lat: washington/philo
           c("WASHINGTON ST","VINE ST"),
           c("FLORIDA AVE","ANDERSON ST"), # shift lon: Penn and Vine
           c("BROADWAY AVE","FLORIDA AVE")) # shift lon: Race and Delaware

wiley.2 <- makePoints(ildot,s)

## vine <- getPoint(ildot,"MICHIGAN AVE","VINE ST")[1]
vine <- getPoint(ildot,"VINE ST","MONTCLAIR RD")[1]
wiley.2 <- shiftPoint(wiley.2,shift=list(3,vine))

philo <- getPoint(ildot,"PHILO RD","FAIRLAWN DR")[1]
wiley.2 <- shiftPoint(wiley.2,shift=list(5,philo))

philo <- getPoint(ildot,"WASHINGTON ST","PHILO RD")[2]
wiley.2 <- shiftPoint(wiley.2,shift=list(8,philo),lon=FALSE)

vine <- getPoint(ildot,"VINE ST","PENNSYLVANIA AVE")[1]
wiley.2 <- shiftPoint(wiley.2,shift=list(10,vine))

race <- getPoint(ildot,"DELAWARE AVE","RACE ST")[1]
wiley.2 <- shiftPoint(wiley.2,shift=list(11,race))

wiley.2 <- makePolygon(wiley.2,
                       poly.id="WILEY.2",
                       proj=proj4string(ildot))

wiley.2 <- makeSPDF(wiley.2,data.frame(poly.data,School.Name="WILEY"))

## 3. see usd116zones2002.R for details
## added points along south border with king.1, prairie.1; see below
s <- rbind(c("RACE ST","UNIVERSITY AVE"), # shift lat: BROADWAY AVE and CUNNINGHAM AVE
           c("MAPLE ST","UNIVERSITY AVE"), # shift lat: broadway/cunningham; see king.1
           c("LYNN ST","CALIFORNIA AVE"), # shift lat: broadway/cunningham; see prairie.1
           c("PERKINS RD","RICHARD DR"), # shift lat: as above
           c("PERKINS RD","RICHARD DR"), # shift lat: OLIVER ST and CLIFFORD ST
           c("CUNNINGHAM AVE","US 45 TO I-74EB"), # shift lat; see below
           ## c("CUNNINGHAM AVE","US 45 TO I-74WB"), # average lat for this and previous
           c("HAGEN BLVD","KENYON RD"), # shift lat: as 74+Lincoln?
           c("LINCOLN TO I-74WB","LINCOLN TO I-74WB"), # shift lon: see next line; i-74WBtolin+lin
           c("BROADWAY AVE","COUNTRY CLUB RD"), # shift lon: (-88.21053,40.12884)
           c("BROADWAY AVE","FRANKLIN ST"), # shift lon: as above
           c("RACE ST","UNIVERSITY AVE"))

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
wiley.3 <- shiftPoint(makePoints(ildot,s),shift=list(1,race),lon=FALSE)
wiley.3 <- shiftPoint(wiley.3,shift=list(2,race),lon=FALSE)
wiley.3 <- shiftPoint(wiley.3,shift=list(3,race),lon=FALSE)
wiley.3 <- shiftPoint(wiley.3,shift=list(4,race),lon=FALSE)

rich <- getPoint(ildot,"OLIVER ST","CLIFFORD ST")[2]
wiley.3 <- shiftPoint(wiley.3,shift=list(5,rich),lon=FALSE)

us45 <- getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74WB")[2]
us45 <- (us45+getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74EB")[2])/2
wiley.3 <- shiftPoint(wiley.3,shift=list(6,us45),lon=FALSE)

i74 <- getPoint(ildot,"LINCOLN TO I-74WB","LINCOLN TO I-74WB")[2]
wiley.3 <- shiftPoint(wiley.3,shift=list(7,i74),lon=FALSE)

club <- getPoint(ildot,"COUNTRY CLUB RD","COUNTRY CLUB RD")[7,]
wiley.3 <- shiftPoint(wiley.3,shift=list(8,club[1]))
wiley.3 <- shiftPoint(wiley.3,shift=list(9,club[1]))
wiley.3 <- shiftPoint(wiley.3,shift=list(10,club[1]))

wiley.3 <- makePolygon(wiley.3,
                       poly.id="WILEY.3",
                       proj=proj4string(ildot))

wiley.3 <- makeSPDF(wiley.3,data.frame(poly.data,School.Name="WILEY"))

## change year to 2012
wiley.1$Year <- as.factor("1989")

## Combine
wiley <- spRbind(spRbind(wiley.1,wiley.2),wiley.3)

## save
save(wiley.1,wiley.2,wiley.3,file=paste0(path.to.data,"Maps/USD116/1989/wiley1989.Rda"))

## --------------------------------------------------------------------------- #
## KING
load(file=paste0(path.to.data,"Maps/2002/king2002.Rda"),verbose=T)

## 1. {University and Wright; Race and University; 74 and Country Club Rd; Wright and 74? - see Lincoln and 74}
s <- rbind(c("UNIVERSITY AVE","WRIGHT ST"), # shift lat: springfield/mathews
           c("MAPLE ST","UNIVERSITY AVE"), # shift lat: maple/main[1,]
           c("MAPLE ST","UNIVERSITY AVE"), # shift lat: broadway/cunningham
           c("RACE ST","UNIVERSITY AVE"), # shift lat: broadway/cunningham; see wiley.3
           c("RACE ST","UNIVERSITY AVE"),
           c("BROADWAY AVE","FRANKLIN ST"), # shift lon: (-88.21053,40.12884); see wiley.3
           c("BROADWAY AVE","COUNTRY CLUB RD"), # shift lon: see above
           c("LINCOLN TO I-74WB","LINCOLN TO I-74WB"), # shift lon: see above
           c("GROVE ST","WRIGHT ST")) # shift lat: lincoln and lincoln

king.1 <- makePoints(ildot,s)

spring <- getPoint(ildot,"MATHEWS AVE","SPRINGFIELD AVE")[2]
king.1 <- shiftPoint(king.1,shift=list(1,spring),lon=FALSE)

main <- getPoint(ildot,"MAPLE ST","MAIN ST")[1,2]
king.1 <- shiftPoint(king.1,shift=list(2,main),lon=FALSE)

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
king.1 <- shiftPoint(king.1,shift=list(3,race),lon=FALSE)
king.1 <- shiftPoint(king.1,shift=list(4,race),lon=FALSE)

club <- getPoint(ildot,"COUNTRY CLUB RD","COUNTRY CLUB RD")[7,]
king.1 <- shiftPoint(king.1,shift=list(6,club[1]))
king.1 <- shiftPoint(king.1,shift=list(7,club[1]))
king.1 <- shiftPoint(king.1,shift=list(8,club[1]))

linc <- getPoint(ildot,"LINCOLN TO I-74WB","LINCOLN TO I-74WB")[2]
king.1 <- shiftPoint(king.1,shift=list(9,linc),lon=FALSE)

## SHIFT WEST
max.pts <- bbox(usd116)
king.1 <- shiftPoint(king.1,shift=list(1,max.pts[1,1]))
king.1 <- shiftPoint(king.1,shift=list(9,max.pts[1,1]))

king.1 <- makePolygon(king.1,
                      poly.id="KING.1",
                      proj=proj4string(ildot))

king.1 <- makeSPDF(king.1,data.frame(poly.data,School.Name="KING"))

## change year to 2012
king.2$Year <- as.factor("1989")

## Combine
king <- spRbind(king.1,king.2)

## save
save(king.1,king.2,file=paste0(path.to.data,"Maps/USD116/1989/king1989.Rda"))

## --------------------------------------------------------------------------- #
## YANKEE RIDGE
load(file=paste0(path.to.data,"Maps/2002/yr2002.Rda"),verbose=T)

## change year to 2012
yr.1$Year <- as.factor("1989")
yr.2.1.2$Year <- as.factor("1989")

## Combine
yr <- spRbind(yr.1,yr.2.1.2)

## save
save(yr.1,yr.2.1.2,file=paste0(path.to.data,"Maps/USD116/1989/yr1989.Rda"))

## --------------------------------------------------------------------------- #
## PRAIRIE
load(file=paste0(path.to.data,"Maps/2002/prairie2002.Rda"),verbose=T)

## 1. see usd116zones2002.R for details
s <- rbind(c("MICHIGAN AVE","JACKSON RD"), # shift lon: philo/fairlawn
           ## c("PHILO RD","FAIRLAWN DR"), # shift lat: michigan/greenridge
           c("PHILO RD","FAIRLAWN DR"),
           c("WASHINGTON ST","PHILO RD"),
           c("LYNN ST","CALIFORNIA AVE"), # shift lat: washington/philo
           c("LYNN ST","CALIFORNIA AVE"), # shift lat: broadway/cunningham
           ## c("WASHINGTON ST","LIERMAN AVE"),
           c("WASHINGTON ST","LIERMAN AVE"), # shift lat: see leal.1
           c("PFEFFER RD","WASHINGTON ST"), # shift lat: abbey rd/university?
           c("PFEFFER RD","WASHINGTON ST"), # shift lat: MICHIGAN/abercorn
           ## c("DELAWARE AVE","GREENRIDGE DR"), # shift lat: MICHIGAN/abercorn
           c("MICHIGAN AVE","GREENRIDGE DR")
           )

prairie.1 <- makePoints(ildot,s)

philo <- getPoint(ildot,"PHILO RD","FAIRLAWN DR")
prairie.1 <- shiftPoint(prairie.1,shift=list(1,philo[1]))

philo <- getPoint(ildot,"WASHINGTON ST","PHILO RD")
prairie.1 <- shiftPoint(prairie.1,shift=list(4,philo[2]),lon=FALSE)

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
prairie.1 <- shiftPoint(prairie.1,shift=list(5,race),lon=FALSE)
prairie.1 <- shiftPoint(prairie.1,shift=list(6,race),lon=FALSE)

abbey <- getPoint(ildot,"ABBEY RD","UNIVERSITY AVE")
prairie.1 <- shiftPoint(prairie.1,shift=list(7,abbey[2]),lon=FALSE)

aber <- getPoint(ildot,"MICHIGAN","ABERCORN ST")
prairie.1 <- shiftPoint(prairie.1,shift=list(8,aber[2]),lon=FALSE)
## prairie.1 <- shiftPoint(prairie.1,shift=list(9,aber[2]),lon=FALSE)

prairie.1 <- makePolygon(prairie.1,
                         poly.id="PRAIRIE.1",
                         proj=proj4string(ildot))

prairie.1 <- makeSPDF(prairie.1,data.frame(poly.data,School.Name="PRAIRIE"))

## change year to 2012
prairie.2$Year <- as.factor("1989")

## Combine
prairie <- spRbind(prairie.1,prairie.2)

## save
save(prairie.1,prairie.2,file=paste0(path.to.data,"Maps/USD116/1989/prairie1989.Rda"))

## --------------------------------------------------------------------------- #
## THOMAS PAINE

load(file=paste0(path.to.data,"Maps/2002/paine2002.Rda"),verbose=T)

## 3. see usd116zones2002.R for details
## added points along northern border with prairie.1; see above
s <- rbind(c("PHILO RD","CURTIS RD"), # shift lon + lat: 1200N/1200N; see wiley.1
           c("WINDSOR RD","PHILO RD"),
           c("PHILO RD","COLORADO AVE"),
           c("MICHIGAN AVE","JACKSON RD"), # shift lon: philo/fairlawn; see prairie.1
           c("MICHIGAN AVE","JACKSON RD"),
           c("MICHIGAN AVE","GREENRIDGE DR"),
           ## c("DELAWARE AVE","GREENRIDGE DR"), # shift lat: MICHIGAN/abercorn; see prairie.1
           c("PFEFFER RD","WASHINGTON ST"), # shift lat: MICHIGAN/abercorn - see prairie.1
           c("PFEFFER RD","WASHINGTON ST"), # shift lat: abbey rd/university?
           c("WASHINGTON ST","LIERMAN AVE"), # shift lat: see leal.1
           c("PERKINS RD","RICHARD DR"), # shift lat: see wiley.3
           c("PERKINS RD","RICHARD DR"), # shift lat: UNIVERSITY/lieman - see leal.2
           c("BROWNFIELD RD","PERKINS RD"), # shift lat: UNIVERSITY - see leal.2
           c("COACHMAN DR","CARRIAGE PL"), # shift lon: smith/carriage - see leal.2
           c("SCHEMAUGER TRAIL","KICKAPOO TRAIL"), # shift - see leal.2
           c("SCHEMAUGER TRAIL","POTAWATOMI TRAIL"), # shift lon: elizabeth/mcgee
           c("SCHEMAUGER TRAIL","POTAWATOMI TRAIL"), # shift to anthony/anthony - see leal.2
           c("HIGHCROSS RD","ANTHONY DR"), # shift lon: abbey/haydon
           c("HIGHCROSS RD","ANTHONY DR"),
           c("HIGHCROSS RD","ANTHONY DR"), # shift lat: 40.12504; see note below
           c("WASHINGTON ST","1800 E"), # shift lat: highcross/anthony? instead: 40.12504; see note
           c("1400 N","1800 E"),
           c("DEERS RD (1200 N)","1800 E")
           )

paine.3 <- makePoints(ildot,s)

philo <- getPoint(ildot,"OLD CHURCH RD(1200N)","OLD CHURCH RD(1200N)")[2,]
paine.3 <- shiftPoint(paine.3,shift=list(1,philo[[1]]))
paine.3 <- shiftPoint(paine.3,shift=list(1,philo[[2]]),lon=FALSE)

philo <- getPoint(ildot,"PHILO RD","FAIRLAWN DR")
paine.3 <- shiftPoint(paine.3,shift=list(4,philo[1]))

aber <- getPoint(ildot,"MICHIGAN","ABERCORN ST")
paine.3 <- shiftPoint(paine.3,shift=list(7,aber[2]),lon=FALSE)

abbey <- getPoint(ildot,"ABBEY RD","UNIVERSITY AVE")
paine.3 <- shiftPoint(paine.3,shift=list(8,abbey[2]),lon=FALSE)

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
paine.3 <- shiftPoint(paine.3,shift=list(9,race),lon=FALSE)

paine.3 <- shiftPoint(paine.3,shift=list(10,race),lon=FALSE)

uni <- getPoint(ildot,"LIEMAN DR","UNIVERSITY AVE")
paine.3 <- shiftPoint(paine.3,shift=list(11,uni[2]),lon=FALSE)

uni <- getPoint(ildot,"COTTAGE GROVE AVE","UNIVERSITY AVE")
paine.3 <- shiftPoint(paine.3,shift=list(12,uni[2]),lon=FALSE)

loop <- getPoint(ildot,"SMITH RD","SLAYBACK DR")
paine.3 <- shiftPoint(paine.3,shift=list(13,loop[1]))

loop <- getPoint(ildot,"SLAYBACK DR","MCGEE RD")
paine.3 <- shiftPoint(paine.3,shift=list(14,loop[1]))

loop <- getPoint(ildot,"ELIZABETH ST","MCGEE RD")
paine.3 <- shiftPoint(paine.3,shift=list(15,loop[1]))

loop <- getPoint(ildot,"ANTHONY DR","ANTHONY DR")[1,]
paine.3 <- shiftPoint(paine.3,shift=list(16,loop[1]))
paine.3 <- shiftPoint(paine.3,shift=list(16,loop[2]),lon=FALSE)

abbey <- getPoint(ildot,"ABBEY RD","HAYDON DR")
paine.3 <- shiftPoint(paine.3,shift=list(17,abbey[1]))

## hc <- getPoint(ildot,"HIGHCROSS RD","ANTHONY DR")
hc <- 40.12504
paine.3 <- shiftPoint(paine.3,shift=list(19,hc),lon=FALSE)
paine.3 <- shiftPoint(paine.3,shift=list(20,hc),lon=FALSE)

## SHIFT 17,18,19,20 NORTH; see prairie.2
## See note below
## max.pts <- bbox(uni[[5]])
## paine.3 <- shiftPoint(paine.3,shift=list(17,max.pts[2,2]),lon=FALSE)
## paine.3 <- shiftPoint(paine.3,shift=list(18,max.pts[2,2]),lon=FALSE)
## paine.3 <- shiftPoint(paine.3,shift=list(19,max.pts[2,2]),lon=FALSE)
## paine.3 <- shiftPoint(paine.3,shift=list(20,max.pts[2,2]),lon=FALSE)

paine.3 <- makePolygon(paine.3,
                       poly.id="PAINE.3",
                       proj=proj4string(ildot))

paine.3 <- makeSPDF(paine.3,data.frame(poly.data,School.Name="PAINE"))

## change year to 2012
paine.1$Year <- as.factor("1989")
paine.2.1$Year <- as.factor("1989")

## Combine
paine <- spRbind(spRbind(paine.1,paine.2.1),paine.3)

## save
save(paine.1,paine.2.1,paine.3,file=paste0(path.to.data,"Maps/USD116/1989/paine1989.Rda"))

## --------------------------------------------------------------------------- #
## LEAL

load(file=paste0(path.to.data,"Maps/2002/leal2002.Rda"),verbose=T)

## see usd116zones2012.R for details
merged <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal.2),yr),prairie),king)
merged <- gUnaryUnion(merged)
leal.1.1 <- gDifference(usd116,merged)
leal.1.1 <- coordinates(as(leal.1.1,"SpatialLines"))[[1]]
leal.1.1 <- leal.1.1[[1]]
leal.1.1 <- makePolygon(leal.1.1,poly.id="LEAL.1.1",proj=proj4string(ildot))
leal.1.1 <- makeSPDF(leal.1.1,data.frame(poly.data,School.Name="LEAL"))

## change year to 2012
leal.2$Year <- as.factor("1989")

## Combine
leal <- spRbind(leal.1.1,leal.2)

## save
save(leal.1.1,leal.2,file=paste0(path.to.data,"Maps/USD116/1989/leal1989.Rda"))

## --------------------------------------------------------------------------- #
## Combine all polygons and make a shapefile

## 1. convert character to factor to numeric?

## 2. *apply spRbind to collection of polygons, label YEAR
urbana.1989 <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal),yr),prairie),king)

save(urbana.1989,file=paste0(path.to.data,"Maps/USD116/1989/urbana1989.Rda"))

## 3. writeOGR to 
writeOGR(urbana.1989,
         dsn=paste0(path.to.data,"Maps/USD116/1989"),
         layer="urbana1989",
         driver="ESRI Shapefile")

## --------------------------------------------------------------------------- #
