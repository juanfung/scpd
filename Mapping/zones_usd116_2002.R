## Attendance Zones in USD 116, 2002-2012
## 2002-2012 map: see "Scanned from a Xerox*.pdf"

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
poly.data <- data.frame(Agency.Name="USD 116",Year="2002")

## --------------------------------------------------------------------------- #
## WILEY
## 1. {Windsor and Race; Windsor and Philo; Curtis and Philo; Curtis and Race}
## s <- rbind(c("WINDSOR RD","RACE ST"),c("WINDSOR RD","PHILO RD"),c("PHILO RD","CURTIS RD"),c("CURTIS RD","1350 E"))
## 1.' {Windsor and Lincoln; Curtis and Lincoln; Curtis and "Wright"; Old Church and "Wright"; Old Church and Philo; Windsor and Philo}
s <- rbind(c("LINCOLN AVE","ST MARY'S RD"), # shift lat: windsor/race,
           c("CURTIS RD","1350 E"), # (curtis/race) shift lon: lincoln/st marys
           c("CURTIS RD","1350 E"), # (curtis/race) shift lon: wright
           c("OLD CHURCH RD","OLD CHURCH RD(1200N)"), # shift lon: wright
           c("OLD CHURCH RD(1200N)","1500 E"), # shift lon + lat: 1200N/1200N
           c("WINDSOR RD","PHILO RD"))

wiley.1 <- makePoints(ildot,s)

race <- getPoint(ildot,"WINDSOR RD","RACE ST")[2]
wiley.1 <- shiftPoint(wiley.1,shift=list(1,race),lon=FALSE)

linc <- getPoint(ildot,"LINCOLN AVE","ST MARY'S RD")[1]
wiley.1 <- shiftPoint(wiley.1,shift=list(2,linc))

wright <- getPoint(ildot,"WRIGHT ST","ARMORY AVE")[1]
wiley.1 <- shiftPoint(wiley.1,shift=list(3,wright))
wiley.1 <- shiftPoint(wiley.1,shift=list(4,wright))

## philo <- getPoint(ildot,"PHILO RD","CURTIS RD")
philo <- getPoint(ildot,"OLD CHURCH RD(1200N)","OLD CHURCH RD(1200N)")[2,]
wiley.1 <- shiftPoint(wiley.1,shift=list(5,philo[[1]]))
wiley.1 <- shiftPoint(wiley.1,shift=list(5,philo[[2]]),lon=FALSE)

wiley.1 <- makePolygon(wiley.1,
                     poly.id="WILEY.1",
                     proj=proj4string(ildot))

wiley.1 <- makeSPDF(wiley.1,data.frame(poly.data,School.Name="WILEY"))

## 2. {Montclair and Race; Montclair and Vine; Colorado and Vine; Colorado and Philo; Fairlawn and Philo; Washington and Philo; Washington and Vine; Florida and Vine; Florida and Race}
s <- rbind(c("RACE ST","MONTCLAIR RD"),
           c("VINE ST","MONTCLAIR RD"),
           c("COLORADO AVE","ANDERSON ST"), # shift lon: Vine/Montclair
           c("PHILO RD","COLORADO AVE"),
           c("PHILO RD","FAIRLAWN DR"),
           c("WASHINGTON ST","PHILO RD"),
           c("WASHINGTON ST","VINE ST"),
           c("FLORIDA AVE","ANDERSON ST"), # shift lon: Penn and Vine
           c("BROADWAY AVE","FLORIDA AVE")) # shift lon: Race and Delaware

## showCrossing(ildot,street="FLORIDA AVE",road1=TRUE)

## vine <- getPoint(ildot,"MICHIGAN AVE","VINE ST")[1]
vine <- getPoint(ildot,"VINE ST","MONTCLAIR RD")[1]
wiley.2 <- shiftPoint(makePoints(ildot,s),shift=list(3,vine))

vine <- getPoint(ildot,"VINE ST","PENNSYLVANIA AVE")[1]
wiley.2 <- shiftPoint(wiley.2,shift=list(8,vine))

race <- getPoint(ildot,"DELAWARE AVE","RACE ST")[1]
wiley.2 <- shiftPoint(wiley.2,shift=list(9,race))

wiley.2 <- makePolygon(wiley.2,
                       poly.id="WILEY.2",
                       proj=proj4string(ildot))

wiley.2 <- makeSPDF(wiley.2,data.frame(poly.data,School.Name="WILEY"))

## 3. {University(*shift lat to BROADWAY AVE AND CUNNINGHAM AVE) and Race; University(*shift lat) and Lierman; Perkins/Richard and Richard (*shift lat); Cunningham and US45/74EB; Hagan St and Kenyon Rd; 74 and Lincoln(*shift lon to next crossing); Country Club Rd and Country Club Rd[7,]=(-88.21053,40.12884); Broadway(*shift lon to Country Club Rd) and Franklin; uni/race}
## "US 45 to I-74{E,W}B", "I-74WB to US 45", "I-74EB TO US.45",
s <- rbind(c("RACE ST","UNIVERSITY AVE"), # shift lat: BROADWAY AVE and CUNNINGHAM AVE
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

rich <- getPoint(ildot,"OLIVER ST","CLIFFORD ST")[2]
wiley.3 <- shiftPoint(wiley.3,shift=list(3,rich),lon=FALSE)

us45 <- getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74WB")[2]
us45 <- (us45+getPoint(ildot,"CUNNINGHAM AVE","US 45 TO I-74EB")[2])/2
wiley.3 <- shiftPoint(wiley.3,shift=list(4,us45),lon=FALSE)

i74 <- getPoint(ildot,"LINCOLN TO I-74WB","LINCOLN TO I-74WB")[2]
wiley.3 <- shiftPoint(wiley.3,shift=list(5,i74),lon=FALSE)

club <- getPoint(ildot,"COUNTRY CLUB RD","COUNTRY CLUB RD")[7,]
wiley.3 <- shiftPoint(wiley.3,shift=list(6,club[1]))
wiley.3 <- shiftPoint(wiley.3,shift=list(7,club[1]))
wiley.3 <- shiftPoint(wiley.3,shift=list(8,club[1]))

wiley.3 <- makePolygon(wiley.3,
                       poly.id="WILEY.3",
                       proj=proj4string(ildot))

wiley.3 <- makeSPDF(wiley.3,data.frame(poly.data,School.Name="WILEY"))

## Combine + ensure unique row names
wiley <- spRbind(spRbind(wiley.1,wiley.2),wiley.3)

save(wiley.1,wiley.2,wiley.3,file=paste0(path.to.data,"Maps/USD116/2002/wiley2002.Rda"))

## --------------------------------------------------------------------------- #
## KING
## 1. {University and Wright; Race and University; 74 and Country Club Rd; Wright and 74? - see Lincoln and 74}
s <- rbind(c("UNIVERSITY AVE","WRIGHT ST"),
           c("RACE ST","UNIVERSITY AVE"),
           c("BROADWAY AVE","FRANKLIN ST"), # shift lon: (-88.21053,40.12884); see wiley.3
           c("BROADWAY AVE","COUNTRY CLUB RD"), # shift lon: see above
           c("LINCOLN TO I-74WB","LINCOLN TO I-74WB"), # shift lon: see above
           c("GROVE ST","WRIGHT ST")) # shift lat: lincoln and lincoln

king.1 <- makePoints(ildot,s)

king.1 <- shiftPoint(king.1,shift=list(3,club[1]))
king.1 <- shiftPoint(king.1,shift=list(4,club[1]))
king.1 <- shiftPoint(king.1,shift=list(5,club[1]))
linc <- getPoint(ildot,"LINCOLN TO I-74WB","LINCOLN TO I-74WB")[2]
king.1 <- shiftPoint(king.1,shift=list(6,linc),lon=FALSE)

## SHIFT WEST
max.pts <- bbox(usd116)
king.1 <- shiftPoint(king.1,shift=list(1,max.pts[1,1]))
king.1 <- shiftPoint(king.1,shift=list(6,max.pts[1,1]))

king.1 <- makePolygon(king.1,
                      poly.id="KING.1",
                      proj=proj4string(ildot))

king.1 <- makeSPDF(king.1,data.frame(poly.data,School.Name="KING"))

## 2. {Race and Windsor; Race and Florida; Lincoln and Florida; Lincoln and Windsor}
s <- rbind(c("WINDSOR RD","RACE ST"),
           c("BROADWAY AVE","FLORIDA AVE"), # shift lon: race and delaware; see wiley.2
           c("DELAWARE AVE","LINCOLN AVE"), # shift lat: busey and florida
           c("LINCOLN AVE","ST MARY'S RD")) # shift lat: windsor and race

king.2 <- makePoints(ildot,s)

race <- getPoint(ildot,"DELAWARE AVE","RACE ST")[1]
king.2 <- shiftPoint(king.2,shift=list(2,race))

florida <- getPoint(ildot,"BUSEY AVE","FLORIDA AVE")[2]
king.2 <- shiftPoint(king.2,shift=list(3,florida),lon=FALSE)

race <- getPoint(ildot,"WINDSOR RD","RACE ST")[2]
king.2 <- shiftPoint(king.2,shift=list(4,race),lon=FALSE)

king.2 <- makePolygon(king.2,
                      poly.id="KING.2",
                      proj=proj4string(ildot))

king.2 <- makeSPDF(king.2,data.frame(poly.data,School.Name="KING"))

## combine
king <- spRbind(king.1,king.2)

## save
save(king.1,king.2,file=paste0(path.to.data,"Maps/USD116/2002/king2002.Rda"))

## --------------------------------------------------------------------------- #
## YANKEE RIDGE
## 1. {Windsor and Philo; Windsor and Race; Montclair and Race; Montclair and Vine; Colorado and Vine; Colorado and Philo; Windsor and Philo}
s <- rbind(c("WINDSOR RD","PHILO RD"),
           c("WINDSOR RD","RACE ST"),
           c("RACE ST","MONTCLAIR RD"),
           c("VINE ST","MONTCLAIR RD"),
           c("COLORADO AVE","ANDERSON ST"), # shift lon: Vine/Montclair; see wiley.2
           c("PHILO RD","COLORADO AVE"))

## vine <- getPoint(ildot,"MICHIGAN AVE","VINE ST")[1]
vine <- getPoint(ildot,"VINE ST","MONTCLAIR RD")[1]
yr.1 <- makePolygon(shiftPoint(makePoints(ildot,s),shift=list(5,vine)),
                               poly.id="YANKEE.RIDGE.1",
                               proj=proj4string(ildot))

yr.1 <- makeSPDF(yr.1,data.frame(poly.data,School.Name="YANKEE.RIDGE"))

## 2. {Wright and University; Cunningham and 74; }
s <- rbind(c("GROVE ST","WRIGHT ST"), # shift lat: 74/wright; see king.1
           c("LINCOLN TO I-74WB","LINCOLN TO I-74WB"), # shift lon: see wiley.3
           c("HAGEN BLVD","KENYON RD"), # shift lat: see wiley.3
           ## c("CUNNINGHAM AVE","US 45 TO I-74WB"), # average lat for this and next; see wiley.3
           c("CUNNINGHAM AVE","US 45 TO I-74EB"),
           c("GEORGE ST","NORTHWOOD DR"), # interpolate lon: cunningham/us45 and cunningham/airport
           c("CAPTIVA ST","GEORGE ST"), # shit lon east: 
           c("CAPTIVA ST","ILLINI AIRPORT RD"), # shift lon east: see above
           c("CUNNINGHAM AVE","ILLINI AIRPORT RD"),
           c("CUNNINGHAM AVE","OLYMPIAN RD"),
           c("CUNNINGHAM AVE","OLYMPIAN RD"), # shift lat north: max lat?
           c("CUNNINGHAM AVE","OLYMPIAN RD"), # shift lat north + shift lon west: Wright vs max west
           c("CUNNINGHAM AVE","OLYMPIAN RD") # shift lon west: align with Wright vs max west
           )

yr.2 <- makePoints(ildot,s)
wright <- getPoint(ildot,"LINCOLN TO I-74WB","LINCOLN TO I-74WB")
yr.2 <- shiftPoint(yr.2,shift=list(1,wright[2]),lon=FALSE)

club <- getPoint(ildot,"COUNTRY CLUB RD","COUNTRY CLUB RD")[7,]
yr.2 <- shiftPoint(yr.2,shift=list(2,club[1]))

i74 <- getPoint(ildot,"LINCOLN TO I-74WB","LINCOLN TO I-74WB")
yr.2 <- shiftPoint(yr.2,shift=list(3,i74[2]),lon=FALSE)

yr.2 <- shiftPoint(yr.2,shift=list(4,us45),lon=FALSE)

interp <- approx(yr.2[c(4,8),1],yr.2[c(4,8),2])
## requires visual inspection
interp.lon <- interp[[1]][19] # or 20
interp.lat <- interp[[2]][19]
yr.2 <- shiftPoint(yr.2,shift=list(5,interp.lon))
yr.2 <- shiftPoint(yr.2,shift=list(5,interp.lat),lon=FALSE)

captiva <- getPoint(ildot,"PRAIRIE VIEW DR","BARNES DR")
captiva <- (captiva[1]+getPoint(ildot,"CONRAY ST","BARNES DR")[1])/2
yr.2 <- shiftPoint(yr.2,shift=list(6,captiva))
yr.2 <- shiftPoint(yr.2,shift=list(6,interp.lat),lon=FALSE)

yr.2 <- shiftPoint(yr.2,shift=list(7,captiva))

max.pts <- bbox(usd116) # get extreme west lon and extreme north lat
## cunn <- getPoint(ildot,)
yr.2 <- shiftPoint(yr.2,shift=list(10,max.pts[2,2]),lon=FALSE)

yr.2 <- shiftPoint(yr.2,shift=list(11,max.pts[1,1]))
yr.2 <- shiftPoint(yr.2,shift=list(11,max.pts[2,2]),lon=FALSE)

wright <- getPoint(ildot,"GROVE ST","WRIGHT ST")
yr.2 <- shiftPoint(yr.2,shift=list(12,wright[1]))

yr.2 <- makePolygon(yr.2,
                    poly.id="YANKEE.RIDGE.2",
                    proj=proj4string(ildot))

yr.2 <- makeSPDF(yr.2,data.frame(poly.data,School.Name="YANKEE.RIDGE"))

## Combine
yr <- spRbind(yr.1,yr.2)

save(yr.1,yr.2,file=paste0(path.to.data,"Maps/USD116/2002/yr2002.Rda"))

## --------------------------------------------------------------------------- #
## LEAL
## 1. {Washington and Vine; Florida and Vine; Florida and Wright; University and Wright; University and Race; Griggs and Race; University/Main(?) and Lierman; Washington and Lierman}
s <- rbind(c("WASHINGTON ST","VINE ST"),
           c("FLORIDA AVE","ANDERSON ST"), # shift lon: VINE/penn
           c("BROADWAY AVE","FLORIDA AVE"), # shift lon: race and delaware; see king.2
           c("DELAWARE AVE","LINCOLN AVE"), # shift lat: busey and florida
           c("KIRBY AVE","FLORIDA AVE"), # or grove/WRIGHT, then shift lat busey/FLORIDA?
           c("UNIVERSITY AVE","WRIGHT ST"),
           c("RACE ST","UNIVERSITY AVE"),
           c("RACE ST","UNIVERSITY AVE"), # shift lat: broadway/cunningham; see wiley.3
           c("WASHINGTON ST","LIERMAN AVE"), # shift lat: as above
           c("WASHINGTON ST","LIERMAN AVE")
           )

leal.1 <- makePoints(ildot,s)

vine <- getPoint(ildot,"VINE ST","PENNSYLVANIA AVE")
leal.1 <- shiftPoint(leal.1,shift=list(2,vine[1]))

race <- getPoint(ildot,"DELAWARE AVE","RACE ST")[1]
leal.1 <- shiftPoint(leal.1,shift=list(3,race))

florida <- getPoint(ildot,"BUSEY AVE","FLORIDA AVE")[2]
leal.1 <- shiftPoint(leal.1,shift=list(4,florida),lon=FALSE)

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
leal.1 <- shiftPoint(leal.1,shift=list(8,race),lon=FALSE)
leal.1 <- shiftPoint(leal.1,shift=list(9,race),lon=FALSE)

## SHIFT WEST
max.pts <- bbox(usd116)
leal.1 <- shiftPoint(leal.1,shift=list(5,max.pts[1,1]))
leal.1 <- shiftPoint(leal.1,shift=list(6,max.pts[1,1]))


leal.1 <- makePolygon(leal.1,
                      poly.id="LEAL.1",
                      proj=proj4string(ildot))

leal.1 <- makeSPDF(leal.1,data.frame(poly.data,School.Name="LEAL"))

## 2. {University and Lierman (Lieman?); *University and 74 (loops!); *74 to Richard (loops!)}
s <- rbind(c("PERKINS RD","RICHARD DR"), # shift lat: UNIVERSITY/lieman
           c("PERKINS RD","RICHARD DR"), # shift lat: OLIVER ST and CLIFFORD ST; see wiley.3
           c("PERKINS RD","CEDRIC DR"), # shift lat: columbia blvd/columbia blvd
           c("BROWNFIELD RD","PERKINS RD"), # shift: one of getPoint(ildot,"BROWNFIELD RD","BROWNFIELD RD"); try point 1,2 or 4
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

rich <- getPoint(ildot,"OLIVER ST","CLIFFORD ST")
leal.2 <- shiftPoint(leal.2,shift=list(2,rich[2]),lon=FALSE)

cedric <- getPoint(ildot,"COLUMBIA BLVD","COLUMBIA BLVD")
cedric <- (cedric[2]+rich[2])/2
leal.2 <- shiftPoint(leal.2,shift=list(3,cedric),lon=FALSE)

brown <- getPoint(ildot,"BROWNFIELD RD","BROWNFIELD RD")[4,]
leal.2 <- shiftPoint(leal.2,shift=list(4,brown[1]))
leal.2 <- shiftPoint(leal.2,shift=list(4,brown[2]),lon=FALSE)

perk <- getPoint(ildot,"PERKINS RD","PERKINS RD")[2,]
leal.2 <- shiftPoint(leal.2,shift=list(5,perk[1]))
leal.2 <- shiftPoint(leal.2,shift=list(5,perk[2]),lon=FALSE)

loop <- getPoint(ildot,"ANTHONY DR","ANTHONY DR")[1,]
leal.2 <- shiftPoint(leal.2,shift=list(6,loop[1]))
leal.2 <- shiftPoint(leal.2,shift=list(6,loop[2]),lon=FALSE)

loop <- getPoint(ildot,"ELIZABETH ST","MCGEE RD")
leal.2 <- shiftPoint(leal.2,shift=list(7,loop[1]))

loop <- getPoint(ildot,"SLAYBACK DR","MCGEE RD")
leal.2 <- shiftPoint(leal.2,shift=list(8,loop[1]))
## loop <- getPoint(ildot,"COACHMAN DR","CARRIAGE PL")
## leal.2 <- shiftPoint(leal.2,shift=list(8,loop[2]),lon=FALSE)

loop <- getPoint(ildot,"SMITH RD","SLAYBACK DR")
leal.2 <- shiftPoint(leal.2,shift=list(9,loop[1]))

uni <- getPoint(ildot,"COTTAGE GROVE AVE","UNIVERSITY AVE")
leal.2 <- shiftPoint(leal.2,shift=list(10,uni[2]),lon=FALSE)

leal.2 <- makePolygon(leal.2,
                      poly.id="LEAL.2",
                      proj=proj4string(ildot))

leal.2 <- makeSPDF(leal.2,data.frame(poly.data,School.Name="LEAL"))

leal <- spRbind(leal.1,leal.2)

save(leal.1,leal.2,file=paste0(path.to.data,"Maps/USD116/2002/leal2002.Rda"))

## --------------------------------------------------------------------------- #
## PRAIRIE
## 1. {Michigan and Philo; Fairlawn and Philo; Washington and Philo; Washington and Lierman; University* and Lierman; Pfeffer and Washington*; Florida and Pfeffer *(Abercorn?); Florida and Kinch (*shift lon Greenridge); Michigan and Greenridge}
s <- rbind(c("MICHIGAN AVE","JACKSON RD"),
           ## c("PHILO RD","FAIRLAWN DR"), # shift lat: michigan/greenridge
           c("PHILO RD","FAIRLAWN DR"),
           c("WASHINGTON ST","PHILO RD"),
           c("WASHINGTON ST","LIERMAN AVE"),
           c("WASHINGTON ST","LIERMAN AVE"), # shift lat: see leal.1
           c("PFEFFER RD","WASHINGTON ST"), # shift lat: abbey rd/university?
           c("PFEFFER RD","WASHINGTON ST"), # shift lat: florida/abercorn
           c("DELAWARE AVE","GREENRIDGE DR"), # shift lat: florida/abercorn
           c("MICHIGAN AVE","GREENRIDGE DR")
           )

prairie.1 <- makePoints(ildot,s)

## philo <- getPoint(ildot,"MICHIGAN AVE","GREENRIDGE DR")
## prairie.1 <- shiftPoint(prairie.1,shift=list(1,philo[2]),lon=FALSE)

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
prairie.1 <- shiftPoint(prairie.1,shift=list(5,race),lon=FALSE)

abbey <- getPoint(ildot,"ABBEY RD","UNIVERSITY AVE")
prairie.1 <- shiftPoint(prairie.1,shift=list(6,abbey[2]),lon=FALSE)

florida <- getPoint(ildot,"ABERCORN ST","FLORIDA AVE")
prairie.1 <- shiftPoint(prairie.1,shift=list(7,florida[2]),lon=FALSE)
prairie.1 <- shiftPoint(prairie.1,shift=list(8,florida[2]),lon=FALSE)

prairie.1 <- makePolygon(prairie.1,
                         poly.id="PRAIRIE.1",
                         proj=proj4string(ildot))

prairie.1 <- makeSPDF(prairie.1,data.frame(poly.data,School.Name="PRAIRIE"))

## 2. {loops around 74 (see leal.2); university and highcross; highcross and airport; airport and ?? (see yr.2)...; ?? and (*shift lon: brownfield/brownfield[1 or 2,]); brownfield/brownfield[1 or 2,]; }
s <- rbind(c("BROWNFIELD RD","PERKINS RD"), # shift: brown/brown - see leal.2
           c("BROWNFIELD RD","PERKINS RD"), # shift: perkins/perkins - see leal.2
           c("SCHEMAUGER TRAIL","POTAWATOMI TRAIL"), # shift to anthony/anthony - see leal.2
           c("HIGHCROSS RD","ANTHONY DR"), # shift lon: abbey/haydon
           c("HIGHCROSS RD","ANTHONY DR"),
           c("HIGHCROSS RD","ILLINI AIRPORT RD"),
           c("CAPTIVA ST","ILLINI AIRPORT RD"), # shift lon: see yr.2
           c("CAPTIVA ST","ILLINI AIRPORT RD"), # shift lon: as above; shift lat: see yr.2 interp
           c("CAPTIVA ST","ILLINI AIRPORT RD"), # shift lat: as above; shift lon: brown/brown
           c("BROWNFIELD RD","PERKINS RD") # shift: brown/brown
           )

prairie.2 <- makePoints(ildot,s)

brown <- getPoint(ildot,"BROWNFIELD RD","BROWNFIELD RD")[4,]
prairie.2 <- shiftPoint(prairie.2,shift=list(1,brown[1]))
prairie.2 <- shiftPoint(prairie.2,shift=list(1,brown[2]),lon=FALSE)

perk <- getPoint(ildot,"PERKINS RD","PERKINS RD")[2,]
prairie.2 <- shiftPoint(prairie.2,shift=list(2,perk[1]))
prairie.2 <- shiftPoint(prairie.2,shift=list(2,perk[2]),lon=FALSE)

loop <- getPoint(ildot,"ANTHONY DR","ANTHONY DR")[1,]
prairie.2 <- shiftPoint(prairie.2,shift=list(3,loop[1]))
prairie.2 <- shiftPoint(prairie.2,shift=list(3,loop[2]),lon=FALSE)

abbey <- getPoint(ildot,"ABBEY RD","HAYDON DR")
prairie.2 <- shiftPoint(prairie.2,shift=list(4,abbey[1]))

captiva <- getPoint(ildot,"PRAIRIE VIEW DR","BARNES DR")
captiva <- (captiva[1]+getPoint(ildot,"CONRAY ST","BARNES DR")[1])/2
prairie.2 <- shiftPoint(prairie.2,shift=list(7,captiva))

prairie.2 <- shiftPoint(prairie.2,shift=list(8,captiva))
prairie.2 <- shiftPoint(prairie.2,shift=list(8,interp.lat),lon=FALSE)

brown <- getPoint(ildot,"BROWNFIELD RD","BROWNFIELD RD")[1,] # or 2?
prairie.2 <- shiftPoint(prairie.2,shift=list(9,brown[1]))
prairie.2 <- shiftPoint(prairie.2,shift=list(9,interp.lat),lon=FALSE)

prairie.2 <- shiftPoint(prairie.2,shift=list(10,brown[1]))
prairie.2 <- shiftPoint(prairie.2,shift=list(10,brown[2]),lon=FALSE)

## SHIFT NORTH, same for paine.3
## See note below
## max.pts <- bbox(uni[[5]])
## prairie.2 <- shiftPoint(prairie.2,shift=list(3,max.pts[2,2]),lon=FALSE)
## prairie.2 <- shiftPoint(prairie.2,shift=list(4,max.pts[2,2]),lon=FALSE)
## prairie.2 <- shiftPoint(prairie.2,shift=list(5,max.pts[2,2]),lon=FALSE)

prairie.2 <- makePolygon(prairie.2,
                         poly.id="PRAIRIE.2",
                         proj=proj4string(ildot))

prairie.2 <- makeSPDF(prairie.2,data.frame(poly.data,School.Name="PRAIRIE"))

prairie <- spRbind(prairie.1,prairie.2)

## save
save(prairie.1,prairie.2,file=paste0(path.to.data,"Maps/USD116/2002/prairie2002.Rda"))

## --------------------------------------------------------------------------- #
## THOMAS PAINE
## 1. see yr.2, leal.2, prairie.2, wiley.3
s <- rbind(c("CAPTIVA ST","ILLINI AIRPORT RD"), # shift: see prairie.2
           c("BROWNFIELD RD","PERKINS RD"), # shift: see prairie.2
           c("BROWNFIELD RD","PERKINS RD"), # shift: see leal.2
           c("PERKINS RD","CEDRIC DR"), # shift: see leal.2
           c("PERKINS RD","RICHARD DR"), # shift: see leal.2/wiley.3
           ## c("PERKINS RD","RICHARD DR"), # shift UNI: see leal.2
           c("CUNNINGHAM AVE","US 45 TO I-74EB"), # no shift; see wiley.3/yr.2
           c("GEORGE ST","NORTHWOOD DR") # interpolate lon: see yr.2
           )

paine.1 <- makePoints(ildot,s)

brown <- getPoint(ildot,"BROWNFIELD RD","BROWNFIELD RD")[1,] # or 2?
paine.1 <- shiftPoint(paine.1,shift=list(1,brown[1]))
paine.1 <- shiftPoint(paine.1,shift=list(1,interp.lat),lon=FALSE)

paine.1 <- shiftPoint(paine.1,shift=list(2,brown[1]))
paine.1 <- shiftPoint(paine.1,shift=list(2,brown[2]),lon=FALSE)

brown <- getPoint(ildot,"BROWNFIELD RD","BROWNFIELD RD")[4,]
paine.1 <- shiftPoint(paine.1,shift=list(3,brown[1]))
paine.1 <- shiftPoint(paine.1,shift=list(3,brown[2]),lon=FALSE)

cedric <- getPoint(ildot,"COLUMBIA BLVD","COLUMBIA BLVD")
cedric <- (cedric[2]+rich[2])/2
paine.1 <- shiftPoint(paine.1,shift=list(4,cedric),lon=FALSE)

rich <- getPoint(ildot,"OLIVER ST","CLIFFORD ST")
paine.1 <- shiftPoint(paine.1,shift=list(5,rich[2]),lon=FALSE)

## uni <- getPoint(ildot,"LIEMAN DR","UNIVERSITY AVE")
## paine.1 <- shiftPoint(paine.1,shift=list(6,uni[2]),lon=FALSE)

paine.1 <- shiftPoint(paine.1,shift=list(6,us45),lon=FALSE)

paine.1 <- shiftPoint(paine.1,shift=list(7,interp.lon))
paine.1 <- shiftPoint(paine.1,shift=list(7,interp.lat),lon=FALSE)

paine.1 <- makePolygon(paine.1,
                       poly.id="PAINE.1",
                       proj=proj4string(ildot))

paine.1 <- makeSPDF(paine.1,data.frame(poly.data,School.Name="PAINE"))

## 2. see prairie.2, yr.2
s <- rbind(c("HIGHCROSS RD","ILLINI AIRPORT RD"),
           c("HIGHCROSS RD","OLYMPIAN RD"),
           c("CUNNINGHAM AVE","OLYMPIAN RD"),
           c("CUNNINGHAM AVE","ILLINI AIRPORT RD")
           )

paine.2 <- makePolygon(makePoints(ildot,s),
                       poly.id="PAINE.2",
                       proj=proj4string(ildot))

paine.2 <- makeSPDF(paine.2,data.frame(poly.data,School.Name="PAINE"))

## 3. see wiley.1, wiley.3, leal.1, leal.2, prairie.1, prairie.2
s <- rbind(c("PHILO RD","CURTIS RD"), # shift lon + lat: 1200N/1200N; see wiley.1
           c("WINDSOR RD","PHILO RD"),
           c("PHILO RD","COLORADO AVE"),
           ## c("PHILO RD","FAIRLAWN DR"), # shift lat: michigan/greenridge - see prairie.1           
           c("PHILO RD","FAIRLAWN DR"),
           c("MICHIGAN AVE","JACKSON RD"),
           c("MICHIGAN AVE","GREENRIDGE DR"),
           c("DELAWARE AVE","GREENRIDGE DR"), # shift lat: florida/abercorn - see prairie.1
           c("PFEFFER RD","WASHINGTON ST"), # shift lat: florida/abercorn - see prairie.1
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
           ## c("CURTIS RD","HIGHCROSS RD")
           c("WASHINGTON ST","1800 E"), # shift lat: highcross/anthony? instead: 40.12504; see note
           c("1400 N","1800 E"),
           c("DEERS RD (1200 N)","1800 E")
           )

paine.3 <- makePoints(ildot,s)

philo <- getPoint(ildot,"OLD CHURCH RD(1200N)","OLD CHURCH RD(1200N)")[2,]
paine.3 <- shiftPoint(paine.3,shift=list(1,philo[[1]]))
paine.3 <- shiftPoint(paine.3,shift=list(1,philo[[2]]),lon=FALSE)

## philo <- getPoint(ildot,"MICHIGAN AVE","GREENRIDGE DR")
## paine.3 <- shiftPoint(paine.3,shift=list(4,philo[2]),lon=FALSE)

florida <- getPoint(ildot,"ABERCORN ST","FLORIDA AVE")
paine.3 <- shiftPoint(paine.3,shift=list(7,florida[2]),lon=FALSE)
paine.3 <- shiftPoint(paine.3,shift=list(8,florida[2]),lon=FALSE)

abbey <- getPoint(ildot,"ABBEY RD","UNIVERSITY AVE")
paine.3 <- shiftPoint(paine.3,shift=list(9,abbey[2]),lon=FALSE)

race <- getPoint(ildot,"BROADWAY AVE","CUNNINGHAM AVE")[2]
race <-(race+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
paine.3 <- shiftPoint(paine.3,shift=list(10,race),lon=FALSE)

paine.3 <- shiftPoint(paine.3,shift=list(11,race),lon=FALSE)

uni <- getPoint(ildot,"LIEMAN DR","UNIVERSITY AVE")
paine.3 <- shiftPoint(paine.3,shift=list(12,uni[2]),lon=FALSE)

uni <- getPoint(ildot,"COTTAGE GROVE AVE","UNIVERSITY AVE")
paine.3 <- shiftPoint(paine.3,shift=list(13,uni[2]),lon=FALSE)

loop <- getPoint(ildot,"SMITH RD","SLAYBACK DR")
paine.3 <- shiftPoint(paine.3,shift=list(14,loop[1]))

loop <- getPoint(ildot,"SLAYBACK DR","MCGEE RD")
paine.3 <- shiftPoint(paine.3,shift=list(15,loop[1]))

loop <- getPoint(ildot,"ELIZABETH ST","MCGEE RD")
paine.3 <- shiftPoint(paine.3,shift=list(16,loop[1]))

loop <- getPoint(ildot,"ANTHONY DR","ANTHONY DR")[1,]
paine.3 <- shiftPoint(paine.3,shift=list(17,loop[1]))
paine.3 <- shiftPoint(paine.3,shift=list(17,loop[2]),lon=FALSE)

abbey <- getPoint(ildot,"ABBEY RD","HAYDON DR")
paine.3 <- shiftPoint(paine.3,shift=list(18,abbey[1]))

## hc <- getPoint(ildot,"HIGHCROSS RD","ANTHONY DR")
hc <- 40.12504
paine.3 <- shiftPoint(paine.3,shift=list(20,hc),lon=FALSE)
paine.3 <- shiftPoint(paine.3,shift=list(21,hc),lon=FALSE)

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

## paine <- spRbind(spRbind(paine.1,paine.2),paine.3)
paine <- spRbind(paine.1,paine.3)

save(paine.1,paine.2,paine.3,file=paste0(path.to.data,"Maps/USD116/2002/paine2002.Rda"))

## --------------------------------------------------------------------------- #
## REMARK 1:
## For prairie.1, leal.1, wiley.3, and paine.3: shift lierman/uni lat:
## race <-(race[2]+getPoint(ildot,"WATER ST","COTTAGE GROVE AVE")[2])/2
## Done.

## --------------------------------------------------------------------------- #
## REMARK 2:
## a. shift EAST borders: {paine.3} - from Highcross Rd to 1800 E;
## Done.
## b. shift NORTH borders: {yr.2, paine.2} - from Olympian Rd to 2200 N (or halfway to 2300 N)??
## *** N.B.: cunningham township ends at olympian; urbana township ends at airport
## HOWEVER, Urbana school district maps cover this area...
##
## c. shift SOUTH borders: wiley.1, paine.3 - from Curtis Rd to Old Church Rd
## Done.

## see district shapefiles

## --------------------------------------------------------------------------- #

## Getting odd borders:

## 1. Merge all except paine.2 into one shapefile
## gDifference -> paine.2.1

## Extend West as far as possible
## OPTION: merge -> get coordinates -> shift coordinates -> makePolygon
merged <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal),yr),prairie),king)
merged <- gUnaryUnion(merged)
merged.xy <- coordinates(as(merged,"SpatialLines"))[[1]][[1]]
## shift extreme west coordinates
max.pts <- bbox(merged.xy)
merged.xy[merged.xy[,"x"]< -88.22,"x"] = max.pts[1,1]
## re-make polygon
merged.xy <- makePolygon(merged.xy,poly.id="merged",proj=proj4string(ildot))

max.pts <- bbox(usd116) # get extreme west lon, max.pts[1,1], and extreme south lat, max.pts[2,1]

## gDifference -> paine.2.1
paine.2.1 <- gDifference(usd116,merged.xy)

paine.2.1.xy <- coordinates(as(paine.2.1,"SpatialLines"))[[1]]
paine.2.1.xy <- paine.2.1.xy[[2]]

## align southern boundary with prairie.2

max.pts <- bbox(prairie.2)

paine.2.1.xy[(paine.2.1.xy[,"x"] < -88.16000 &
                  paine.2.1.xy[,"x"] > -88.19000 &
                      paine.2.1.xy[,"y"] < 40.1500 &
                          paine.2.1.xy[,"y"] > 40.14200),]

paine.2.1 <- makePolygon(paine.2.1.xy,poly.id="PAINE.2.1",proj=proj4string(ildot))
paine.2.1 <- makeSPDF(paine.2.1,data.frame(poly.data,School.Name="PAINE"))

paine <- spRbind(paine.1,spRbind(paine.2.1,paine.3))

## save
save(paine.1,paine.2.1,paine.3,file=paste0(path.to.data,"Maps/USD116/2002/paine2002.Rda"))

## 2. Repeat: Drop yr.2, add paine.2.1 and merge again
## gDifference -> yr.2.1
merged <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal),yr.1),prairie),king)
merged <- gUnaryUnion(merged)
merged.xy <- coordinates(as(merged,"SpatialLines"))[[1]]

max.pts <- bbox(usd116)

## merged.xy[[1]][merged.xy[[1]][,"x"]< -88.22,"x"] = max.pts[1,1]
merged.xy[[2]][merged.xy[[2]][,"x"]< -88.22,"x"] = max.pts[1,1]

merged.xy.1 <- makePolygon(merged.xy[[1]],poly.id="merged.1",proj=proj4string(ildot))
merged.xy.2 <- makePolygon(merged.xy[[2]],poly.id="merged.2",proj=proj4string(ildot))
merged.xy <- spRbind(merged.xy.1,merged.xy.2)
merged.xy <- gUnaryUnion(merged.xy)

yr.2.1 <- gDifference(usd116,merged.xy)
yr.2.1 <- coordinates(as(yr.2.1,"SpatialLines"))[[1]]

yr.2.1.2 <- yr.2.1[[2]]
yr.2.1.2 <- makePolygon(yr.2.1.2,poly.id="YR.2.1",proj=proj4string(ildot))
yr.2.1.2 <- makeSPDF(yr.2.1.2,data.frame(poly.data,School.Name="YANKEE.RIDGE"))

yr <- spRbind(yr.2.1.2,yr.1)

## save
save(yr.1,yr.2.1.2,file=paste0(path.to.data,"Maps/USD116/2002/yr2002.Rda"))

## 3.  Drop leal.1, and merge again
## gDifference -> leal.1.1
merged <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal.2),yr),prairie),king)
merged <- gUnaryUnion(merged)
leal.1.1 <- gDifference(usd116,merged)
leal.1.1 <- coordinates(as(leal.1.1,"SpatialLines"))[[1]]
leal.1.1 <- leal.1.1[[1]]
leal.1.1 <- makePolygon(leal.1.1,poly.id="LEAL.1.1",proj=proj4string(ildot))
leal.1.1 <- makeSPDF(leal.1.1,data.frame(poly.data,School.Name="LEAL"))

leal <- spRbind(leal.1.1,leal.2)

## save
save(leal.1.1,leal.2,file=paste0(path.to.data,"Maps/USD116/2002/leal2002.Rda"))

## Shift king.1 WEST; see above

## 5. NOTE. Shift latitude for prairie.2 and paine.3? see above

zones <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal),yr),prairie),king)
uni <- gDifference(usd116,gUnaryUnion(zones))
uni <- coordinates(as(uni,"SpatialLines"))[[1]]


## --------------------------------------------------------------------------- #
## Combine all polygons and make a shapefile

## 1. convert character to factor to numeric?

## 2. *apply spRbind to collection of polygons, label YEAR
urbana.2002 <- spRbind(spRbind(spRbind(spRbind(spRbind(wiley,paine),leal),yr),prairie),king)

save(urbana.2002,file=paste0(path.to.data,"Maps/USD116/2002/urbana2002.Rda"))

## 3. writeOGR to 
writeOGR(urbana.2002,
         dsn=paste0(path.to.data,"Maps/USD116/2002"),
         layer="urbana2002",
         driver="ESRI Shapefile")


## --------------------------------------------------------------------------- #

## map in ggplot
library(ggplot2)
library(ggmap)
library(Cairo)

## prepare data.frame for ggplot
fzones <- fortify(zones,region="School.Name")

## group by school
schools <- c("KING","LEAL","PAINE","PRAIRIE","WILEY","YANKEE.RIDGE")

for (school in schools) {
    fzones[grep(school,fzones$group),"School"] <- school
}

fzones$School <- as.factor(fzones$School)

## make map
p <- ggplot(fzones) +
    aes(long,lat,group=group,fill=School) +
        geom_polygon() +
            geom_path(color="white") +
                coord_map()

p <- p + theme_nothing(legend = TRUE) +
    labs(title = "Urbana Attendance Zones, 2002-2012",
         fill = "")

ggsave(p, file = "../Paper/Figs/urbana2002.png", width = 6, height = 4.5, type = "cairo-png")
