## Make descriptive statistics tables

## --------------------------------------------------------------------------- #

library(tables)

## disable scientific notation
options(scipen=999)

## Subset data for tables
s0 <- dt.ha[grep("champaign|urbana",Agency.Name,ignore.case=TRUE),]
## convert sq ft/lotsize to 1000s
s0 <- s0[,Sqft:=Sqft/1000]
s0 <- s0[,Lotsize:=Lotsize/1000]
s0 <- s0[,Age:=Year-Year.Built]

## make functions na.rm by default
Mean=function(x) mean(x,na.rm=TRUE)
Median=function(x) median(x,na.rm=TRUE)
SD=function(x) sd(x,na.rm=TRUE)

## generate table of houseing level descriptives by School District
s1 <- tabular(
            (RowFactor(x=Agency.Name,
                name="School District",
                levelnames=paste0("\\textit{",c("Champaign","Urbana"),"}"),
                spacing=1,space=0.5))*
            #(Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
            (Log.Sale.Price+
             Lotsize+
             (Sq.Ft=Sqft/1000)+
             Bedrooms+
             (Bathrooms=Baths.Total)+
             (Basement=Basement/1000)+             
             Garage+
             Age.Home+
             Neighborhood.School.Distance#+Closest.School.Distance
            ) ~
                (Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
                (#(n=1) +
                    Format(digits=3)*(Mean + Median + SD)) ,
    ##data=s0[SR_VAL_TRANSFER>0,])
    data=s0)

##latexNumeric()

save <- booktabs()

s1 <- latex(s1,
            file="../Paper/Tables/s1.tex")

table_options(save)

system(paste0("sed -i -r 's/(Log)\\.(Sale)\\.(Price)/\\1 \\2 \\3/g' ../Paper/Tables/s1.tex"))


## generate table of neighborhood level descriptives by district
s2 <- tabular(
            (RowFactor(x=Agency.Name,
                name="School District",
                levelnames=paste0("\\textit{",c("Champaign","Urbana"),"}"),
                spacing=1,space=0.5))*
            #(Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
            (Households+
             Median.Household.Income+
             Median.Rooms+
             Owner.Occupied.Units+
             Owner.Occupied.Median.Value             
            ) ~
                (Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
                (#(n=1) +                    
                    Format(digits=3)*(Mean + Median + SD)) ,
    data=s0)

save <- booktabs()

s2 <- latex(s2,
            file="../Paper/Tables/s2.tex")

table_options(save)


## generate table of descriptive statistics by school or school district
t0 <- dt.schools[grep("CHAMPAIGN|URBANA",Agency.Name)]
t0 <- t0[grep("ELEM",School.Name)]
t0 <- droplevels(t0)
t0[Year>=1998, Post.Choice:=1L]
t0[Year<1998, Post.Choice:=0L]

t1 <- tabular(
            (RowFactor(x=Agency.Name,
                name="School District",
                levelnames=paste0("\\textit{",c("Champaign","Urbana"),"}"),
                spacing=1,space=0.5))*
            #(Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
            (School.Tax.Rate+
             (Class.Size=AVG.CLASS.SIZE.SCHOOL.KG)+
             (Low.Income=LOWINCOME.SCHOOL.PERCENT)+
             (Black=SCHOOL.BLACK.PERCENT)+
             (Enrollment=SCHOOL.TOTAL.ENROLLMENT)+
             (Excel.Math.Gr3=GR3.MATH.SCHOOL.EXCEEDS.ALL)+
             (Excel.Reading.Gr3=GR3.READ.SCHOOL.EXCEEDS.ALL)
             #(Excel.Math.Gr5=GR5.MATH.SCHOOL.EXCEEDS.ALL)+
             #(Excel.Reading.Gr5=GR5.READ.SCHOOL.EXCEEDS.ALL)
            ) ~
                (Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
                (##(n=1) +                    
                    Format(digits=3)*(Mean + Median + SD)) ,
    data=t0[Year<2002])

## save
t1 <- latex(t1,
            file="../Paper/Tables/t1.tex")

table_options(save)

system(paste0("sed -i -r 's/(School)\\.(Tax)\\.(Rate)/\\1 \\2 \\3/g' ../Paper/Tables/t1.tex"))

##system(paste0("sed -i -r 's/(^[a-zA-Z]+$)\\.(^[a-zA-A]+$)/\\1 \\2/g' ../Paper/Tables/t1.tex"))


## tabulate BY SCHOOL
pre.choice.schools <- t0[Year<1998, unique(School.Name)]
post.choice.schools <- t0[Year>1998 & Year<2002, unique(School.Name)]


t2.data <- t0[Year<2002,
              .("Mean_enrollment"=mean(SCHOOL.TOTAL.ENROLLMENT),
               "Mean_math"=mean(GR3.MATH.SCHOOL.EXCEEDS.ALL)),
              by=c("Post.Choice","Agency.Name","School.Name")]

##setnames(t2.data, old="V1", new="Enrollment")


t2 <- tabular((RowFactor(x=Agency.Name,name="District"))*
              (RowFactor(x=as.factor(School.Name),name="School")) ~
                  Heading()*identity*Enrollment, data=t2.data)

## a few by School, pre-choice in Champaign
t2.1 <- tabular(
#(RowFactor(x=Agency.Name,name="District"))*
(RowFactor(x=School.Name,name="School", spacing=1, space=0.5,
           levelnames=c("Bottenfield",
                        "Carrie Busey",
                        "Columbia",
                        "Howard",
                        "Garden Hills",
                        "Kenwood",
                        "Robeson",
                        "South Side",
                        "Stratton",
                        "Barkstall",
                        "Washington",
                        "Westview")
           ))*
   #(Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
(#(Class.Size=AVG.CLASS.SIZE.SCHOOL.KG)+
 #(Excel.Reading.Gr3=GR3.READ.SCHOOL.EXCEEDS.ALL)+
 (Excel.Math.Gr3=GR3.MATH.SCHOOL.EXCEEDS.ALL)
 #(Low.Income=LOWINCOME.SCHOOL.PERCENT)+
 #(Black=SCHOOL.BLACK.PERCENT)+
 #(Enrollment=SCHOOL.TOTAL.ENROLLMENT)
) ~
    (Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
    ( Format(digits=3)*(Mean + Median + SD) ) ,
data=t0[Year<2002 #& School.Name %in% pre.choice.schools
        & Agency.Name=="CHAMPAIGN CUSD 4"])

## save
t2.1 <- latex(t2.1,
            file="../Paper/Tables/schools_pre_champaign.tex")
table_options(save)
#system(paste0("sed -i -r 's/(School)\\.(Tax)\\.(Rate)/\\1 \\2 \\3/g' ../Paper/Tables/schools_pre_champaign.tex"))

t2.2 <- tabular(
#(RowFactor(x=Agency.Name,name="District"))*
(RowFactor(x=School.Name,name="School", spacing=2))*
#(Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
((Class.Size=AVG.CLASS.SIZE.SCHOOL.KG)
 #(Low.Income=LOWINCOME.SCHOOL.PERCENT)+
 #(Black=SCHOOL.BLACK.PERCENT)+
 #(Enrollment=SCHOOL.TOTAL.ENROLLMENT)
) ~
    ( Format(digits=3)*(Mean + Median + SD) ) ,
data=t0[Year>1998 & Year < 2002 & School.Name %in% post.choice.schools & Agency.Name=="CHAMPAIGN CUSD 4"])

## save
t2.2 <- latex(t2.2,
            file="../Paper/Tables/schools_post_champaign.tex")
table_options(save)

## a few by School, pre-choice in Urbana
t3.1 <- tabular(
#(RowFactor(x=Agency.Name,name="District"))*
(RowFactor(x=School.Name,name="School", spacing=1, space=0.5,
           levelnames=c("Leal",
                        "King",
                        "Prairie",
                        "Thomas Paine",
                        "Wiley",
                        "Yankee Ridge")))*
   #(Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
(#(Class.Size=AVG.CLASS.SIZE.SCHOOL.KG)+
 #(Excel.Reading.Gr3=GR3.READ.SCHOOL.EXCEEDS.ALL)+
 (Excel.Math.Gr3=GR3.MATH.SCHOOL.EXCEEDS.ALL)
 #(Low.Income=LOWINCOME.SCHOOL.PERCENT)+
 #(Black=SCHOOL.BLACK.PERCENT)+
 #(Enrollment=SCHOOL.TOTAL.ENROLLMENT)
) ~
    (Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
    ( Format(digits=3)*(Mean + Median + SD) ) ,
data=t0[Year<2002 & #School.Name %in% pre.choice.schools &
        Agency.Name!="CHAMPAIGN CUSD 4"])

## save
t3.1 <- latex(t3.1,
            file="../Paper/Tables/schools_pre_urbana.tex")
table_options(save)
#system(paste0("sed -i -r 's/(School)\\.(Tax)\\.(Rate)/\\1 \\2 \\3/g' ../Paper/Tables/schools_pre_champaign.tex"))


t0.007 <- t0[Year<2002][grep("CHAMPAIGN",Agency.Name)] %>%
    group_by(School.Name, Post.Choice) %>%
    rename(Enrollment=SCHOOL.TOTAL.ENROLLMENT) %>%
    summarise(
        "Mean_enrollment"=Mean(Enrollment),
        "SD_enrollment"=SD(Enrollment)
    ) %>%
    arrange(Post.Choice, desc(Mean_enrollment))


t3.2 <- tabular(
#(RowFactor(x=Agency.Name,name="District"))*
(RowFactor(x=School.Name,name="School", spacing=2))*
#(Factor(x=Post.Choice, name="Post Choice", levelnames=c("Pre", "Post")))*
((Class.Size=AVG.CLASS.SIZE.SCHOOL.KG)
 #(Low.Income=LOWINCOME.SCHOOL.PERCENT)+
 #(Black=SCHOOL.BLACK.PERCENT)+
 #(Enrollment=SCHOOL.TOTAL.ENROLLMENT)
) ~
    ( Format(digits=3)*(Mean + Median + SD) ) ,
data=t0[Year>1998 & Year < 2002 & School.Name %in% post.choice.schools & Agency.Name!="CHAMPAIGN CUSD 4"])

## save
t3.2 <- latex(t3.2,
            file="../Paper/Tables/schools_post_urbana.tex")
table_options(save)


## generate cross-tab: district vs treatment
s3 <- tabular(
(Factor(x=Agency.Name,
        name="School District",
        levelnames=c("Champaign","Urbana")) + 1) ~
    (Factor(x=Post.Choice,
            name="Post Choice",
            levelnames=c("Pre-","Post-")) + 1) ,
##data=s0[SR_VAL_TRANSFER>0,])
data=s0)

save <- booktabs()

s3 <- latex(s3,
            file="../Paper/Tables/s3.tex")

table_options(save)

##system(paste0("sed -i -r 's/(6371)/\\\textbf{\\1}/g' ../Paper/Tables/s3.tex"))

## generate table of descriptives by School District and Treatment
s4 <- tabular(
    (Log.Sale.Price+Lotsize+Bedrooms)*
        (RowFactor(x=Agency.Name,
                name="School District",
                levelnames=paste0("\\textit{",c("Champaign","Urbana"),"}"),
                spacing=3,space=5)*
         RowFactor(x=Post.Choice,
                   name="Post Choice",
                   levelnames=c("Pre-","Post-"),
                   spacing=3,space=5)) ~
            ((n=1) + Format(digits=3)*(Mean + Median + SD)) ,
    ##data=s0[SR_VAL_TRANSFER>0,])
    data=s0)
