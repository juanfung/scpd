## Post process imputed datasets

## ----------------------------------------------------------------------------------- #

## load paths,packages, and functions
source("prepLoad.R")

## load package(s) for multiple imputation
library(missForest)
##library(doMC)

## ----------------------------------------------------------------------------------- #

## load the "original" data
load(file=paste0(path.to.data,"Processed/mf/dt_xz_medium.Rda"),verbose=TRUE)

## load the imputed data
path.to.list = paste0(path.to.data,"Processed/mf/mf_list.Rda")

load( path.to.list ,verbose=TRUE )

## ----------------------------------------------------------------------------------- #

## inspect OOB errors


## ----------------------------------------------------------------------------------- #

## Replce dummy* vars in imp.xz with TRACTCE00 factor in dt.xz, merge by My.ID
dummies = grep("dummy", names(dt.xz), value=TRUE, ignore.case=TRUE)

## load the original data without dummies
load(file=paste0(path.to.data,"Processed/dt_xz.Rda"),verbose=TRUE)

dropvars = setdiff(names(dt.xz), c("My.ID", "TRACTCE00", "Post.Choice", "d.obs", "Sale.Date"))
dt.xz[, (dropvars):=NULL ]


## ----------------------------------------------------------------------------------- #

for (i in 1:length(imp.list)) {
    df = imp.list[[i]]$ximp
    df[, (dummies):=NULL ]
    df = merge(df, dt.xz, by="My.ID")
    imp.list[[i]] = df
}

facs = c("ProxB", "TRACTCE00", "Agency.Name", "Post.Choice", "d.obs")
imp.list = lapply(imp.list, function(df)
    df[ , (facs):=lapply(.SD,as.factor), .SDcols=facs] )

## ----------------------------------------------------------------------------------- #

## misc clean up cus i was sloppy the first time...

## 1. subset pre-Urbana attendance area changes
##dt.xz.merge = dt.xz[,.(My.ID, Sale.Date)]
##imp.list = lapply(imp.list, function(df) merge(df, dt.xz.merge, by="My.ID", all.x=TRUE) )
imp.list = lapply(imp.list, function(df) df[ Sale.Date < as.IDate("2002-06-01") ] )
##imp.list = lapply(imp.list, function(df) df[ Year < 2002 ] )

## 2. drop extreme prices: approx between 2nd and 98th quantile (ie, drop top/bottom 2%)
##imp.list = lapply(imp.list, function(df) df[ Sale.Price > 15000 & Sale.Price < 500000 ] )
## drop top/bottom 1%?
imp.list = lapply(imp.list, function(df) df[ Sale.Price > 10000 & Sale.Price < 1000000 ] )

## 3. convert numeric year to factor?
## NB conflict with "Post.Choice" factor
##as.num.fac = function(f) { as.numeric(levels(f)[f]) }
##imp.list = lapply(imp.list, function(df) df[, Year:=as.num.fac(Year)] )
imp.list = lapply(imp.list, function(df) df[, Year:=as.factor(Year)] )

## 4. labeling selection variable
imp.list = lapply(imp.list, function(df) df[Agency.Name==dis & Post.Choice=="1", d.obs:="1"])

imp.list = lapply(imp.list, function(df) df[!(Agency.Name==dis & Post.Choice=="1"), d.obs:="0"])

imp.list = lapply(imp.list, function(df)
    df[,`:=`(Agency.Name=as.factor(Agency.Name), d.obs=as.factor(d.obs))] )

## 5. misc fixes
imp.list = lapply(imp.list, function(df)
    df[,ProxB:=gsub("^Distance\\.(.*)","\\1",ProxB)])

imp.list = lapply(imp.list, function(df)
    df[,`:=`(TRACTCE00=as.factor(TRACTCE00))] )

## ----------------------------------------------------------------------------------- #

## variable names
yvars = c("Sale.Price", "Log.Sale.Price")

dvars = c("d.obs")

xvars = c(
    ##"Sale.Date",
    ##"Year",
    "Sqft",
    "Lotsize",
    "Age.Home",
    "Garage", # binary
    "Basement",
    "Bedrooms",
    "Baths.Total",
    "Baths.Full",
    "Baths.Half",
    "Agency.Name", # binary = T/C
    "Post.Choice", # binary = pre/post
    "Zip.Code",
    "Neighborhood.School", # 16 levels
    "Neighborhood.School.Distance",
    "Closest.School", # 16 levels
    "Closest.School.Distance",
    "ProxA.Total",
    "ProxB", # 8 levels
    ##"Census.Tract.Number", # 59 levels
    "TRACTCE00", # 42 levels
    ##"Block.Group.00", # 165 levels
    ##"Population",
    "Households",
    "Median.Household.Income",
    ##"Families.Below.Poverty",
    "Total.Housing.Units",
    "Median.Rooms",
    ##"Occupied.Units",
    "Owner.Occupied.Units",
    ##"Renter.Occupied.Units",
    "Owner.Occupied.Median.Value"
    ##"Median.Gross.Rent"
)

zvars = c("Loan",
          "Buyer.Origin",
          "Income",
          "Applicant.Race.1",
          "Applicant.Sex")

idvars = c(
    "My.ID",
    "PIN",
    "Longitude",
    "Latitude"
) 

var.list = list("yvars"=yvars,
                "dvars"=dvars,
                "xvars"=xvars,
                "zvars"=zvars,
                "idvars"=idvars)

save(var.list, imp.list, file=paste0(path.to.data, "Processed/mf/mf_analyze.Rda") )
