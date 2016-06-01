## Visualize sales prices

## Load paths, packages, and functions

## ----------------------------------------------------------------------------------- #

## set path; change as necessary
home.dir <- ".."

## Path to data, change as necessary

path.to.data <- paste0(home.dir,"/Data/")
path.to.custom <- paste0(home.dir,"/Functions/")

## ----------------------------------------------------------------------------------- #

## Packages #

library(data.table)
library(plyr)
library(dplyr)
library(reshape2)

library(ggplot2)
library(ggmap)
library(ggthemes)
library(Cairo)
library(gridExtra)

## ----------------------------------------------------------------------------------- #

# Data

load(file=paste0(path.to.data,"Processed/dt_ha.Rda"),verbose=TRUE)

## only positive sale price
dt.ha <- dt.ha[SR_VAL_TRANSFER>0,]

## alt: only arms-length
## dt.ha <- dt.ha[SR_ARMS_LENGTH_FLAG>0,]

## Keep both cases:
## dt.ha <- dt.ha[xor(SR_VAL_TRANSFER>0,(as.numeric(SR_ARMS_LENGTH_FLAG)==1&SR_VAL_TRANSFER==0)),]

## drop outliers
dt.ha <- dt.ha[Log.Sale.Price<7&Log.Sale.Price>1,]

## rename sale date
setnames(dt.ha,old="SR_DATE_TRANSFER",new="Sale.Date")


## ----------------------------------------------------------------------------------- #

## Group by Champaign and Urbana school districts and subset
plot.df <- dt.ha[grep("CHAMPAIGN|URBANA",Agency.Name),.(Year,Sale.Date,Log.Sale.Price,Agency.Name,Post.Choice)]
##plot.df <- s0[,.(Year,Sale.Date,Log.Sale.Price,Agency.Name,Post.Choice)]

## scale price by variance?
##plot.df[,Price:=Log.Sale.Price/var(Log.Sale.Price)]

## alt: include Mahomet
## plot.df <- dt.ha[grep("CHAMPAIGN|URBANA|MAHOMET",Agency.Name),.(Year,Log.Sale.Price,Agency.Name)]


## ----------------------------------------------------------------------------------- #

## Plot distribution of (log) sale price (pooled over time VS. PRE/POST)

## separate layers
ggplot(plot.df, aes(Log.Sale.Price) ) + geom_density() + facet_wrap(~Agency.Name+Post.Choice,ncol=2)

## single layer
##q <- ggplot(plot.df,aes(Log.Sale.Price,color=Agency.Name))+geom_density()

plot.df[,Log.Sale.Price:=log(MLS.Sale.Price)]

q <- ggplot(plot.df, aes(Log.Sale.Price,fill=Agency.Name)) +
    geom_density(alpha=0.2) +
    facet_wrap(~Post.Choice, ncol=2)

## save pdf
##cairo_pdf("../Paper/Figs/price_distribution.pdf",width=6,height=4.5)
cairo_pdf("../Paper/Figs/price_distribution_pre_post.pdf",width=8,height=4.5)
print(q)
dev.off()

( champs <- ggplot(t0[Year<2002], aes(GR3.MATH.SCHOOL.EXCEEDS.ALL, fill=as.factor(Post.Choice))) +
      geom_density(alpha=0.2) +
      facet_wrap(~Agency.Name, ncol=2) )


## ----------------------------------------------------------------------------------- #

## Plot median (log) sale price

sum.df <- group_by(plot.df, Year, Agency.Name)

sum.df <- summarise(sum.df,
                    med_price=median(Log.Sale.Price,na.rm=TRUE),
                    mean_price=mean(Log.Sale.Price,na.rm=TRUE))

## plot on single layer using colour and group

( q2 <- ggplot(sum.df,aes(x=Year,y=med_price,colour=Agency.Name,group=Agency.Name))+geom_line() )

## plot separately using facets

q3 <- ggplot(sum.df,aes(x=Year,y=med_price,colour=Agency.Name))+
    geom_line(size=1.2)+
    facet_wrap(~Agency.Name,scales='free_y',ncol=1)

## add x and y labels
q3 <- q3 + labs(x = "Year", y = "Log sale price", fill = "")

## drop legend (redundant for facets)
q3 <- q3 + theme(legend.position = "none")

## save pdf
cairo_pdf("../Paper/Figs/price_time_series.pdf",width=6,height=4.5)
print(q3)
dev.off()


## REMARK: try volume adjusted?


## ----------------------------------------------------------------------------------- #

## Plot full (noisy) time series

## melt by date and school district
melt.df <- melt(plot.df,id.vars=c("Sale.Date","Agency.Name"),measure.vars="Log.Sale.Price")

## plot multiple layers
q <- ggplot(melt.df,aes(x=Sale.Date,y=value))+
    geom_line()+
    facet_wrap(~Agency.Name,scales='free_y',ncol=1)

q <- q + geom_smooth()

## save pdf
cairo_pdf("../Paper/Figs/price_time_series_full.pdf",width=6,height=4.5)
print(q)
dev.off()


## plot BOTH? e.g.,

## create champaign subset
## create urbana subset
## plot something like:
## ggplot() + geom_line(data=melt.df,aes=(x=date,y=value,color="red")) + geom_line(data=plot.df,aes=(x=date,y=value,color="blue"))
## NB: different x-axis!



## ----------------------------------------------------------------------------------- #

## alt: plot mls prices, county prices, etc.

