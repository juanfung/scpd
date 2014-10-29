# Summary:
# Import original housing data from dataquick (in fixed width format) #

# ----------------------------------------------------------------------------------- #

# Packages #

library(LaF)

# ----------------------------------------------------------------------------------- #

# Path to data, change as necessary

path.to.data <- "/home/juanfung/googledrive/Data"

# ----------------------------------------------------------------------------------- #

# Files, change as necessary #

# "UOI_ASSR_INITIAL.txt" - property data
path.assr <- paste0(path.to.data,"/Dataquick/UOI_ASSR_INITIAL.txt")

# "UOI_HIST_INITIAL.txt" - transaction data
path.hist <- paste0(path.to.data,"/Dataquick/UOI_HIST_INITIAL.txt") 

# "UOI\ Assessor\ Layout.xls" - column names, classes, and widths; I converted xls to csv
path.assr.layout <- paste0(path.to.data,"/Dataquick/assessor_layout.csv")

# "UOI\ History\ Layout.xls" - column names, classes, and widths; I converted xls to csv
path.hist.layout <- paste0(path.to.data,"/Dataquick/history_layout.csv")

# N.B. To match assr and hist data: SR_PROPERTY_ID == SA_PROPERTY_ID
# From dataquick: 
# SR_PROPERTY_ID "Joined to Assessor data to merge Assessor and Recorder data.  Internal identification number assigned to every property"
# SA_PROPERTY_ID "Unique DataQuick primary key identifier assigned to a property."

# ----------------------------------------------------------------------------------- #

# 1. Import data: summary #

# Import method, using LaF package:
# create LaF "link" (laf object) to original data
# pre-specify column classes, optional column names, and column widths for fixed width
# once "linked", can import select rows or columns to a data.frame

# EXAMPLE (from help)
# dat <- laf_open_fwf(filename="file.fwf", column_types=c("integer", "categorical", "string", "integer", "double"), column_names=c("id", "gender", "postcode", "age", "income"), column_widths=c(6, 1, 6, 3, 8))
# where column_types is character vector of variable classes modified so that:
# "numeric" <-> "double";
# "character" <-> "string";
# optionally can pre-define "character" as "categorical"
# N.B.: csv should not have header, set option "skip=1"
# N.B.: LaF handles selective import of fixed width as well

# ----------------------------------------------------------------------------------- #

# 1a. Import assessment data #

# Import column names, classes, and lengths
assr.layout <- read.table(file=path.assr.layout,header=TRUE,sep=",",fill=TRUE,allowEscapes=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,blank.lines.skip=TRUE,skipNul=TRUE)

# drop any "false" fields, i.e., Field.Number==NA (e.g., "Totals")
assr.layout <- assr.layout[!is.na(assr.layout[,"Field.Number"]),]

# save as new csv, after trimming white space, etc.
write.table(assr.layout,file=paste0(path.to.data,"/assr_layout.csv"))

assr.names <- c(assr.layout$Field.Name)
assr.lengths <- c(assr.layout$Length)
assr.class <- c(assr.layout$ANSI.Standard.Data.Type)
# Modify class for LaF format as specified above:
 assr.class <- gsub("No Equivalent","string",assr.class)
 assr.class <- gsub("timestamp","string",assr.class)
 assr.class <- gsub("character varying","string",assr.class)
 assr.class <- gsub("numeric","double",assr.class)
 assr.class <- gsub("smallint","integer",assr.class)

# Create LaF object (link to data)
assr.laf.fwf <- laf_open_fwf(filename=path.assr,column_types=assr.class,column_names=assr.names,column_widths=assr.lengths,trim=TRUE)

############
# OPTIONAL #

# save above as data model for re-use, possible with other DQ data:
# write_dm(assr.laf.fwf, "assrfwf.yaml")

# to re-use data model, e.g., 
# laf2 <- laf_open(read_dm("assrfwf.yaml", filename="/path/to/anotherfile.txt"))

############

# Selective (subset) import; change as necessary

# create a data.frame such that MM_STATE_CODE == "IL"
assr.laf.fwf.il <- assr.laf.fwf[assr.laf.fwf[,"MM_STATE_CODE"] == "IL" , ]

# save
save(assr.laf.fwf.il,file=paste0(path.to.data,"/ILassr.Rda"))

# ----------------------------------------------------------------------------------- #

# 1b. Import transaction data, same as above

# Import column names, classes, and lengths; same as before
hist.layout <- read.csv(file=path.hist.layout,header=TRUE,sep=",",fill=TRUE,allowEscapes=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,blank.lines.skip=TRUE,skipNul=TRUE)

# drop any "false" fields, i.e., Field.Number==NA (e.g., "Totals")
hist.layout <- hist.layout[!is.na(hist.layout[,"Field.Number"]),]

write.table(hist.layout,file=paste0(path.to.data,"/hist_layout.csv"))

hist.names <- c(hist.layout[,2])
hist.lengths <- c(hist.layout[,7])
hist.class <- c(hist.layout[,3])
# modify classes to match LaF format as specified above:
 hist.class <- gsub("character varying","string",hist.class)
 hist.class <- gsub("numeric","double",hist.class)
 hist.class <- gsub("smallint","integer",hist.class)

# Create LaF link 
hist.laf.fwf <- laf_open_fwf(filename=path.hist,column_types=class.hist.laf,column_names=hist.names,column_widths=hist.lengths,trim=TRUE)

# Select "IL" data, by MM_STATE_CODE
hist.laf.fwf.il <- hist.laf.fwf[hist.laf.fwf[,"MM_STATE_CODE"] == "IL" , ]

# save 
save(hist.laf.fwf.il,file=paste0(path.to.data,"/ILhist.Rda"))
