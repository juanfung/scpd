# Create Stata dictionary for import

# ----------------------------------------------------------------------------------- #

# path to files, change as necessary
path.to.data <- "/home/juanfung/Data/Dataquick/"

# ----------------------------------------------------------------------------------- #

# Choose layout file: "assessor_layout" or "history_layout" - change as necessary
# Layout files contain column names, classes, and widths; convert xls to csv
lay.raw <- "UIL_ASSR_LAYOUT"

# path to layout, change as necessary (assr.lay, hist.lay)
path.to.layout <- paste0(path.to.data,lay.raw,".csv")

# ----------------------------------------------------------------------------------- #

# Import column names, classes, and lengths
layout <- read.table(file=path.to.layout,header=TRUE,sep=",",fill=TRUE,allowEscapes=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,blank.lines.skip=TRUE,skipNul=TRUE,quote="\"")

# drop any "false" fields, i.e., Field.Number==NA (e.g., "Totals")
layout <- layout[!is.na(layout[,"Field.Number"]),]

# Optional: save as new csv, after trimming white space, etc.
# write.table(layout,file=paste0(path.to.data,lay.raw,".txt"))

layout.names <- c(layout$Field.Name)
layout.length <- c(layout$Length)
layout.class <- c(layout$ANSI.Standard.Data.Type)

# re-name with stata formats
layout.class <- gsub("numeric","float",layout.class)
layout.class <- gsub("smallint","byte",layout.class)
layout.class <- gsub("^((?!float|byte).)*$","str",layout.class,perl=T)

# create start-end positions
layout.cumsum <- cumsum(layout.length)
layout.start <- layout.cumsum - layout.length + 1
layout.pos <- paste(as.character(layout.start),as.character(layout.cumsum),sep="-")

# create and save dictionary
layout.dict <- cbind(layout.class,layout.names,layout.pos)
write.table(layout.dict,file=paste0(path.to.data,lay.raw,".dct"),sep=" ",quote=F,row.names=FALSE,col.names=FALSE)

# Open text editor
# add to top: infix dictionary using NAME_OF_DATA_FILE.TXT {
# add to bottom: }
