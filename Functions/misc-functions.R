## misc versions of prepFunctions.R

## For converting factors to numeric
##as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
## equivalent to function in prepFunctions.R

## "dollars as strings" remove dollar signs, commas, etc and convert to numeric
##deDollarize <- function(x) {
##    x <- gsub("^(\\d+)\\s.*","\\1",x) # clean up extra space
##    x <- as.numeric(gsub("\\$|,","",x)) # remove {$,}
##    return(x)
##}

