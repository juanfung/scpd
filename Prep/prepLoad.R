## Summary

## Load paths, packages, and functions for "Prep/*.R" files

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

library(rgdal)
library(rgeos)
library(maptools)

## Functions #

source(paste0(path.to.custom,"prepFunctions.R"))

## ----------------------------------------------------------------------------------- #
