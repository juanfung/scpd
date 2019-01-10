## Summary

## Load paths, packages, and functions for "Prep/*.R" files

## ----------------------------------------------------------------------------------- #

## using rprojroot via here for better directory management
library("here")

## Path to data, change as necessary

path.to.data <- here("Data")
path.to.custom <- here("Functions")

## ----------------------------------------------------------------------------------- #

## Packages #

library("data.table")
## TODO:
## replace usage of plyr with dplyr or appropriate substitute
library("plyr")
library("dplyr")

library("rgdal")
library("rgeos")
library("sf")
library("maptools")

## Functions #

source(file.path(path.to.custom, "prepFunctions.R"))

## ----------------------------------------------------------------------------------- #
