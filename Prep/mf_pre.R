## Multiple imputation for Dataquick

## ----------------------------------------------------------------------------------- #

## load paths,packages, and functions
source("prepLoad.R")

## load package(s) for multiple imputation
library(missForest)
library(doMC)

## ----------------------------------------------------------------------------------- #

## load the data

## Dataquick
load(file=paste0(path.to.data,"Processed/mf/dt_xz_medium.Rda"),verbose=TRUE)

## ----------------------------------------------------------------------------------- #

## Multiple imputation using "missForest"

## Set parameters
## NB: variables not scoping for parallel

nc = 10 # number of workers
nit = 30  # max number of iterations
ntree = 300 # number of trees (default: 100)
ntry = 10 # number of splits (default: p^0.5)
ndfs = 10 # number of data.frames to save
par = "forests" # parallelization: c("forests","variables","no")

registerDoMC(nc) # register parallel backend

## ----------------------------------------------------------------------------------- #

## generated ndfs multiply imputed df's

## initialize list for storage
path.to.list = paste0(path.to.data,"Processed/mf/mf_list.Rda")
imp.list = list()
save(imp.list, file=path.to.list )
rm(imp.list)
gc()

## impute, save to list, collect garbage
for (i in 1:ndfs) {
    imp.xz = missForest(dt.xz, verbose=TRUE, maxiter=nit, ntree=ntree, parallelize=par)
    load( path.to.list ,verbose=TRUE )
    imp.list[[i]] = imp.xz
    save( imp.list, file=path.to.list )
    rm(imp.xz, imp.list)
    gc()
}

## ----------------------------------------------------------------------------------- #

## PID: 37364
