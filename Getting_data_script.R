rm(list=ls()) # clear out enviroment, good practice.

setwd("/Users/ful08h/Documents/Data_School/data for project/") # Set working directory where data is stored.
   
library(tidyverse)  # load tidyverse library
library(tidync)     # load tidync library
library(dplyr)      # load dplyr library
library(RNetCDF)    # load RNetCDF library
library(ncdf4)      # load ncdf libray
# library(raster)     # load raster library

############################################## ncdf4 VERSION ##############################################

# open the ncdf file to extract the data.
# TheDataFile.nc <- nc_open('gbr4_simple_2020-03.nc', verbose = TRUE, write = FALSE)

# Old data extract - files too big so had to find another way
# saltRaw <- ncvar_get(TheDataFile.nc,'salt') # salinity
# tempRaw <- ncvar_get(TheDataFile.nc,'temp') # temperature
# timeRaw <- ncvar_get(TheDataFile.nc,'time') # time
# dhwRaw <- ncvar_get(TheDataFile.nc,'dhw')   # dhw = degree heating week

# nc_close(TheDataFile.nc) # close the data file just created.

############################################## tidync VERSION ##############################################

# So lets do it with tidync instead

filename <- "gbr4_simple_2020-03.nc"
TheDataFile.nc <- tidync(filename)  # Opening file using tidync of name defined in filename
TheDataFile.nc                      # To se dimensions ofthe nc file

# extract the variables ("suck out the data") - will need to filter on indexs to avoid large space of NANs in file


# rbind or merge (or similar) to get into right datafram format


# plot up temperature vs salinity



# plot through time

