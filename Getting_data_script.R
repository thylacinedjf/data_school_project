rm(list=ls()) # clear out enviroment, good practice.

setwd("/Users/ful08h/Documents/Data_School/data for project/") # Set working directory where data is stored.
   
library(tidyverse)  # load tidyverse library
library(ncdf4)      # load ncdf libray

# open the ncdf file to extract the data.

TheDataFile.nc <- nc_open('gbr4_simple_2020-03.nc', verbose = TRUE, write = FALSE)

# extract the variables ("suck out the data").

saltRaw <- ncvar_get(TheDataFile.nc,'salt') # salinity
tempRaw <- ncvar_get(TheDataFile.nc,'temp') # temperature
timeRaw <- ncvar_get(TheDataFile.nc,'time') # time
dhwRaw <- ncvar_get(TheDataFile.nc,'dhw')   # dhw = degree heating week

nc_close(TheDataFile.nc) # close the data file just created.
