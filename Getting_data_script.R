rm(list=ls()) # clear out enviroment, good practice.

setwd("/Users/ful08h/Documents/Data_School/Data_school_project/") # Set working directory where data is stored.
   
library(tidyverse)  # load tidyverse library
library(tidync)     # load tidync library
library(dplyr)      # load dplyr library
library(ggplot2)    # ggplot library
library(pals)       # even more palettes
library(gridExtra)  # load gridextra library

# Assume entire grid active

sstdata <- tidync('gbr4_simple_2020-03.nc') %>%
  hyper_filter(k = between(k, 46, 47)) %>%    # subsetting to just the two surface layers.
  hyper_tibble(select_var = c("temp", "dhw")) # from the subset, create a tibble only of variables 'temp' and 'dhw'

# Subset to D1, D2 active and merge into one data frame.

sstdata2D <- tidync('gbr4_simple_2020-03.nc') %>% 
  activate('D1,D2') %>% 
  hyper_tibble(select_var = c('latitude', 'longitude'))
sstDataCombo <- merge(sstdata, sstdata2D, by=c('i','j'))

# To choose the palettes you need to see the density of the data distribution to see natural break points.

sstdata_surf <- filter(sstDataCombo, k == 47) #filter the tibble to the surface layer and all the time steps.

ggplot(sstdata_surf, aes(x = temp)) + 
  geom_density()

# based on the plot above I don't need values less than 25.

ggplot(sstdata_surf, aes(x = dhw)) + 
  geom_density()

# based on this plot I don't need numbers more than 10. Masking the outliner values allows greater focus on the most dense data points.

# look at the properties through time.

sstdata_mask <- sstdata_surf %>% 
  filter(between(dhw, 0, 10), between(temp, 25, 40)) #filter the dhw to meaningful values and temperature to values off Qld

# Get summary statistics.

sstdata_dhwSum <- sstdata_mask %>% 
  group_by(time) %>% 
  dplyr::summarise(dhwSum = sum(dhw), dhwMax = max(dhw), dhwMean = mean(dhw), dhwSD = sd(dhw), SSTmean = mean(temp), SSTmax = max(temp))
sstdata_dhwSum$NewTime <- sstdata_dhwSum$time - min(sstdata_dhwSum$time)

# Plot the different summary statistics - turns out dhw is boring so do max SST (SST as well as short for 'sea surface temperature'
# also is short for 'Starship Troopers').

p1 <- ggplot(sstdata_dhwSum, aes(x = NewTime, y = dhwSum)) +
  geom_line(size = 2, colour = 'violetred4') +
  labs(x = 'Days Since Feb 29 2020', y = 'Degree Heating Week SUM') + 
  theme_bw() + theme(axis.title = element_text(face = 'bold'))

p2 <- ggplot(sstdata_dhwSum, aes(x = NewTime, y = dhwMax)) +
  geom_line(size = 2, colour = 'darkgreen') +
  labs(x = 'Days Since Feb 29 2020', y = 'Degree Heating Week MAX') + 
  theme_bw() + theme(axis.title = element_text(face = 'bold'))

p3 <- ggplot(sstdata_dhwSum, aes(x = NewTime, y = dhwMean)) +
  geom_line(size = 2, colour = 'hotpink3') +
  labs(x = 'Days Since Feb 29 2020', y = 'Degree Heating Week MEAN') + 
  theme_bw() + theme(axis.title = element_text(face = 'bold'))

p4 <- ggplot(sstdata_dhwSum, aes(x = NewTime, y = dhwSD)) +
  geom_line(size = 2, colour = 'palegreen3') +
  labs(x = 'Days Since Feb 29 2020', y = 'Degree Heating Week STANDARD DEVIATION') + 
  theme_bw() + theme(axis.title = element_text(face = 'bold'))

p5 <- ggplot(sstdata_dhwSum, aes(x = NewTime, y = SSTmean, colour = SSTmean)) +
  geom_line(size = 2) +
  scale_colour_gradientn(colours = ocean.balance(100)) +
  labs(x = 'Days Since Feb 29 2020', y = 'Sea Surface Temperature MEAN') + 
  theme_bw() + theme(axis.title = element_text(face = 'bold'))

p6 <- ggplot(sstdata_dhwSum, aes(x = NewTime, y = SSTmax, colour = SSTmax)) +
  geom_line(size = 2) +
  scale_colour_gradientn(colours = ocean.balance(100)) +
  labs(x = 'Days Since Feb 29 2020', y = 'Sea Surface Temperature MAX') + 
  theme_bw() + theme(axis.title = element_text(face = 'bold'))

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)

# SSTMax is interesting, time to draw some maps.

# Plot 1  - Plot a single time stamp on plan unprojected model grid.

sstdata_sub <- filter(sstdata_mask, time == 11017)     # filter the tibble to single timestep.
imax <- max(sstdata$i)                                 # this line finds the largest value of 'i' and assigns it to the variable 'imax'.
sstdata_sub$yinvert <- imax - sstdata_sub$i            # take imax minus i and assign it to the variable yinvert. otherwise the plot will be upside down.
ggplot(sstdata_sub, aes(x=j, y=yinvert, fill=temp)) + 
  geom_raster() +
  scale_fill_gradientn(colours = ocean.balance(100)) +
  labs(x = 'i', y = 'j', fill = 'Sea Surface\nTemperature(C)')

# Plot 2 - Or more appropiately using latitude and longitude.

ggplot(sstdata_sub, aes(x = longitude, latitude, colour = temp)) +
  geom_point(size = 1, alpha = 1) +
  scale_colour_gradientn(colours = ocean.balance(100)) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Sea Surface\nTemperature(C)') +
  coord_quickmap()
                      
# Do the animation.

#sstdata_mask$yinvert <- imax - sstdata_mask$i
#ggplot(sstdata_mask, aes(x = j, y = yinvert, fill = temp)) +
#  geom_raster() +
#  scale_fill_gradientn(colours = ocean.balance(100)) +
#  transition_states(time)

# Plot of dhw

ggplot(sstdata_sub, aes(x = j, y = yinvert, fill = dhw)) +
  geom_raster() +
  scale_fill_gradientn(colours = ocean.curl(100))

# Subset to just D1, D2 active and melt into one data frame

# sstdata2D <- tidync('gbr4_simple_2020-03.nc') %>% activate("D1,D2") %>%
#  hyper_tibble(select_var = c("latitude", "longitude"))
# sstdataCombo <- merge(sstdata, sstdata2D, by=c("i","j"))
# sstdata_sub2 <- filter(sstdataCombo, time == 11017)
# ggplot(sstdata_sub2, aes(x=longitude, y=latitude, fill=temp)) + 
#  geom_tile() +
#  scale_color_viridis() +
#  scale_fill_viridis() 
  
# rbind or merge (or similar) to get into right dataframe format


# plot up temperature vs salinity



# plot through time

