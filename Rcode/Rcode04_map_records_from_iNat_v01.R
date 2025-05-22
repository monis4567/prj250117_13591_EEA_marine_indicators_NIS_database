#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# “prj250117_13591_EEA_marine_indicators_NIS”
# Authors: Steen Wilhelm Knudsen.

library(plyr)
library(scales)
library(gplots)
library(fields)

## install the package 'scales', which will allow you to make points on your plot more transparent
# #install.packages("scales")
# if(!require(scales)){
#   install.packages("scales")
#   library(scales)
# }
library(scales)
# #install.packages("fields")
# if(!require(fields)){
#   install.packages("fields")
#   library(fields)
# }
library(fields)
## install the package 'gplots', to be able to translate colors to hex - function: col2hex
# #install.packages("gplots")
# if(!require(gplots)){
#   install.packages("gplots")
#   library(gplots)
# }
library(gplots)
library(ggplot2)
## install the package 'glad', to be able to color using the function 'myPalette'
#install.packages("glad")
#library(glad)
require(graphics)
#get package to do count number of observations that have the same value at earlier records:
# see this website: https://stackoverflow.com/questions/11957205/how-can-i-derive-a-variable-in-r-showing-the-number-of-observations-that-have-th
# #install.packages("plyr")
# if(!require(plyr)){
#   install.packages("plyr")
#   library(plyr)
# }
library(plyr)

# if(!require(xlsx)){
#   install.packages("xlsx")
#   library(xlsx)
# }
library(xlsx)

#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("mapdata")
#library(mapdata)
#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("maps")
#library(maps)
# #get package for shapefiles see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
# install.packages(maptools)
# library(maptools)  #for shapefiles
# #get package for adding pies on the map
#install.packages("mapplots")
#library(mapplots)
# # devtools::install_github("davidgohel/ReporteRs")
# # devtools::install_github("davidgohel/officer")
# if(!require(officer)){
#   install.packages("officer")
#   library(officer)
# }
library(officer)

# if(!require(splitstackshape)){
#   install.packages("splitstackshape")
#   library(splitstackshape)
# }
library(splitstackshape)
#install.packages("tableHTML")
# #https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
# if(!require(tableHTML)){
#   install.packages("tableHTML")
#   library(tableHTML)
# }
library(tableHTML)
# # install package if required
# if(!require(envDocument)){
#   install.packages("envDocument")
#   library(envDocument)
# }
library(envDocument)


#_______________________________________________________________________________
# First (before making maps)  make sure all the required packages are loaded
# https://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
# #install packages needed
# if(!require(maps)){
#   install.packages("maps")
# }
# if(!require(ggplot2)){
#   install.packages("ggplot2")
# }
library(ggplot2)
library(maps)
# # #https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# To get rgdal and googleway to work,
#first run these in a terminal:

# $ sudo apt install netcdf-*
# $   sudo apt install libnetcdf-dev
# $ sudo apt install libjq-dev
# $ sudo apt install gdal-bin libgdal-dev libproj-dev
# $ sudo apt install libudunits2-dev
# if(!require(cowplot)){
#   install.packages("cowplot")
# }
library(cowplot)

# if(!require(googleway)){
#   install.packages("googleway")
# }
library(googleway)

# if(!require(ggrepel)){
#   install.packages("ggrepel")
# }
library(ggrepel)

# if(!require(ggspatial)){
#   install.packages("ggspatial")
# }
library(ggspatial)

# if(!require(libwgeom)){
#   install.packages("libwgeom")
#   library(libwgeom)
# }
# if(!require(sf)){
#   install.packages("sf")
# }
library(sf)

# if(!require(rnaturalearth)){
#   install.packages("rnaturalearth")
# }
library(rnaturalearth)

# if(!require(rnaturalearthdata)){
#   install.packages("rnaturalearthdata")
# }
library(rnaturalearthdata)
# if(!require(ggforce)){
#   install.packages("ggforce")
# }
library(ggforce)
#get 'rnaturalearthhires' installed
# if(!require(rnaturalearthhires)){
#   #install.packages("rnaturalearthhires")
#   install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
# }
library(rnaturalearthhires)
# # 
library("ggplot2")
library("sf")
library(ggforce)

theme_set(theme_bw())
#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
# # 
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
# # Get a map, use a high number for 'scale' for a coarse resolution
# use a low number for scale for a high resolution
# if the map 'world' does not exist, then download it
world <- ne_countries(scale = 10, returnclass = "sf")
library(ggplot2)

#https://www.eleanor-jackson.com/post/searching-for-spring/
options(stringsAsFactors = F)
#get spocc package
# if(!require(spocc)){
#   install.packages("spocc")
# 
# }  
library(spocc)
# #get rinat package
# if(!require(rinat)){
#   remotes::install_github("ropensci/rinat")
#   install.packages("rinat")
# }  
library(rinat)
library("tidyverse")
library("httr")
library("jsonlite")
library("dplyr") 
#load libraries
library(readr)
library(rgbif)
library(dismo)
library(tidyr)

# define working directory
wd00 <- getwd()
# define output directory
wdout <- "output03_map_fetched_records_from_iNat"
# define output directory
wdin01 <- "output01_EEA_NIS_list"
wdin02 <- "output02_fetched_records_from_GBIF_iNat"
#paste dirs together
wd00_wdout <- paste0(wd00,"/",wdout)
##Delete any previous versions of the output directory
unlink(wd00_wdout, recursive=TRUE)
##Create a directory to put resulting output files in
dir.create(wd00_wdout)
# read in the table03 from  output01 directory
dfts <- read.delim(paste0(wd00,"/",wdin01,"/",
                          "table03_EUR_geogr_reg.csv"), 
                   header = T, 
                   sep = ";")
# read in the fetched records from iNat in the output02 directory
dfiN <- read.delim(paste0(wd00,"/",wdin02,"/",
                          "iNat_rec_all.csv"), 
                   header = T, 
                   sep = ";")
nrow(dfiN)
nrow(df_g03)

# limit the 'dfts' data frame to only have unique values in the 'ScientificName' column
# as the next step using left_join only has the purpose of 
# getting the taxonomic information for each species
dfts <- dfts[!duplicated(dfts$ScientificName),]

# use left join to add the geographic region to the df_g03 data frame
# using the 'scientific name' as the key in the df_g03 data frame
# and the 'ScientificName' as the key in the dfts data frame
df_g04 <- dplyr::left_join(dfiN, dfts, by = c("scientific_name2" = "ScientificName"))
colnames(df_g04)
nrow(df_g04)
# omit the rows that have NA in 'longitude' column
df_g04 <- df_g04[!is.na(df_g04$longitude),]
# count the unique values in the 'Class' column
df_g04 %>% 
  dplyr::count(Class) %>% 
  dplyr::arrange(desc(n))
# count the unique values in the 'Order' column
df_g04 %>% 
  dplyr::count(Order) %>% 
  dplyr::arrange(desc(n))
# https://github.com/cran/rinat
#_______________________________________________________________________________
# nelng cuts on the eastern boundary
# nelat cut on the northern border
#                             |
#                       nelng |
#             N         nelat___    |
#             |                     | y-axis is lat
#             |                     |
#             |                     |
#   W____________________E          |
#             |                     |
#             |                     |
#             |                     |
# ___  swlat  S                     |
#     |swlng
#     |
#_____________________________
#             x-axis is lon
# swlng cuts on the western boundary
# swlat cut on the southern border
#try defining your own bounding box
set_nelat= 58
set_nelng= 15.4
set_swlat= 54.4
set_swlng= 8
#try defining your own bounding box
set_nelat= 70
set_nelng= 50
set_swlat= 10
set_swlng= -30
# needs to be in the format 
#  'min_lat','min_lon','max_lat', 'max_lon'
# whcih equals
#  'min_y','min_x','max_y', 'max_x'
boundslim <- c(set_swlat, set_swlng, set_nelat, set_nelng)

# #make a ggplot map with facet wrap per phylum
# where the points are colored by the 'Class' column
plt_g03 <- ggplot(data = df_g04, aes(x = longitude,
                                      y = latitude,
                                      colour = Class)) +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               fill = "grey95",
               color = "gray40",
               linewidth = 0.1) +
  geom_point(size = 0.7, alpha = 0.5) +
  # coord_fixed(xlim = range(df_iNat01$longitude, na.rm = TRUE),
  #             ylim = range(df_iNat01$latitude, na.rm = TRUE)) +
  coord_fixed(xlim = c(set_swlng,
                       set_nelng) ,
              ylim = c(set_swlat,
                       set_nelat) ) +
  facet_wrap(~Phylum) +
  theme_bw()
# see the plot
#plt_g03
# store the plot as a png file in the wdout directory
ggsave(paste0(wd00_wdout,"/","Fig03_iNat_map_records_by_phylum.png"),
       plt_g03,
       width = 12,
       height = 8,
       dpi = 300)
