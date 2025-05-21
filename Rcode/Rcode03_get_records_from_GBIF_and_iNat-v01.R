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
wdout <- "output02_fetched_records_from_GBIF_iNat"
# define output directory
wdin <- "output01_EEA_NIS_list"
#paste dirs together
wd00_wdout <- paste0(wd00,"/",wdout)
##Delete any previous versions of the output directory
unlink(wd00_wdout, recursive=TRUE)
##Create a directory to put resulting output files in
dir.create(wd00_wdout)

#_______________________________________________________________________________

# 01 start - make trycatch function, in case the genus name is not on GBIF 
#_________________________
# https://www.statology.org/r-trycatch/
try.c.iNat <- function(tx, boundslim, endval){
  tryCatch(
    {
      #get iNaturalist records
      g <- rinat::get_inat_obs(
        taxon_name = tx,
        quality = "research",
        geo=T, #only include geo referenced results
        bounds = boundslim,
        maxresults = endval)
      return(g)
    },
    error=function(e) {
      message('An Error Occurred')
      print(e)
    },
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
}
#_________________________
# 01 end - make trycatch function, in case the genus name is not on GBIF 
#_________________________


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
## Search using the boundslimits defined above
df_iNat01 <- get_inat_obs(taxon_name = "Mya arenaria",
                          quality = "research",
                          bounds = boundslim,
                          maxresults = 500)
# make the ggplot
plt_amp01 <- ggplot(data = df_iNat01, aes(x = longitude,
                                          y = latitude,
                                          colour = scientific_name)) +
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
                theme_bw()
# see the plot
plt_amp01
#_______________________________________________________________________________
# read in the file with the valid species names
dfts <- read.delim(paste0(wd00,"/",wdin,"/",
"table04_EUR_NIS_spc.csv"), 
          header = T, 
          sep = ";") 
# change the column header
colnames(dfts) <- c("scientific_name")
# make a list of column names to keep
kee <- c("scientific_name",
         "datetime",
         "place_guess",
         "latitude",
         "longitude",
         "tag_list",
         "common_name",
         "url",
         "image_url",
         "species_guess",
         "iconic_taxon_name",
         "taxon_id",
         "num_identification_agreements",
         "num_identification_disagreements",
         #"observed_on_string",
         "observed_on",
         #"time_observed_at",
         "time_zone")
# Make a list of the species names
lst_spcs <- dfts$scientific_name
length(lst_spcs)
# subset the list to only include the first 10 species
lst_spcs <- lst_spcs[1:4]
#make an empty list to use for collecting data frame
lst_tx_gobs <- list()
#start a growing number
i <- 1
#iterate over taxon names in list 
for (tx in lst_spcs)
{
  #  print(tx)}
  print(tx)
  # substitute the underscore
  tx <- gsub("_"," ",tx)
  ## Search for the  species using the boundslimits defined above
  g <- try.c.iNat(tx, boundslim, 5000)
  # limit to only specific columns, otherwise it ends up being too much data
  g <- g[kee]
  # check if there are no data, and in that case, add NAs for the 'kee' columns
  if(!is.null(colnames(g)))
  {g <- g} else {
    df_tmp <- as.data.frame(t(as.matrix(kee)))
    df_tmp <- rbind(df_tmp,rep(NA,length(kee)))
    colnames(df_tmp) <- df_tmp[1,]
    df_tmp <- df_tmp[-1,]
    g <- df_tmp
  }
  # add the taxon name that was used for making the search
  g$txNmsrch <- tx
  # make the entire data frame characters
  g[] <- lapply(g, as.character)
  # append the data frame to the list of data frames
  # store it as the i'th element 
  lst_tx_gobs[[i]] <- g
  # pad the count number with zeroes to have 5 digits
  pi <- sprintf("%05d", i)
  # substitute the space with an underscore
  txs <- gsub(" ","_",tx)
  #bind the rows in each list in to one data frame
  df_g03 <- data.table::rbindlist(lst_tx_gobs, fill=T)
  df_g03 <- as.data.frame(df_g03)
  # if there is no latitude, then omit the row
  df_g03 <- df_g03[!is.na(df_g03$lat),]
  
  # make a file name
  fn <- paste0("iNat_rec",pi,"_",txs,".csv")
  #show the file name
  print(fn)
  # paste the file name together with the output directory
  fn <- paste0(wd00_wdout,"/",fn)
  # store the data frame in a csv file
  write.csv(df_g03, fn, row.names = F, 
            col.names = T, sep = ";")
  # increase the count of i by one
  i <- i+1
  # end iteration over  species in the list
}
#bind the rows in each list in to one data frame
df_g03 <- data.table::rbindlist(lst_tx_gobs, fill=T)
df_g03 <- as.data.frame(df_g03)
# if there is no latitude, then omit the row
df_g03 <- df_g03[!is.na(df_g03$lat),]
# copy the column with the scientific name
df_g03$scientific_name2 <- df_g03$scientific_name
#View(df_g03)
# make all columns with numbers numeric using dplyr mutate
# and convert all other numeric columns
df_g03 <- df_g03 %>% 
  dplyr::mutate_at(c('latitude',
                     'longitude',
                     'taxon_id',
                     'num_identification_agreements',
                     'num_identification_disagreements'), as.numeric)

# #make a ggplot map with facet wrap per scientific name
# plt_g03 <- ggplot(data = df_g03, aes(x = longitude,
#                                       y = latitude,
#                                       colour = scientific_name)) +
#   geom_polygon(data = map_data("world"),
#                aes(x = long, y = lat, group = group),
#                fill = "grey95",
#                color = "gray40",
#                linewidth = 0.1) +
#   geom_point(size = 0.7, alpha = 0.5) +
#   # coord_fixed(xlim = range(df_iNat01$longitude, na.rm = TRUE),
#   #             ylim = range(df_iNat01$latitude, na.rm = TRUE)) +
#   coord_fixed(xlim = c(set_swlng,
#                        set_nelng) ,
#               ylim = c(set_swlat,
#                        set_nelat) ) +
#   facet_wrap(~scientific_name2) +
#   theme_bw()
# # see the plot
# plt_g03
