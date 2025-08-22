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
wdin02 <- "/home/hal9000/Documents/Dokumenter/NIVA_Ansaettelse_2025/records_from_iNaturalist_2025may21"
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
dfiN <- read.delim(paste0(wdin02,"/",
                          "iNat_rec_all.csv"), 
                   header = T, 
                   sep = ";")
nrow(dfiN)
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
#_______________________________________________________________________________
# prepare taxonomical group_catagories for the species
# to be able to assign gradient colors to the stacked bar plots
#https://stackoverflow.com/questions/49818271/stacked-barplot-with-colour-gradients-for-each-bar
library(ggplot2)
#::::
#::::
df_gT <- df_g04
df_gT$Lat_Species <- df_gT$scientific_name
# df_g04$Phylum
# make a column with class and order combined
df_gT$class_order <- paste0(df_gT$Class,"_" ,df_gT$Order)
# make a column with phylum, class and order combined
df_gT$phylum_class_order <- paste0(df_gT$Phylum,"_",df_gT$Class,"_" ,df_gT$Order)
df_gT$phylum_class_order_latspc <- paste0(df_gT$Phylum,"_",df_gT$Class,"_" ,df_gT$Order,"_",df_gT$Lat_Species)
df_gT$class_order_latspc <- paste0(df_gT$Class,"_" ,df_gT$Order,"_",df_gT$Lat_Species)
df_gT$order_latspc <- paste0(df_gT$Order,"_",df_gT$Lat_Species)
# make a column with kingdom, phylum, and class
df_gT$kingdom_phylum_class <- paste0(df_gT$Kingdom,"_",df_gT$Phylum,"_",df_gT$Class)
# combine categories
df_gT$group <- paste0(df_gT$Phylum, "-", 
                      df_gT$class_order,
                      sep = "")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# start - function 'ColourPalleteMulti'
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ColourPalleteMulti <- function(df, group, subgroup){
  # count the number of distinct rows in the 'df' data frame
  nrn <- df %>%
    dplyr::distinct() %>%
    nrow()
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup,
                                           group, sep="~" )), 
                          df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 130)(nrow(categories))) # Set the top of the colour pallete
  category.end  <- (scales::hue_pal(l = 9)(nrow(categories))) # set the bottom
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = 
                                                c(category.start[i], 
                                                  category.end[i]))(categories[i,2])}))
  # # Set the colour pallette to be the same length as the number of rows in the data frame
  # if (length(colours) < nrow(df)){
  #   colours <- c(colours, rep(colours[length(colours)], 
  #                             nrow(df) - length(colours)))
  # }
  length(colours)
  return(colours)
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# end - function 'ColourPalleteMulti'
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# subset the data frame to only have the columns 'class_order_latspc' and 'Phylum'
nrow(df_gT)
df_TS <- df_gT %>% dplyr::select(class_order_latspc, Phylum) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(class_order_latspc) %>%
  # exclude the row if the column 'Phylum' is NA
  dplyr::filter(!is.na(Phylum))

df_TO <- df_gT %>% dplyr::select(order_latspc, Class) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(order_latspc) %>%
  # exclude the row if the column 'Phylum' is NA
  dplyr::filter(!is.na(Class))
# Build the colour pallete using the function
col.phycla <-ColourPalleteMulti(df_TS , "Phylum", "class_order_latspc")
length(col.phycla)
# Build the colour pallete using the function
col.claord <-ColourPalleteMulti(df_TO , "Class", "order_latspc")
nrow(df_TS)
# combine columns to a data frame
df_cfphy <- as.data.frame(cbind(df_TS,col.phycla))
df_cfcla <- as.data.frame(cbind(df_TO,col.claord))
#_______________________________________________________________________________
# start function to make italics in the legend
#_______________________________________________________________________________
# https://stackoverflow.com/questions/59554096/ggplot2-italics-in-the-legend
toexpr <- function(x, plain = NULL) {
  getfun <- function(x) {
    ifelse(x == plain, "plain", "italic")
  }
  as.expression(unname(Map(function(f,v) substitute(f(v),
                                                    list(f=as.name(f), v=as.character(v))), getfun(x), x)))
}
#_______________________________________________________________________________
# end function to make italics in the legend
#_______________________________________________________________________________

# use dplyr left_join to add the 'col.phycla' column to the df_gT data frame
df_gT <- dplyr::left_join(df_gT, 
                          # deselect the 'Phylum' column from the df_cfphy data frame
                          df_cfphy %>% dplyr::select(-Phylum), 
                          by = c("class_order_latspc" = "class_order_latspc"))

# exclude row if 'Phylum' is NA
df_gT <- df_gT[!is.na(df_gT$Phylum),]
# make a subsetted version of 'df_gT', that only holds the columns
# 'Class' and 'col.phycla' , make it distinct by only 'Class'
df_gTCc <- df_gT %>% dplyr::select(Class, col.phycla) %>% 
  dplyr::group_by(Class) %>%
  dplyr::arrange(Class) %>%
  # only keep the first row of each group, to get a representative color
  dplyr::filter(row_number()==1)

# make a subsetted version of 'df_gT', that only holds the columns
# 'Order' and 'col.phycla' , make it distinct by only 'Class'
df_gTOc <- df_gT %>% dplyr::select(Order, col.phycla) %>% 
  dplyr::group_by(Order) %>%
  dplyr::arrange(Order) %>%
  # only keep the first row of each group, to get a representative color
  dplyr::filter(row_number()==1)

# make a subsetted version of 'df_gT', that only holds the columns
# 'class_order_latspc' and 'class_order_latspc' , make it distinct by only 'Class'
df_gTSc <- df_gT %>% dplyr::select(class_order_latspc, col.phycla) %>% 
  dplyr::group_by(class_order_latspc) %>%
  dplyr::arrange(class_order_latspc) %>%
  # only keep the first row of each group, to get a representative color
  dplyr::filter(row_number()==1) %>%
  # this  'df_gTCc'  data frame now holds a representative color for each class
  # # reorder the data frame by the Class name
  dplyr::arrange(class_order_latspc)


# make a ggplot with facetwrap per Phylum, using the 'col.phycla' column to
# determine the colors of the points
plt_g04 <- ggplot(data = df_gT, aes(
  x = longitude,
  y = latitude,
  color = Class,
  fill = class_order_latspc
), 
shape=21) +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               fill = "grey75",
               color = "gray45",
               linewidth = 0.1) +
  geom_point(size = 0.8, alpha = 0.4) +
  # coord_fixed(xlim = range(df_iNat01$longitude, na.rm = TRUE),
  #             ylim = range(df_iNat01$latitude, na.rm = TRUE)) +
  coord_fixed(xlim = c(set_swlng,
                       set_nelng) ,
              ylim = c(set_swlat,
                       set_nelat) ) +
  facet_wrap(~Phylum, ncol=3) +
  scale_color_manual(values = c(df_gTCc$col.phycla)) +
  scale_fill_manual(values = c(df_gTSc$col.phycla), guide="none") +
  labs(title = "iNaturalist records by Phylum",
       x = "Longitude",
       y = "Latitude") +
  theme_bw() +
  labs(col="Class") +
  guides(
    #fill = guide_legend(override.aes = list(shape = 21), ncol=2 ),
    #shape = guide_legend(override.aes = list(fill = "black"), ncol=2  ),
    col = guide_legend(override.aes = list(shape = 21), ncol=2 ) ) +
  theme(
    strip.background = element_rect(fill = "white", linetype = "solid",
                                    color = "white", linewidth = 1, size = 0.5),
    panel.spacing.x = unit(0, "pt"),
    strip.text.x = element_text(colour = "black", 
                                face = "bold",
                                hjust = 0.0) )

#plt_g04
# store the plot as a png file in the wdout directory
# define the output file name
outfl <- paste0(wd00_wdout,"/","Fig04_iNat_map_records_by_phylum.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = plt_g04, 
         # define the output filenmae by pasting together 
         filename = paste0(outfl),
         width=210,height=297,
         units="mm",dpi=300)
}

#:::
#:::

#:::
#:::

PhN<- unique(df_gT$Phylum)
# limit to only comprise the first 3 Phylum
#PhN<- PhN[1:3] 
# count the number of Phylum names
nPh <-length(PhN)
# make seq of numbers that can represent the Phylum names
PhNseq <- seq(1, nPh)
# iterate over this seq 
for (i in PhNseq){
  PhNm <- PhN[i]
  print(paste0("preparing plot ",i," for ",PhNm))
  # make a subset of the data frame 'df_gT' that only holds the rows
  # where the 'Phylum' column is equal to the current Phylum name
  df_gTPh <- df_gT %>% dplyr::filter(Phylum == PhNm)
  
  
  # make a ggplot with facetwrap per Phylum, using the 'col.phycla' column to
  # determine the colors of the points
  plt_g04 <- ggplot(data = df_gTPh, aes(
    x = longitude,
    y = latitude,
    color = Order,
    fill = class_order_latspc
  ), 
  shape=21) +
    geom_polygon(data = map_data("world"),
                 aes(x = long, y = lat, group = group),
                 fill = "grey75",
                 color = "gray45",
                 linewidth = 0.1) +
    geom_point(size = 0.8, alpha = 0.4) +
    # coord_fixed(xlim = range(df_iNat01$longitude, na.rm = TRUE),
    #             ylim = range(df_iNat01$latitude, na.rm = TRUE)) +
    coord_fixed(xlim = c(set_swlng,
                         set_nelng) ,
                ylim = c(set_swlat,
                         set_nelat) ) +
    facet_wrap(~Class, ncol=1) +
    scale_color_manual(values = c(df_gTOc$col.phycla)) +
    scale_fill_manual(values = c(df_gTSc$col.phycla), guide="none") +
    labs(title = "iNaturalist records by Class",
         x = "Longitude",
         y = "Latitude") +
    theme_bw() +
    labs(col="Order") +
    guides(
      #fill = guide_legend(override.aes = list(shape = 21), ncol=2 ),
      #shape = guide_legend(override.aes = list(fill = "black"), ncol=2  ),
      col = guide_legend(override.aes = list(shape = 21), ncol=1 ) ) +
    theme(
      strip.background = element_rect(fill = "white", linetype = "solid",
                                      color = "white", linewidth = 1, size = 0.5),
      panel.spacing.x = unit(0, "pt"),
      strip.text.x = element_text(colour = "black", 
                                  face = "bold",
                                  hjust = 0.0) )
  
  #plt_g04
  # pad the the running number 'i' with leading zeros
  # pad the count number with zeroes to have 5 digits
  pi <- sprintf("%02d", i)
  # store the plot as a png file in the wdout directory
  # define the output file name
  outfl <- paste0(wd00_wdout,"/","Fig05_",pi,"_iNat_map_records_by_class_for_",PhNm,".png")
  #set variable to define if figures are to be saved
  bSaveFigures<-T
  # save the figure if the above 'bSaveFigures' is TRUE
  if(bSaveFigures==T){
    ggsave(plot = plt_g04, 
           # define the output filenmae by pasting together 
           filename = paste0(outfl),
           width=210,height=297,
           units="mm",dpi=300)
  }
  # end iteration over Phylum names
}

