#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#
library(dplyr)
library(plyr)
library(ggplot2)
library(litsearchr)
#Use the libraries
library(stats)
library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
library(httr)
# if(!require(tidyverse)){
#   install.packages("tidyverse")
# }  
library(tidyverse)
#remotes::install_github("ropensci/worrms")
library("worrms")
#get worms package
# if(!require(worrms)){
#   install.packages("worrms")
# }  
library(worrms)

#install.packages("stringr") # Install stringr package
library("stringr")          # Load stringr package

# get the working directory
wd00 <- getwd()
# define the output directory
wd01 <- "output01_EEA_NIS_list"
# make path for out put directory
wd00_wd01 <- paste(wd00,
                   wd01,
                   sep="/")

# # delete a directory -- must add recursive = TRUE
# unlink(wd00_wd01, recursive = TRUE)
# #create anew directory
# dir.create(wd00_wd01)

# set working directory to be able to read in functions
wdSRC <- (paste0(wd00,"/Rcode"))
# read in the functions to query the worms database
source(paste0(wdSRC,"/","Rfunctions_for_query_worms_database_v01.R"))

# define the data input directory
wdd <- paste0(wd00,"/data")

# read in the data table prepared in Rcode01
dfspcD <- read.table(paste0(file=wd00_wd01,"/","table01_all_geogr_reg.csv"),
            header=T,sep = ";", fileEncoding = "UTF-8")

#View(dfspcD)
# get all 'higherGeography' names
All.HiGeoReg <- unique(dfspcD$higherGeography)


# make a vector with European higher geography
EUR.HiGeoReg <- c(
  "Adriatic Sea",
  "Aegean Sea",
  "Albania",
  "Alboran Sea",
  "Austria",
  "Baelt Sea",
  "Balearic Sea",
  "Baltic Sea",
  "Bay of Biscay",
  "Belarus",
  "Belgium",
  "Black Sea",
  "Bristol Channel",
  "Bulgaria",
  "Cape Verde",
  "Caribbean Sea",
  "Celtic Sea",
  "Clyde Sea Area",
  "Croatia",
  "Cyprus",
  "Czech Republic",
  "Denmark",
  "Egypt",
  "English Channel",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Grand Duchy of Luxembourg",
  "Greece",
  "Gulf of Bothnia",
  "Gulf of Finland",
  "Gulf of Riga",
  "Gulf of Suez",
  "Hungary",
  "Inner Seas off the West Coast of Scotland",
  "Ionian Sea",
  "Ireland",
  "Irish Sea and St. George's Channel",
  "Israel",
  "Italy",
  "Kattegat",
  "Laptev Sea",
  "Lebanon",
  "Levantine Sea",
  "Libya",
  "Ligurian Sea",
  "Lithuania",
  "Malta",
  "Mauritania",
  "Mediterranean Sea - Eastern Basin",
  "Mediterranean Sea - Western Basin",
  "Mediterranean Sea",
  "Monaco",
  "Morocco",
  "Netherlands",
  "North Atlantic Ocean",
  "North Sea",
  "Norway",
  "Norwegian Sea",
  "Palestine",
  "Poland",
  "Portugal",
  "Red Sea",
  "Sea of Azov",
  "Sea of Marmara",
  "Serbia",
  "Skagerrak",
  "Spain",
  "Strait of Gibraltar",
  "Sweden",
  "Switzerland",
  "Tunisia",
  "Tyrrhenian Sea",
  "Türkiye",
  "Ukraine",
  "United Kingdom",
  "Wadden Sea",
  "White Sea",
  "Vainameri",
  "Inland Sea",
  "Bosnia and Herzegovina",
  "Central Mediterranean",
  "Kazakhstan",
  "Algeria",
  NA)
# get the difference between the two vectors
# get the names that are in the 'All.HiGeoReg' vector
# but not in the 'EUR.HiGeoReg' vector
Miss.HiGeo <- All.HiGeoReg[which(!All.HiGeoReg %in% EUR.HiGeoReg)]


# subset the 'dfspcD' data frame to only comprise rows where
# the 'higherGeography' column matches the 'EUR.HiGeoReg' vector
dfspcD <- dfspcD %>%
  dplyr::filter(higherGeography %in% EUR.HiGeoReg)
nrow(dfspcD)
length(unique(dfspcD$locality))
local.u <- unique(dfspcD$locality)
local.u <- local.u[order(local.u)]
# make a list of localities to exclude
excl.loc <- c(              "Africa",
                            "Alaska",
                            "Alberta",
                            "America",
                            "Andaman",
                            "Antarctica",
                            "Arab",
                            "Argentina",
                            "Aruba",
                            "Austral",
                            "australia",
                            "Bahamas",
                            "Belize",
                            "Bermud",
                            "Bora",
                            "Brazil",
                            "burma",
                            "California",
                            "cambodia",
                            "canad",
                            "Canada",
                            "Cape Howe",
                            "Caribbean",
                            "Carolina",
                            "cayman",
                            "Ceylon",
                            "chile",
                            "china",
                            "Chinese",
                            "christmas",
                            "cocos",
                            "Colombia",
                            "Comoros",
                            "Costa Rica",
                            "Croix",
                            "cuba",
                            "Curaçao",
                            "Dahomey",
                            "Dakhla",
                            "Dardanelles",
                            "Djibouti",
                            "Dominica",
                            "Easter Island",
                            "Ecuador",
                            "Falkland",
                            "fiji",
                            "Florida",
                            "French Guiana",
                            "Gambia",
                            "Genoa Gulf",
                            "Georgia",
                            "Greenland",
                            "Guam",
                            "Guatemala",
                            "Guyana",
                            "Hawaii",
                            "Hokkaido",
                            "Hong Kong",
                            "Hudson",
                            "Iceland",
                            "India",
                            "indian",
                            "Indies",
                            "Indo",
                            "indonesia",
                            "Jamaica",
                            "japan",
                            "Java",
                            "kenya",
                            "Korea",
                            "Korean",
                            "Kuroshio",
                            "Labrador",
                            "laos",
                            "Lawrence",
                            "Madagascar",
                            "Maine",
                            "malaysia",
                            "maldives",
                            "Mariana",
                            "marquesas",
                            "Marshall",
                            "Martinican",
                            "Mauritius",
                            "Mayotte",
                            "Mexic",
                            "Micronesia",
                            "Mozambique",
                            "Manitoba",
                            "Murmansk",
                            "myanmar",
                            "Namibia",
                            "Nantucket",
                            "new caledonia",
                            "New Jersey",
                            "New South Wales",
                            "New York",
                            "new zealand",
                            "Newfoundland",
                            "Nha-Trang",
                            "Nicobar",
                            "Ningaloo",
                            "North Carolina",
                            "Nova Scotia",
                            "Ohio",
                            "okhotsk",
                            "Okinawa",
                            "Oman",
                            "Ontario",
                            "Oregon",
                            "Panama",
                            "Papua",
                            "pacific",
                            "Paraguay",
                            "Patagonia",
                            "Persian",
                            "Peru",
                            "philippines",
                            "Polynes",
                            "Puerto Ric",
                            "Puget Sound",
                            "Reunion",
                            "Rhode",
                            "Rio Grande",
                            "Russia",
                            "Ryukyu Islands",
                            "Sahara",
                            "samoa",
                            "San Francisco",
                            "Saskatchewan",
                            "Senegal",
                            "Seychelles",
                            "Seychellois",
                            "Singapore",
                            "solomon",
                            "South Africa",
                            "sri lanka",
                            "Sumatra",
                            "sunda",
                            "Suriname",
                            "Svalbard",
                            "Taghazout",
                            "Taiwan",
                            "Tanzania",
                            "Taranto Gulf",
                            "Tasman",
                            "Texas",
                            "thailand",
                            "Tibet",
                            "tonga",
                            "Trindade",
                            "Tropical",
                            "Tuamotus",
                            "United States",
                            "Uruguay",
                            "USA",
                            "vanuatu",
                            "Venezuela",
                            "vietnam",
                            "Virgin",
                            "West Atlantic",
                            "West Central Atlantic",
                            "West North Atlantic",
                            "Woods Hole",
                            "Yellow Sea",
                            "Ymuiden Harbour",
                            "Zanzibar",
                            #_______
                            "Agulhas",
                            "Ambon",
                            "Angolan Exclusive Economic Zone",
                              "Anse de Belliveau",
                            "Aral Sea",
                            "Arctic",
                            "Asilah",
                            "Aves I.",
                            "Bass Strait",
                            "Bath",
                            "Beaufort Sea",
                            "Belarus",
                            "Bonaire",
                            "Bonaire Exclusive Economic Zone",
                            "Bou Regreg",
                            "Branco",
                            "British Columbia",
                            "Broome",
                            "Buton",
                            "Cape Blanc",
                            "Carrie Bow Cay",
                            "Cayenne",
                            "Courtney Campbell Causeway",
                            "Crozet Islands Exclusive Economic Zone",
                            "Essaouira",
                            "Faeroe Exclusive Economic Zone",
                            "Faeroe part of the North Atlantic Ocean",
                            "FAO fishing area 67",
                            "Faroe Plateau",
                            "Flemish Banks",
                            "Fort Macon",
                            "Gilbert Islands",
                            "Great Lakes",
                            "Guadeloupean Exclusive Economic Zone",
                            "Guayaquil",
                            "Hakodate Bay",
                            "Harima-nada",
                            "Henlopen, Cape",
                            "Hispaniola",
                            "Inhaca Island",
                            "Iraq",
                            "Jiangsu",
                            "Kamaran Island",
                            "Katse Hoek",
                            "Kazakh Exclusive Economic Zone",
                            "Ksar-Sghir",
                            "Lesser Antilles",
                            "Madagascan Exclusive Economic Zone",
                            "Majona",
                            "Malay Archipelago",
                            "Manning-Hawkesbury",
                            "Massachusetts",
                            "Maui Island",
                            "Midway Atoll",
                            "Midway Is.",
                            "Mirhleft",
                            "Moloasses Reef",
                            "Montserrat Exclusive Economic Zone",
                            "Moorea",
                            "Nador",
                            "Nador Lagoon",
                            "New Brunswick",
                            "Northeastern Honshu",
                            "Northwest Territories",
                            "Nouackchott",
                            "Orikasa",
                            "OSPAR Region I (Arctic)",
                            "Palaearctic",
                            "Pandanon",
                            "Plage Guy ville",
                            "Port Alfred",
                            "Port Jackson",
                            "Port Phillip Bay",
                            "Port Royal",
                            "Quirimba Archipelago",
                            "Sahul Shelf",
                            "Saint John",
                            "Saint Martin",
                            "Saint Thomas",
                            "Saint-Barthélemy Exclusive Economic Zone",
                            "São Paulo",
                            "Sebou Estuary",
                            "Sierra Leonian part of the North Atlantic Ocean",
                            "Simon's Town Harbour",
                            "Society Islands",
                            "Sonora",
                            "Statia",
                            "Tan-tan",
                            "Togolese Exclusive Economic Zone",
                            "Torres Strait",
                            "Ubay",
                            "Venice lagoon",
                            "Western Bassian",
                            #____
                            "Bahamian",
                            "Bat Yam",
                            "Bombay",
                            "British Overseas Territories",
                            "Cape of Good Hope",
                            "Chesapeake Bay",
                            "Connecticut",
                            "Goa",
                            "Circum",
                            "Nagasaki",
                            "New Britain",
                            "Ogasawara Islands",
                            "Oklahoma",
                            "Otago region",
                            "Philippine Exclusive Economic Zone",
                            "Queensland",
                            "Rio De Janeiro",
                            "St. Helena",
                            "Sudan"
)
# paste together in to one vector
excl.loc.ts <- paste0(excl.loc, collapse = "|")
# get location ID
exlocat.ID <- dfspcD$locationID[grepl(excl.loc.ts,dfspcD$locality, ignore.case = TRUE)]
# use gsub to remove everything before the last '/'
exlocat.IDno <- gsub(".*\\/", "", exlocat.ID)
# get the unique location ID numbers
exlocat.IDno <- unique(exlocat.IDno)
# order the location ID numbers
exlocat.IDno <- exlocat.IDno[order(exlocat.IDno)]
# make a vector with the location ID numbers, collapse with '|'
exlocat.IDno.ts <- paste0(exlocat.IDno, collapse = "|")
# use grepl to exclude if the location ID number is in the
# 'locationID' column
dfspcD <- dfspcD[!grepl(exlocat.IDno.ts,dfspcD$locationID, ignore.case = TRUE),]
# subset to exclude
dfspcD <- dfspcD[!grepl(excl.loc.ts,dfspcD$locality, ignore.case = TRUE),]
# get only unique 'locality'
local.u <- unique(dfspcD$locality)
local.u <- local.u[order(local.u)]
# read in csv -table with higher geography for 'locality'
det_lc <- read_delim(paste0(file=wdd,"/","locality_details.csv"),
                     col_names = T,
                     delim = "\t")

  # write out the table as csv file
write.table(local.u, paste0(wd00_wd01,"/","table02_locality.csv"),
            row.names = FALSE, sep = ";", fileEncoding = "UTF-8")
#View(dfspcD)
nrow(dfspcD)
# use 'left_join' to add the 'Higher_GEO' column to the 'dfspcD' data frame
# using the 'locality' column as the key
dfspcD2 <- dfspcD %>%
  left_join(det_lc, by = c("locality" = "locality"))
# get the unique 'Higher_GEO' names from the 'det_lc' data frame
HiGeoReg.u <- unique(dfspcD2$Higher_GEO)
# order the unique higher geography names
HiGeoReg.u <- HiGeoReg.u[order(HiGeoReg.u)]
# get the number of unique higher geography names
length(HiGeoReg.u)
dput(HiGeoReg.u)
# make a vector with European higher geography
EUHG <- c("Algeria",
  "Atlantic Europe",
  "Atlantic Ocean",
  "Azores",
  "Baltic Sea",
  "Estonia",
  "Baltic Sea",
  "Latvia",
  "Baltic Sea",
  "Poland",
  "Belgium",
  "Black Sea",
  "Canary Islands",
  "Gran Canaria",
  "Cape Verde",
  "Caspian Sea",
  "Celtic Sea",
  "Central Europe",
  "Denmark",
  "East Atlantic",
  "Egypt",
  "England",
  "Europe",
  "Finland",
  "France",
  "Germany",
  "Global",
  "Ireland",
  "Italy",
  "Luxembourg",
  "Madeira",
  "Mediterranean",
  "Morocco",
  "NE Atlantic",
  "Netherlands",
  "North Atlantic",
  "Canary Islands",
  "North East Atlantic",
  "North Sea",
  "Norway",
  "Poland",
  "Portugal",
  "E Europe",
  "S Europe",
  "Spain",
  "Sweden",
  "Turkey",
  "Ukraine",
  "Western Sahara,
   Cape Verde")
# use this vector to subset the 'dfspcD2' data frame
dfspcD2 <- dfspcD2 %>%
  dplyr::filter(Higher_GEO %in% EUHG)

# write out the table as csv file
write.table(dfspcD2, paste0(wd00_wd01,"/","table03_EUR_geogr_reg.csv"),
            row.names = FALSE, sep = ";", fileEncoding = "UTF-8")

# get the unique 'ScientificName' names
spc.u <- unique(dfspcD2$ScientificName)
# order the unique species names
spc.u <- spc.u[order(spc.u)]

# count the number of unique species
length(spc.u)
# convert the list of species names to a data frame
dfspc.u <- as.data.frame(spc.u)
# write out the table as csv file
write.table(dfspc.u, paste0(wd00_wd01,"/","table04_EUR_NIS_spc.csv"),
            row.names = FALSE, sep = ";", fileEncoding = "UTF-8")

#View(dfspcD2)
