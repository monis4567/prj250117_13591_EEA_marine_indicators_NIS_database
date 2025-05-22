#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# Authors: Steen Wilhelm Knudsen, 
#remove everything in the working environment, without a warning!!
#rm(list=ls())
#https://recology.info/2012/10/rgbif-newfxns/
#https://ropensci.github.io/CoordinateCleaner/articles/Cleaning_GBIF_data_with_CoordinateCleaner.html
library(dismo)
library(rgbif) 
# define the working directory
wd00 <- getwd()
setwd (wd00)
wd_ext01 <- "/home/hal9000/Documents/Dokumenter/NIVA_Ansaettelse_2025"
#define an output directory
wdout <- "/output04_records_from_GBIF"
#paste together path
wd00_wdout <- paste(wd00,wdout,sep="")
# delete a directory -- must add recursive = TRUE
unlink(wd00_wdout, recursive = TRUE)
#create anew directory
dir.create(wd00_wdout)
#define an input directory
wd01 <- "/output01_EEA_NIS_list"
#paste together path
wd00_wd01 <- paste(wd00,wd01,sep="")
#paste path and file together
wd00_wd01_inpf01 <- paste(wd00_wd01,"/table03_EUR_geogr_reg.csv",sep="")
#read csv with species names
dkl <-as.data.frame(read.csv(wd00_wd01_inpf01,
                             header = TRUE, sep = ";", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "", 
                             stringsAsFactors = FALSE))
# replace space with underscore in the Latin species names
dkl$ScientificNamewU <- gsub(" ","_",dkl$ScientificName)
# define extent of area to get gbif records from
#try defining your own bounding box
set_nelat= 70
set_nelng= 50
set_swlat= 10
set_swlng= -30
exta <- extent(c(5, 17, 52, 60))
exta <- extent(c(set_swlng, 
                 set_nelng, 
                 set_swlat, 
                 set_nelat))
#define columns to keep
kee <- c(#"acceptedScientificName",          
  "datasetName",
  "lat",                            
  "lon",
  "scientificName",
  "species") #,
# define columns with citation information
ctk2 <- c("datasetKey","gbifID")
# add to vector with column names to keep
kee<- c(kee,ctk2)
# get the unique Latin Species names
USciNm <- unique(dkl$ScientificNamewU)
# order the species names
USciNm <- USciNm[order(USciNm)]
# count the number of elements in the vector
length(USciNm)
#define list of species
lsp <- USciNm
lsp <- USciNm[1:13]
# try using only the 2nd element in the vector for doing the iteration
#lsp <- lsp[2]
#define empty list
lstg <- list()
# set a running number
i <- 1
#iterate over elements in list
for (e in lsp)
{
  #print(e)
  #substitute in string to get genus and species name
  genus <- sub('\\_.*', '',e)
  species <- sub('.*\\_', '',e)
  #get gbif records
  g <- gbif(genus, species, geo=TRUE, ext = exta, end=3000)
  #View(g)
  #colnames(g)
  if (!"datasetName" %in% colnames(g))
  {g$datasetName <- NA}
  g <- g[kee]
  lstg[[i]] <- g
  i <- i+1
}
#bind the rows in each list in to one data frame
df_g03 <- data.table::rbindlist(lstg, fill=T)
#View(df_g03)
dtsKey <- unique(df_g03$datasetKey)
dtsGBID <- unique(df_g03$gbifID)

#define column names to keep
#subset data frame
df_g03 <- as.data.frame(df_g03)
# subset the data frame to exclude columns
df_g03 <- df_g03[kee]
#define output flie name
outfl1 = "out08_06b_gbif_records_amphibia_Denmark.csv"
# paste together path and input flie
pthoutf01 <- paste0(wd00_wd01,"/",outfl1)
# use tab as separator
# write.table(df_g03, file=pthoutf01, sep=";",
#             row.names = F, quote = F) # do not use row names
# get column with heysets
datasetKey.g03 <- df_g03$datasetKey
# USe external input file with logon details
# path to file with personal logon
lgl <- "/home/hal9000/Documents/Documents/Dokumenter SWK/logon_gbif.txt"
# read the file as a text file
lgl <- readr::read_delim(lgl, delim = "\t")
# make it a data frame
lgl <- as.data.frame(lgl)
# only grep the row that has username and password
Username_gbif <- lgl[grepl("Username",lgl[,1]),]
Password_gbif <- lgl[grepl("Password",lgl[,1]),]
# substitute in these rows
Username_gbif <- gsub("Username: ","",Username_gbif)
Password_gbif <- gsub("Password: ","",Password_gbif)
# define path for github csv file
gthb_pth_for_csv <- "https://github.com/monis4567/amphibia_eDNA_in_Denmark/blob/main/supma03_inp_files_for_R/out08_06b_gbif_records_amphibia_Denmark.csv"
# use dplyr to count
library(dplyr)
# count up records
dd_meta <- df_g03 %>% dplyr::count(datasetKey) 
# use rgbif to get DOI records
my_dd<-rgbif::derived_dataset(
  citation_data<-dd_meta,
  title<-"Data from gbif for : Detection of environmental DNA from amphibians in Northern Europe applied in citizen science",
  description = "A csv file with occurence data for amphibians in Denmark used in the study:  Knudsen, S. W., Hesselsøe, M., Rytter, M., Lillemark, M. R., Tøttrup, A. P., Rahbek, C., Sheard, J. K., Thomsen, P. F., Agersnap, S., Mortensen, P. B., & Møller, P. R. (2023). Detection of environmental DNA from amphibians in Northern Europe applied in citizen science. Environmental DNA, 00, 1–20. https://doi.org/10.1002/edn3.462 ",
  source_url = gthb_pth_for_csv,
  user=Username_gbif,
  pwd=Password_gbif
)
# 
my_dd
#Herefter kan du finde din DOI i my_dd$doi
