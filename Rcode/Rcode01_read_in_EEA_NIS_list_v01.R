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

# delete a directory -- must add recursive = TRUE
unlink(wd00_wd01, recursive = TRUE)
#create anew directory
dir.create(wd00_wd01)

# set working directory to be able to read in functions
wdSRC <- (paste0(wd00,"/Rcode"))
# read in the functions to query the worms database
source(paste0(wdSRC,"/","Rfunctions_for_query_worms_database_v01.R"))

# define the data input directory
wdd <- paste0(wd00,"/data")
lstfd <- list.files(wdd)
# get the file name that contains the word "non-indigenous"
lstfd <- lstfd[grepl("non-indigenous", lstfd)]
# read in the excel file with the NIS
dfex <- readxl::read_xlsx(paste0(wdd,"/",lstfd),
                          sheet = 1)
# get the column names
cn <- colnames(dfex)
# replace all spaces with underscores in the 'cn'
cn <- gsub(" ", "_", cn)
cn <- gsub("-", "_", cn)
# add back the modified column names to the data frame, 
# to replace the old column names
colnames(dfex) <- cn
# get the column names that contains the word "ScientificName"                                                                                                                                                                                                                                      
# get the unique taxa names in the column 'ScientificName_accepted'
uscNm <- unique(dfex$ScientificName_accepted)
uscNM <-as.character(dfex$Species_name_original)
# retain only unique names
uscNM <- unique(uscNM)
# convert text string to ASCII and translate the odd 
# characters - some spaces are not spaces
# https://character-encoding-decoding.ssojet.com/ascii-in-r/
uscNM <- iconv(uscNM, from = "UTF-8", to = "ASCII//TRANSLIT")
# split the string ,  splitting 
# use gsub on 'Species_name_original' 
# and remove everything after the third space
uscNM <-gsub("^([^ ]* [^ ]* [^ ]*) .*$", "\\1",
             uscNM)
# remove everything that comes after 
# the second space and a parenthesis
uscNM <- gsub("^([^ ]* [^ ]*) \\(.*", "\\1",uscNM)
# remove everything that comes after the second space
# if the string contains a comma after the third word
uscNM <- gsub("^([^ ]* [^ ]*) [^ ]*,.*", "\\1",uscNM)
uscNM <- gsub("^([^ ]* \\([^ ]*\\) [^ ]*) [^ ].*", "\\1",uscNM)
# exclude ' cf. ' from the string and replace with space
# as it does not make sense to have 'cf.' in the name
# the 'cf.' is used to indicate that the name is only a
# suggestion and not a confirmed name
# see : https://www.practicalfishkeeping.co.uk/features/why-do-some-scientific-names-have-cf--in-them
# and to work on list as this , it does not make any sense
# to work on undescribed species
# i.e. - remove the 'cf.' from the string

uscNM <- gsub(" cf\\.", "", uscNM)
# substitute to modify  names where genera or species only have 
# affinity to species. It does not make much 
# sense to work on on a list of 
# nonindigenous species that are not described to species level
uscNM <- gsub(" aff\\.", "", uscNM)
# use grepl to get the names that do not have ' sp.' in the name
# it does not make much sense to work on genera for this list
# as other representatives of the genus may not be 
# considered nonindigenous species
uscNM <- uscNM[!grepl(" sp\\.", uscNM)]

# use gsub to remove ' subsp.' from the string, as it does
# not make any sense to work on unknown subspecies
uscNM <- gsub(" subsp\\.", "", uscNM)
# some species have been added a '-' in the species name
# this is not possible to search in the WoRMS database
# and is not a valid character in a Latin species name
# remove the '-' from the string using gsub
uscNM <- gsub("-", " ", uscNM)
# get all that does NOT have parenthesis in the string
npsNm<- uscNM[!grepl("\\(", uscNM)]
# remove everything that comes after the second space
npsNm <- gsub("^([^ ]* [^ ]*) .*", "\\1",npsNm)
# replace the names in the 'uscNM' list that do not have
# parenthesis 
uscNM[!grepl("\\(", uscNM)] <- npsNm
#
usplNm <- strsplit(uscNM, " ")
# use 'plyr::rbind.fill' to split
# and fill the data frame with NAs
#https://stackoverflow.com/questions/17308551/do-callrbind-list-for-uneven-number-of-column
library(plyr)
df_usNM <- plyr::rbind.fill(lapply(usplNm,
                  function(y){as.data.frame(t(y),
                  stringsAsFactors=FALSE)}))
# identify the names in column 2 that has parenthesis
puNmc2 <- df_usNM[,2][grepl("\\(",df_usNM[,2])]
# get the corresponding names in column 1
puNmc1 <- df_usNM[,1][grepl("\\(",df_usNM[,2])]
# remove all paranthesis from the names in column 2
puNmc2 <- gsub("\\(", "", puNmc2)
puNmc2 <- gsub("\\)", "", puNmc2)
# check if the names in puNmc1 are identical to the names in puNmc2
# if they are the same then there is no need to have an identical subgenus 
# name in the column 2
# copy the puNmc2 column to a new column that can be modifed
puNmc2m <- puNmc2
puNmc2m[(puNmc1==puNmc2)] <- ""
# add back parenthesis to the names in column 2
puNmc2m[(puNmc1!=puNmc2)] <- paste0("(",puNmc2m[(puNmc1!=puNmc2)],")")
# add back into the 'df_usNM[,2]' column
df_usNM[,2][grepl("\\(",df_usNM[,2])] <- puNmc2m
# paste all rows together in the 'df_usNM' data frame
df_usNM <- df_usNM %>%
  dplyr::mutate(spcNm = paste0(V1, " ", V2," ", V3)) %>%
  dplyr::select(spcNm) %>%
  # remove ' NA' from the 'spcNm'
  dplyr::mutate(spcNm = gsub(" NA", "", spcNm)) %>%
  # use gsub to replace double spcae with single space
  dplyr::mutate(spcNm = gsub("  ", " ", spcNm))
# only get the first column
usNM <- df_usNM[,1]
# exclude the row if the name contains a punctuation mark
usNM <- usNM[!grepl("\\.", usNM)]
# exclude the row if the name contains a 'sp' at
# the end of the string
usNM <- usNM[!grepl(" sp$", usNM)]

# exclude the row if it contains "Pseudo nitzschia"
# since this is a name that have originated from an error
# there is no genus called "Pseudo", it is a misspelling
# where a space has been introduced in the genus name 'Pseudonitzschia'
# and since the search in the WoRMS database will not be performed
# on only a genus name it is omitted from this list
usNM <- usNM[!grepl("Pseudo nitzschia", usNM)]
# write the data frame to a csv file, using ';'
# as the delimiter
write.table(usNM, paste0(wd00_wd01,"/","df_usNM.csv"),
          row.names = FALSE, sep = ";", fileEncoding = "UTF-8")
# omit from the 'uscNm' list if the string contains only a single word
usNM <- usNM[grepl(" ", usNM)]
# get the names that has parantheses included in the name
puNm <- usNM[grepl("\\(", usNM)]


# limit to only unique, as there is no point in looking up the
# same name twice
usNM <- unique(usNM)
# use the 'GetSpeciesIDFuzzy' function to get the species ID
# from the uscNm list
#make an empty list to add aphia records and species to
# and a list for not found species
lstap.f <- list()
lstap.notF <- list()
#iterate over elements in list of species'
pNm <- usNM[1:600]
#pNm <- usNM
for (i in 1:length(pNm)) {
  # get the species name
  spNm <- pNm[i]
  # use the 'GetSpeciesIDFuzzy' function to get the species Aphia ID number
  AIDNm <- GetSpeciesIDFuzzy(spNm)
  # check if there is an Aphia ID available
  cch <- grepl("[0-9]", AIDNm)
  # evaluate whether there is an AphiaID number
  if (cch==T){
  # check the status of the AphiaID number
  stAph <- GetAphiaParameter(AIDNm, "status")
  stat_ass <- stAph=="accepted"} else {
    # if no Aphia ID is available, then 'cch' is F
    # the next elements will be
    stAph <- "No content"  
    stat_ass <- F
  }
  #if the 'stat_ass' is T then obtain the wm_records_name
  if (stat_ass==T){
  # The 'worrms::wm_records_name' fails for 'Aerococcus viridans'
  # I abandoned using the   'worrms::wm_records_name' function
    # and instead decided on using the 'GetAphiaParameter'
    # function
  # wm_rec <- worrms::wm_records_name(name = spNm)
  # subset the dataframe to only comprise the accepted names
  # and then get the authority for the accepted name
  # wm_rec <-  wm_rec[(wm_rec$status=="accepted"),]
  # get the authority, the kingdom, phylum, class, order, 
    # family, genus, 
  # and the citation and evaluation of whether it 
    # is marine for the accepted name
    tryCatch(
      #get Aphia ID records
      { oVe <- try.c.GetAphiaParameter_df(AIDNm)  },
      error=function(e) {
        message('An Error Occurred')
        print(e)
        #return(NA)
        oVe <- rep(NA,12) },
      warning=function(w) {
        message('A Warning Occurred')
        print(w)
        oVe <- rep(NA,12)  }
    )
  # add as vector to the list
  lstap.f[[i]] <- oVe
  # end the if test with an else evaluation
  } else {
    lstap.notF[[i]] <- c(spNm,AIDNm, stAph)    
  }
  # end the iteration over species
}
# combine the list to a data frame
dfap.f <- do.call(rbind, lstap.f)
dfap.notF <- do.call(rbind, lstap.notF)
# convert the matrix to a data frame
dfap.f <- as.data.frame(dfap.f)
dfap.notF <- as.data.frame(dfap.notF)
#View(dfap.notF)
# rename the columns
colnames(dfap.f) <- c("spcNm", "Acc_authority","AphiaID",
                       "Kingdom","Phylum","Class",
                       "Order","Family","Genus",
                       "Citation","Status","isMarine")
colnames(dfap.notF) <- c("spcNm", "AphiaID",
                      "Status")
# use the row numbers to make a column with numbers
dfap.f <- dfap.f %>%
  dplyr::mutate(row = row_number()) %>%
  dplyr::select(row, everything()) %>%
  # make the columns 'row' and 'AphiaID' to be numeric
  dplyr::mutate(row = as.numeric(row),
         AphiaID = as.numeric(AphiaID))
# use the row numbers to make a column with numbers
dfap.notF <- dfap.notF %>%
  dplyr::mutate(row = row_number()) %>%
  dplyr::select(row, everything()) %>%
  # make the columns 'row' and 'AphiaID' to be numeric
  dplyr::mutate(row = as.numeric(row),
                AphiaID = as.numeric(AphiaID))

# get the species distribution from the AphiaID from the WoRMS database
# make an empty list to add species distributions
lstspcD <- list()
# iterate over the AphiaID numbers
for (i in 1:length(dfap.f$AphiaID)) {
  # get the AphiaID number
  AIDNm <- dfap.f$AphiaID[i]
  # use the 'GetSpeciesDistributions' function to get the species distribution
  lst.spcD <- try.c.GetSpeciesDistributions(AIDNm)
  # convert the matrix to a data frame
  dfspcD <- as.data.frame(lst.spcD)
  # add as vector to the list
  lstspcD[[i]] <- dfspcD
}


# use 'plyr::rbind.fill' to split
# and fill the data frame with NAs
#https://stackoverflow.com/questions/17308551/do-callrbind-list-for-uneven-number-of-column
library(plyr)
dfspcD <- plyr::rbind.fill(lapply(lstspcD,
                function(y){as.data.frame((y),
                stringsAsFactors=FALSE)}))
# convert the matrix to a data frame
dfspcD <- as.data.frame(dfspcD)
# From the 'dfap.f' data frame, match the AphiaID numbers,
# to add up the "Acc_authority","AphiaID","Citation","isMarine"
# to the 'dfspcD' data frame
# first select only the columns that are to be added
dfap.f <- dfap.f %>% dplyr::select(AphiaID, 
                            Acc_authority, 
                            Citation,
                            Status,
                            isMarine)
# colnames(dfap.f)
# colnames(dfspcD)
# then add by the AphiaID number
dfspcD <- dfspcD %>%
  dplyr::left_join(dfap.f, by = c("AphiaID" = "AphiaID")) 

# write out the table as csv file
write.table(dfspcD, paste0(wd00_wd01,"/","table01_all_geogr_reg.csv"),
            row.names = FALSE, sep = ";", fileEncoding = "UTF-8")

#View(dfspcD)