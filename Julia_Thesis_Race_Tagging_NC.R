#Julia Buchanan Thesis
#North Carolina voter race tagging assessment 

rm(list = ls())
library(wru)
library(tidyverse)

source("~/GitHub/Julia-Thesis/county_helpers.R")

##########################################
##    this code demonstrates how to race tag 
##    a *new* voter file using the wru package. 
##
##    race scores have already been pre-
##    computed for the 2020 North Carolina voter file 
##     and can be found in the folder
##    "Processed Data/Voter Files/Non-LA Files" 
##########################################

##########################################
##             data loading             ##
##########################################

# constant: switch to CVAP priors?
cvapPrior <- TRUE

# set the working directory
setwd("E:/Users/Processed Data/")

# load the 2020 NC voter file
load("Voter Files/Non-LA Files/nc_voterfile_2020_narrow.rData") 

# load the 2010 NC Census data
# use the get_census_data function in wru to save this file
# to the remote machine 
load("Census Data/ncCensus_2010.rData")
load("Census Data/ncCVAP_2010_tract.rData")

if(cvapPrior) {
  ncCensus_2010$NC$tract <- tract_priors_cvap
}

###########################################
##           race tagging code           ##
###########################################

# add state and county to the file
vf$state <- 'NC'

# rename columns to match wru names
names(vf)[names(vf) == "Residence_Addresses_CensusBlock"] <- 'block'
names(vf)[names(vf) == "Residence_Addresses_CensusTract"] <- 'tract'
names(vf)[names(vf) == "County"] <- 'county'
names(vf)[names(vf) == "Voters_LastName"] <- 'surname'
names(vf)[names(vf) == "Voters_FirstName"] <- 'first'
names(vf)[names(vf) == "Voters_MiddleName"] <- 'middle'

# map the counties to their appropriate code
vf$county <- nc_county_fips[vf$county]

# sanity check: any unmapped?
if (any(is.na(vf$county))) {
  warning("Some county names were not matched to FIPS codes.")
}

# run bisg at the tract level 
names(vf) <- gsub("c_", "c.", names(vf))
taggedVf <- predict_race(vf, names.to.use = 'surname, first, middle', skip_bad_geos = TRUE, 
                       census.geo = 'tract', census.data = ncCensus_2010, year = "2010")

##########################################
##           simple analytics           ##
##########################################

# what are the voter file racial distributions?
vfRacialDistributions <- taggedVf %>%
  summarise(white = mean(pred.whi, na.rm = TRUE), 
            black = mean(pred.bla, na.rm = TRUE),
            hispanic = mean(pred.his, na.rm = TRUE), 
            aapi = mean(pred.asi, na.rm = TRUE), 
            other = mean(pred.oth, na.rm = TRUE))

vfRacialDistributions
