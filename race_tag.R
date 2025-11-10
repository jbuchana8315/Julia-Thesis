rm(list = ls())
library(wru)
library(tidyverse)

##########################################
##    this code demonstrates how to race tag 
##    a *new* voter file using the wru package. 
##
##    race scores have already been pre-
##    computed for the 2022 LA voter file 
##     and can be found in the folder
##    "Processed Data/Voter Files/LA Files/Auxiliary Data" 
##########################################

##########################################
##             data loading             ##
##########################################

# set the working directory
setwd("E:/Users/Processed Data/")

# load the 2022 voter file
load("Voter Files/LA Files/la_voterfile_2022.rData") 

# load the 2020 Census data
# use the get_census_data function in wru to save this file
# to the remote machine 
load("Census Data/caCensusData_2020.rdata")

###########################################
##           race tagging code           ##
###########################################

# add state and county to the file
laData$state <- 'CA'

# rename columns to match wru names
names(laData)[names(laData) == "Residence_Addresses_CensusBlock"] <- 'block'
names(laData)[names(laData) == "Residence_Addresses_CensusTract"] <- 'tract'
names(laData)[names(laData) == "Voters_LastName"] <- 'surname'
names(laData)[names(laData) == "Voters_FirstName"] <- 'first'
names(laData)[names(laData) == "Voters_MiddleName"] <- 'middle'

# column name fix
names(laData) <- gsub('c_', 'c.', names(laData))

# run bisg at the tract level 
laData <- predict_race(laData, names.to.use = 'surname, first, middle', skip_bad_geos = TRUE, model = 'BISG',
                       census.geo = 'block', census.data = caData_2020, year = "2020")

##########################################
##           simple analytics           ##
##########################################

# what are the voter file racial distributions?
vfRacialDistributions <- laData %>%
  summarise(white = mean(pred.whi, na.rm = TRUE), 
            black = mean(pred.bla, na.rm = TRUE),
            hispanic = mean(pred.his, na.rm = TRUE), 
            aapi = mean(pred.asi, na.rm = TRUE), 
            other = mean(pred.oth, na.rm = TRUE))

# what are the 2022 general electorate racial distributions?
g2022RacialDistributions <- laData %>%
  filter(!is.na(General_2022_11_08)) %>%
  summarise(white = mean(pred.whi, na.rm = TRUE), 
            black = mean(pred.bla, na.rm = TRUE),
            hispanic = mean(pred.his, na.rm = TRUE), 
            aapi = mean(pred.asi, na.rm = TRUE), 
            other = mean(pred.oth, na.rm = TRUE))

# what are the 2022 primary electorate racial distributions?
p2022RacialDistributions <- laData %>%
  filter(!is.na(Primary_2022_06_07)) %>%
  summarise(white = mean(pred.whi, na.rm = TRUE), 
            black = mean(pred.bla, na.rm = TRUE),
            hispanic = mean(pred.his, na.rm = TRUE), 
            aapi = mean(pred.asi, na.rm = TRUE), 
            other = mean(pred.oth, na.rm = TRUE))