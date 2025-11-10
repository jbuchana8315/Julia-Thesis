rm(list = ls())
library(wru)
library(tidyverse)
library(reshape)
library(ggrepel)
library(tigris)
library(scales)
library(sf)

source(paste0("~/GitHub/LA_City_Council_Redistricting/R Team Code/Rosenman Code/", 
              "Ecological Inference/Additional Helper Functions.R"))

##########################################
##    this code demonstrates how to link the
##    2022 voter file to *existing* EI scores,
##    which have already been computed for four
##    2022 general elections in LA (mayor, 
##    governor, senator, and prop 1). 
##
##    we also link to race predictions in
##    this script, which allows us to 
##    compute racial crosstabs
##########################################

##########################################
##             data loading             ##
##########################################

# set the working directory
setwd("E:/Users/Processed Data/")

# load the 2022 voter file 
load("Voter Files/LA Files/la_voterfile_2022.rData") 

# load the race scores
load("Voter Files/LA Files/Auxiliary Data/la_race_preds_2022.rData")

# load the EI scores
eiScores <- read_csv("Voter Files/LA Files/Auxiliary Data/la_eiScores_2022g.csv")

################################################
##             merge all the data             ##
################################################

# merge the race predictions
laData <- left_join(laData, la_racePreds_2022, by = 'LALVOTERID')

# merge the ei scores
laData <- left_join(laData, eiScores, by = 'LALVOTERID')

#######################################################
##            compute the race crosstabs            ##
#######################################################

# get the crosstabs
race_crosstabs(laData[!is.na(laData$newsomEIScore),], 
               list('newsomEIScore', 'padillaEIScore',
                             'prop1EIScore', 'bassEiScore'))
