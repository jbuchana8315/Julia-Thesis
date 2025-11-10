rm(list = ls())
library(wru)
library(tidyverse)
library(reshape)
library(ggrepel)
library(tigris)
library(scales)
library(sf)
library(ggplot2)

##########################################
##    this code demonstrates how to link the
##    2022 voter file to voting precincts, and
##    then to merge to shapefile data. it also
##    shows how to generate a precinct map.
##
##    as an example, we build a map showing
##    2022 general election turnout 
##    by voting precinct. 
##########################################

##########################################
##             data loading             ##
##########################################

# set the working directory
setwd("E:/Users/Processed Data/")

# load the 2022 voter file and precincts
load("Voter Files/LA Files/la_voterfile_2022.rData") 
load("Voter Files/LA Files/Auxiliary Data/la_precincts_2022.rData")

# reset the working directory
setwd(paste0("C:/Users/erosenman/Documents/GitHub/LA_City_Council_Redistricting/R Team Code/",
             "Rosenman Code/Processed Data/"))

# read in the precincts
precinctMap <- sf::st_read(paste0("C:/Users/erosenman/Documents/GitHub/",
                                "LA_City_Council_Redistricting/redistricting_team_code/data_prep/",
                                "processed_data/precincts_2022g_repaired_shp/precincts_2022g_repaired_shp.shp"))

################################################
##                turnout map                 ##
################################################

# link the voter file to the precincts
laData <- left_join(laData, la_precincts_2022, by = c('LALVOTERID'))

# compute turnout by precinct
precinctTurnout <- laData %>%
  group_by(Agg_Precinct) %>%
  summarise(
    count = n(),
    generalTurnout = mean(!is.na(General_2022_11_08)),
    primaryTurnout = mean(!is.na(Primary_2022_06_07))
  ) %>%
  filter(count > 100)

# join turnout data to precinct geometry
precinctMap <- precinctTurnout %>%
  left_join(precinctMap, by = c("Agg_Precinct" = "Code")) %>%
  st_as_sf()

# plot general turnout by precinct
ggplot(precinctMap) +
  geom_sf(aes(fill = generalTurnout),
          color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "inferno",
    labels = percent_format(accuracy = 1),
    name = "2022 General\nturnout"
  ) +
  labs(
    title = "Turnout in the 2022 General Election by Precinct",
    subtitle = "City of Los Angeles, CA",
    caption = "Source: LA County voter file"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
    