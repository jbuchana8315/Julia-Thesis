#Julia Buchanan Thesis
#North Carolina voter race tagging assessment 

rm(list = ls())
library(wru)
library(tidyverse)
library(pROC)
library(dplyr)
library(ggplot2)
library(scales)

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
taggedvf <- predict_race(vf, names.to.use = 'surname, first, middle', skip_bad_geos = TRUE, 
                         census.geo = 'tract', census.data = ncCensus_2010, year = "2010")

##########################################
##           simple analytics           ##
##########################################

# what are the voter file racial distributions?
vfRacialDistributions <- taggedvf %>%
  summarise(white = mean(pred.whi, na.rm = TRUE), 
            black = mean(pred.bla, na.rm = TRUE),
            hispanic = mean(pred.his, na.rm = TRUE), 
            aapi = mean(pred.asi, na.rm = TRUE), 
            other = mean(pred.oth, na.rm = TRUE))

vfRacialDistributions


#predictive accuracy against MAP predictions
taggedvf <- taggedvf %>%
  mutate(
    ethnic_desc = toupper(CountyEthnic.Description),     
    
    # true race in 5 BISG categories
    true_race_5 = case_when(
      str_detect(ethnic_desc, "HISPANIC") ~ "HISPANIC",
      
      str_detect(ethnic_desc, "WHITE") ~ "WHITE",
      
      str_detect(ethnic_desc, "BLACK") |
        str_detect(ethnic_desc, "AFRICAN") ~ "BLACK",
      
      str_detect(ethnic_desc, "ASIAN") |
        str_detect(ethnic_desc, "PACIFIC") |
        str_detect(ethnic_desc, "NATIVE HAWAIIAN") ~ "ASIAN",
      
      TRUE ~ "OTHER"
    ),
    
    # MAP predicted race (highest posterior)
    MAP_pred = case_when(
      pred.whi == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "WHITE",
      pred.bla == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "BLACK",
      pred.his == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "HISPANIC",
      pred.asi == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "ASIAN",
      pred.oth == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "OTHER",
      TRUE ~ NA_character_
    )
  )

# overall accuracy
accuracy <- mean(taggedvf$MAP_pred == taggedvf$true_race_5, na.rm = TRUE)
print(accuracy)

# ROC AUC
# truth variable
taggedvf <- taggedvf %>%
  mutate(
    true_white    = as.integer(true_race_5 == "WHITE"),
    true_black    = as.integer(true_race_5 == "BLACK"),
    true_hispanic = as.integer(true_race_5 == "HISPANIC"),
    true_asian    = as.integer(true_race_5 == "ASIAN"),
    true_other    = as.integer(true_race_5 == "OTHER")
  )

#compute ROC
roc_white <- roc(taggedvf$true_white, taggedvf$pred.whi, quiet = TRUE)
roc_black <- roc(taggedvf$true_black, taggedvf$pred.bla, quiet = TRUE)
roc_hisp  <- roc(taggedvf$true_hispanic, taggedvf$pred.his, quiet = TRUE)
roc_asian <- roc(taggedvf$true_asian, taggedvf$pred.asi, quiet = TRUE)
roc_other <- roc(taggedvf$true_other, taggedvf$pred.oth, quiet = TRUE)

#find AUC
auc_values <- tibble(
  race = c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER"),
  auc  = c(
    auc(roc_white),
    auc(roc_black),
    auc(roc_hisp),
    auc(roc_asian),
    auc(roc_other)
  )
)

print(auc_values)

# plot all 5 curves
plot(roc_white, col = "blue",  lwd = 2, main = "ROC Curves for NC BISG Race Prediction")
plot(roc_black, col = "black", lwd = 2, add = TRUE)
plot(roc_hisp,  col = "red",   lwd = 2, add = TRUE)
plot(roc_asian, col = "green", lwd = 2, add = TRUE)
plot(roc_other, col = "purple",lwd = 2, add = TRUE)

legend(
  "bottomright",
  legend = paste0(auc_values$race, " (AUC = ", round(auc_values$auc, 3), ")"),
  col    = c("blue", "black", "red", "green", "purple"),
  lwd    = 2
)

#calibration
#calibration table for each race
calibration_table <- function(df, prob_col, race_label) {
  df %>%
    transmute(
      p = .data[[prob_col]],
      y = as.integer(true_race_5 == race_label)
    ) %>%
    filter(!is.na(p), !is.na(y)) %>%
    mutate(
      bin = cut(
        p,
        breaks = seq(0, 1, by = 0.1),
        include.lowest = TRUE,
        right = TRUE
      )
    ) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      pred_mean = mean(p),
      obs_rate  = mean(y),
      .groups = "drop"
    ) %>%
    filter(n > 0)
}

#calibration plotting
plot_calibration <- function(cal_df, race_name) {
  ggplot(cal_df, aes(x = pred_mean, y = obs_rate)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(size = n), color = "steelblue", alpha = 0.8) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste("Calibration Plot -", race_name),
      x = "Mean predicted probability",
      y = "Observed frequency",
      size = "Bin size"
    ) +
    theme_minimal()
}

#5 individual race plots
# WHITE
cal_white <- calibration_table(taggedvf, "pred.whi", "WHITE")
p_white <- plot_calibration(cal_white, "White")
print(p_white)

# BLACK
cal_black <- calibration_table(taggedvf, "pred.bla", "BLACK")
p_black <- plot_calibration(cal_black, "Black")
print(p_black)

# HISPANIC
cal_hisp <- calibration_table(taggedvf, "pred.his", "HISPANIC")
p_hisp <- plot_calibration(cal_hisp, "Hispanic")
print(p_hisp)

# ASIAN
cal_asian <- calibration_table(taggedvf, "pred.asi", "ASIAN")
p_asian <- plot_calibration(cal_asian, "Asian")
print(p_asian)

# OTHER
cal_other <- calibration_table(taggedvf, "pred.oth", "OTHER")
p_other <- plot_calibration(cal_other, "Other")
print(p_other)
