################################################################################
# Function: Modify data for analyses
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
# Purpose: Population comparison analyses for Sims and Reader 2020
################################################################################

# Housekeeping 

library(tidyverse)

data_analysis <- 
  read_csv("data/data_Sims-Reader_2020.csv") %>%
  mutate(across(where(is.character), ~as_factor(.x))) # Convert characters to factors

# Load custom functions 
source("R/my-functions.R")

data_analysis_mod <-
  data_analysis %>%
  mutate(across(.cols = c(goal_z_lat, body_length,  # log transformations
                          learn_prop, tot_z),  
                ~ log(.x),
                .names = "{col}_LN"),
         across(.cols = c(body_length, tot_z),  # mean center and scale variables by 1 SD
                ~ na_rm_scale(.x, na.rm = T),
                .names = "{col}_sc"))

write_csv(data_analysis_mod,
          path = "data/data_Sims-Reader_2020_mod.csv")