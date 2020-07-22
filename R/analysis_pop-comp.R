################################################################################
# Function: Analysis - Population comparisons
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
# Purpose: Population comparison analyses for Sims and Reader 2020\
################################################################################

#### Housekeeping #### 

library(tidyverse)  # for cleaning and modifying data
library(nlme)  # for mixed models and generalized least squares
library(broom)  # for tidying model output

# Load custom functions 

source("R/custom-functions.R")  

# Load data - only need file name (not extension)

data_analysis <- 
  read_mod_data("data_Sims-Reader_2020") 

#### Population comparisons ####

## Body length, total zones entered ##

# Create list of models for both variables

m_length_totz_popcomp_tidy <- 
  data_analysis %>%
  select(group, pop, trial, body_length_LN, tot_z_LN) %>%
  filter(trial == 1) %>% 
  pivot_longer(cols = c("body_length_LN", "tot_z_LN"),  # reshape data
               names_to = "response",
               values_to = "value") %>%
  group_by(response) %>%
  nest() %>%
  mutate(model = map(data, ~lm(value ~ pop, data = .)),  # run models for each
         tidy_model = map(model, tidy)) %>%  # tidy output
  unnest(tidy_model) %>%
  mutate(across(.cols = c(estimate:statistic), ~ round_est(.x)),  # round values
         p_value = round_pval(p.value)) %>%  
  select(-c(data, model, p.value))  # extract relevant columns

# Body length population comparison model summary

data_analysis %>%
filter(trial == 1) %>%
lm(body_length_LN ~ pop, data = .) %>%
summary(.)

# Total zones entered population comparison model summary

data_analysis %>%
filter(trial == 1) %>%
lm(tot_z_LN ~ pop, data = .) %>%
summary(.)

## Innovation population comparison ##

# Create dataset without NAs - lme won't remove them

data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
  drop_na() 

# Relevel population for testing total zones slope in other population
# data_analysis_NA$pop <- relevel(data_analysis_NA$pop, "Upper Aripo")  # Upper Aripo baseline
# data_analysis_NA$pop <- relevel(data_analysis_NA$pop, "Lower Aripo")  # Lower Aripo baseline, original baseline

# Change trial contrasts in order to get marginal effects for average trial 
contrasts(data_analysis_NA_inno$trial) <- c(-1,1)
contrasts(data_analysis_NA_inno$trial) # check

# For changing trial back to dummy coding
# contrasts(mydata$trial.F) <- c(0,1)
# contrasts(mydata$trial.F) # check

# Reduced innovation predictor model (see analysis_inno-predict.R for how it was obtained)

m_inno_predict_final <- 
  lme(goal_z_lat_LN ~ tot_z_sc * pop + 
        body_length_sc + trial,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno,
      method = "REML")

# Tidy innovation predictor model

m_inno_predict_final_tidy <-
  m_inno_predict_final %>%
  broom.mixed::tidy() %>%
  filter(effect == "fixed",
         term %in% c("(Intercept)", "popUpper Aripo")) %>%
  mutate(response = "goal_z_lat_LN",
         across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("response", "term", "estimate","std.error", "statistic", "p_value") # deselect "df" since not included in tidy

## Learning: improvement ratio ##

# Create dataset without NAs - lme won't remove them

data_analysis_NA_learn <-
  data_analysis %>%
  filter(trial == 1) %>%
  select(learn_prop_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc) %>%
  drop_na() 

# Reduced learning population comparison model

m_learn_pop_comp <- 
  gls(learn_prop_LN ~ pop,
      weights = varIdent(form = ~ 1|site_uni * pop),
      data = data_analysis_NA_learn,
      method = "REML")

# Model summary: population differences in learning

summary(m_learn_pop_comp)

# Reduced learning model

m_learn <- update(m_learn_pop_comp, ~ 1)

# Model summary: learning overall

summary(m_learn)

# Tidy learning population comparison model

m_learn_pop_comp_tidy <-
  m_learn_pop_comp %>%
  broom.mixed::tidy() %>%
  mutate(response = "improvement_ratio",
         across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("response", "term", "estimate","std.error", "statistic", "p_value") 

#### Cleanup and final results ####

# Bind all population comparisons together

m_pop_comp_final_tidy <-
  bind_rows(m_learn_pop_comp_tidy, m_inno_predict_final_tidy,
            m_length_totz_popcomp_tidy) %>%
  rename(predictor = "term")

print(m_pop_comp_final_tidy)  # See reduced inno predictor model for trial measure of learning
