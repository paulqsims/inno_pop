################################################################################
# Function: Analysis - Population comparisons
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
# Purpose: Population comparison analyses for Sims and Reader 2020\
################################################################################

# Housekeeping ----

library(tidyverse)
library(nlme)
library(broom)

data_analysis <- 
  read_csv("data/data_Sims-Reader_2020_mod.csv") %>%
  mutate(across(where(is.character), ~as_factor(.x))) # Convert characters to factors

# Load custom functions 
source("r/my-functions.R")

# Population comparisons ----

# Body length, total zones entered

m_length_totz_popcomp_tidy <- 
  data_analysis %>%
  select(group, pop, trial, body_length_LN, tot_z_LN) %>%
  filter(trial == 1) %>%
  pivot_longer(cols = c("body_length_LN", "tot_z_LN"),
               names_to = "response",
               values_to = "value") %>%
  group_by(response) %>%
  nest() %>%
  mutate(model = map(data, ~lm(value ~ pop, data = .)), 
         tidy_model = map(model, tidy)) %>%
  unnest(tidy_model) %>%
  mutate(across(.cols = c(estimate:statistic), ~ round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select(-c(data, model, p.value)) 

# Body length population comparison summary
m_body_length <-
  data_analysis %>%
  filter(trial == 1) %>%
  lm(body_length_LN ~ pop, data = .) %>%
  summary(.)

# Total zones entered population comparison summary
m_tot_z <-
  data_analysis %>%
  filter(trial == 1) %>%
  lm(tot_z_LN ~ pop, data = .) %>%
  summary(.)

# Innovation 

# Create dataset without NAs - lme won't remove them
data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
  drop_na() 

# Relevel population for testing total zones slope in other population
# data_analysis_NA$pop <- fct_relevel(data_analysis_NA$pop, "Upper Aripo") 
# data_analysis_NA$pop <- fct_relevel(data_analysis_NA$pop, "Lower Aripo") 

# Reduced innovation predictor model
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
  tidy() %>%
  filter(effect == "fixed",
         term %in% c("(Intercept)", "popUpper Aripo")) %>%
  mutate(response = "goal_z_lat_LN",
         across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("response", "term", "estimate","std.error", "statistic", "p_value") # deselect "df" since not included in tidy

# Learning : improvement ratio

# Create dataset without NAs - lme won't remove them
data_analysis_NA_learn <-
  data_analysis %>%
  select(learn_prop_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
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

# Model summary: learning w/out population differences
summary(m_learn)

# Tidy learning population comparison model
m_learn_pop_comp_tidy <-
  m_learn_pop_comp %>%
  broom.mixed::tidy() %>%
  mutate(response = "improvement_ratio",
         across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("response", "term", "estimate","std.error", "statistic", "p_value") 

# Cleanup and final results ----

# Bind all population comparisons together

m_pop_comp_final_tidy <-
  bind_rows(m_learn_pop_comp_tidy, m_inno_predict_final_tidy,
            m_length_totz_popcomp_tidy) %>%
  rename(predictor = "term")

print(m_pop_comp_final_tidy)
