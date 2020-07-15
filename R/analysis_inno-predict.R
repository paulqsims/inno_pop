################################################################################
# Function: Analysis - Predictors of innovation 
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
# Purpose: Predictors of innovation analyses for Sims and Reader 2020
################################################################################

# Housekeeping ----

library(tidyverse)
library(nlme)

# Load custom functions 
source("R/my-functions.R")

data_analysis <- 
  read_csv_wfact("data_Sims-Reader_2020_mod") 

# Create dataset without NAs - lme won't remove them
data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
  drop_na() 

# Random effect testing ----

# Test for significant group differences

inno_full_pred <- formula(goal_z_lat_LN ~ tot_z_sc * pop +
                            body_length_sc * pop +
                            trial * pop)

m_inno_predict_full <- 
  lme(inno_full_pred,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno,
      method = "REML")

m1_inno_predict_red_rand <- 
  gls(inno_full_pred,
      weights = varIdent(form = ~ 1|site_uni * pop),
      data = data_analysis_NA_inno,
      method = "REML")

anova(m_inno_predict_full, m1_inno_predict_red_rand)

# Fixed effect selection ----

# Test significance of interactions

m1_inno_predict_red_fix <- 
  update(m_inno_predict_full, method = "ML")

model_sel_temp1 <- drop1(m1_inno_predict_red_fix, test = "Chi")
model_sel_temp1

m1_temp <- update(m1_inno_predict_red_fix, ~ . -pop:body_length_sc)
model_sel_temp2 <- drop1(m1_temp, test = "Chi")
model_sel_temp2

m2_temp <- update(m1_temp, ~ . -pop:trial)
model_sel_temp3 <- drop1(m2_temp, test = "Chi")
model_sel_temp3

# Final model of innovation predictors

inno_pred_reduc <- update(inno_full_pred, ~ . 
                            -pop:body_length_sc -pop:trial)

# Relevel population for testing total zones slope in other population
# data_analysis_NA$pop <- fct_relevel(data_analysis_NA$pop, "Upper Aripo") 
# data_analysis_NA$pop <- fct_relevel(data_analysis_NA$pop, "Lower Aripo") 

# Change trial contrasts in order to get marginal effects for average trial 
contrasts(data_analysis_NA_inno$trial) <- c(-1,1)
contrasts(data_analysis_NA_inno$trial) # check

# For changing trial back to dummy coding
# contrasts(data_analysis_NA_inno$trial) <- c(0,1)
# contrasts(data_analysis_NA_inno$trial) # check

m_inno_predict_reduc <- 
  lme(inno_pred_reduc,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno)

# Innovation model summary
summary(m_inno_predict_reduc)

# R2
MuMIn::r.squaredGLMM(m_inno_predict_reduc)

# Tidy innovation predictor model
m_inno_predict_final_tidy <-
  m_inno_predict_reduc %>%
  broom.mixed::tidy() %>%
  filter(effect == "fixed") %>%
  mutate(across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("term", "estimate","std.error", "statistic", "p_value") # deselect "df" since not included in tidy

m_inno_predict_final_tidy