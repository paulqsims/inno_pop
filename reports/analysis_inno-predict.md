
# Analysis: Predictors of innovation

  - Author: Paul Q. Sims
  - Contact: <paul.q.sims@gmail.com>
  - Date: 2020
  - Purpose: Predictors of innovation analyses for Sims and Reader 2020

<!-- end list -->

``` r
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file(),
                     eval = TRUE, echo = TRUE, message = FALSE,
                     warning = FALSE)
knitr::opts_chunk$set(root.dir = rprojroot::find_rstudio_root_file(),
                     eval = TRUE, echo = TRUE, message = FALSE,
                     warning = FALSE)
```

## Setup

``` r
# Load libraries
library(tidyverse)  # for cleaning and modifying data
library(nlme)  # for mixed models and generalized least squares

# Load personalized functions
source("R/custom-functions.R")

# Read in data
data_analysis <-
  read_mod_data("data_Sims-Reader_2020")

# Create dataset without NAs - lme won't remove them
data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
  drop_na() 
```

## Random-effect testing

Check if there are significant among-group differences

``` r
# Model formula for fixed-effects
inno_full_pred <- formula(goal_z_lat_LN ~ tot_z_sc * pop + 
                            body_length_sc * pop +
                            trial * pop)

# Fit model with group
m_inno_predict_full <-  
  lme(inno_full_pred,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno,
      method = "REML")

# Fit model without group
m1_inno_predict_red_rand <-  
  gls(inno_full_pred,
      weights = varIdent(form = ~ 1|site_uni * pop),
      data = data_analysis_NA_inno,
      method = "REML")

# Likelihood ratio test
anova(m_inno_predict_full, m1_inno_predict_red_rand) %>%
  as_tibble(.) %>%
  mutate(call = str_trunc(as.character(.$call), width = 10)) %>%  # condense call
  knitr::kable(., digits = 2, align = "l",
               caption = "Likielihood ratio test for group random effect") 
```

| call     | Model | df | AIC    | BIC    | logLik   | Test   | L.Ratio | p-value |
| :------- | :---- | :- | :----- | :----- | :------- | :----- | :------ | :------ |
| lme.for… | 1     | 13 | 246.26 | 276.73 | \-110.13 |        | NA      | NA      |
| gls(mod… | 2     | 12 | 246.96 | 275.09 | \-111.48 | 1 vs 2 | 2.7     | 0.1     |

Likielihood ratio test for group random effect

## Fixed-effect selection

Test significance of interactions

``` r
# Fit full model with ML 
m1_inno_predict_red_fix <-  
  update(m_inno_predict_full, method = "ML")

# Find largest non-significant p-value for interaction
model_sel_temp1 <- drop1(m1_inno_predict_red_fix, test = "Chi")
model_sel_temp1 %>%
  rd_stepwise_out(.) %>%
  knitr::kable(.) 
```

| Variable             | Df | AIC    | LRT  | p.value |
| :------------------- | :- | :----- | :--- | :------ |
| <none>               | NA | 233.03 | NA   | NA      |
| tot\_z\_sc:pop       | 1  | 240.94 | 9.92 | 0.002   |
| <pop:body_length_sc> | 1  | 231.12 | 0.09 | 0.76    |
| <pop:trial>          | 1  | 231.58 | 0.55 | 0.46    |

``` r
# Remove largest non-significant p-value for interaction and update model and continue process
m1_temp <- update(m1_inno_predict_red_fix, ~ . -pop:body_length_sc)  # Remove most non-sig interactions
model_sel_temp2 <- drop1(m1_temp, test = "Chi")  # Update model and check remaining sig interactions
model_sel_temp2 %>%
  rd_stepwise_out(.) %>%
  knitr::kable(., caption = "No evidence for learning (i.e. trial) differences between populations (pop:trial)")
```

| Variable         | Df | AIC    | LRT   | p.value |
| :--------------- | :- | :----- | :---- | :------ |
| <none>           | NA | 231.12 | NA    | NA      |
| body\_length\_sc | 1  | 229.47 | 0.35  | 0.55    |
| tot\_z\_sc:pop   | 1  | 240.27 | 11.15 | \<0.001 |
| <pop:trial>      | 1  | 229.67 | 0.55  | 0.46    |

No evidence for learning (i.e. trial) differences between populations
(<pop:trial>)

``` r
m2_temp <- update(m1_temp, ~ . -pop:trial)  # same as above
model_sel_temp3 <- drop1(m2_temp, test = "Chi")  # same as above
model_sel_temp3 %>%
  rd_stepwise_out(.) %>%
  knitr::kable(.)
```

| Variable         | Df | AIC    | LRT   | p.value |
| :--------------- | :- | :----- | :---- | :------ |
| <none>           | NA | 229.67 | NA    | NA      |
| body\_length\_sc | 1  | 228.09 | 0.42  | 0.52    |
| trial            | 1  | 228.08 | 0.41  | 0.52    |
| tot\_z\_sc:pop   | 1  | 238.44 | 10.77 | 0.001   |

``` r
# Final model of innovation predictors
inno_pred_reduc <- update(inno_full_pred, ~ .  # Update fixef model formula
                            -pop:body_length_sc -pop:trial)
```

## Final model of predictors of innovation

Fit final model

``` r
# Relevel population for testing total zones slope in other population
# data_analysis_NA_inno$pop <- relevel(data_analysis_NA_inno$pop, "Upper Aripo")  # Upper Aripo baseline
# data_analysis_NA_inno$pop <- relevel(data_analysis_NA_inno$pop, "Lower Aripo")  # Lower Aripo baseline, original 

# Fit final reduced model
m_inno_predict_reduc <- 
  lme(inno_pred_reduc,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      contrasts = list(trial = c(-1,1)),  # Change trial contrasts in order to get marginal effects for average trial 
      data = data_analysis_NA_inno,
      method = "REML")
```

Final model summary

``` r
# Tidy model output
pretty_PredictTab(m_inno_predict_reduc,
                  title = "Predictors of goal zone latency",
                  mixedModel = TRUE) 
```

| term                      | estimate | std.error | df | statistic | p.value |
| :------------------------ | :------- | :-------- | :- | :-------- | :------ |
| (Intercept)               | 4.08     | 0.24      | 40 | 17.24     | \<0.001 |
| tot\_z\_sc                | \-0.75   | 0.19      | 39 | \-3.88    | \<0.001 |
| popUpper Aripo            | \-0.73   | 0.28      | 39 | \-2.62    | 0.013   |
| body\_length\_sc          | \-0.08   | 0.12      | 39 | \-0.64    | 0.52    |
| trial1                    | \-0.04   | 0.07      | 40 | \-0.65    | 0.52    |
| tot\_z\_sc:popUpper Aripo | 0.92     | 0.25      | 39 | 3.74      | \<0.001 |

Predictors of goal zone latency

``` r
# R squared (delta)
knitr::kable(MuMIn::r.squaredGLMM(m_inno_predict_reduc),
      digits = 2, align = "l",
      caption = "Marginal and conditional R2")
```

| R2m  | R2c  |
| :--- | :--- |
| 0.59 | 0.87 |

Marginal and conditional R2
