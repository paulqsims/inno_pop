
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
inno_full_pred <- formula(goal_z_lat_LN ~ tot_z_sc * pop +  # fixed effect formula
                            body_length_sc * pop +
                            trial * pop)

m_inno_predict_full <-  # Fit model with group
  lme(inno_full_pred,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno,
      method = "REML")

m1_inno_predict_red_rand <-  # Fit model without group
  gls(inno_full_pred,
      weights = varIdent(form = ~ 1|site_uni * pop),
      data = data_analysis_NA_inno,
      method = "REML")

anova(m_inno_predict_full, m1_inno_predict_red_rand)  # Likelihood ratio test
```

    ##                          Model df      AIC      BIC
    ## m_inno_predict_full          1 13 246.2637 276.7331
    ## m1_inno_predict_red_rand     2 12 246.9630 275.0886
    ##                             logLik   Test L.Ratio
    ## m_inno_predict_full      -110.1318               
    ## m1_inno_predict_red_rand -111.4815 1 vs 2 2.69932
    ##                          p-value
    ## m_inno_predict_full             
    ## m1_inno_predict_red_rand  0.1004

## Fixed-effect selection

Test significance of interactions

``` r
m1_inno_predict_red_fix <-  # Fit full model with ML 
  update(m_inno_predict_full, method = "ML")

# Find largest non-significant p-value for interaction
model_sel_temp1 <- drop1(m1_inno_predict_red_fix, test = "Chi")
model_sel_temp1
```

    ## Single term deletions
    ## 
    ## Model:
    ## goal_z_lat_LN ~ tot_z_sc * pop + body_length_sc * pop + trial * 
    ##     pop
    ##                    Df    AIC    LRT Pr(>Chi)   
    ## <none>                233.03                   
    ## tot_z_sc:pop        1 240.94 9.9154 0.001639 **
    ## pop:body_length_sc  1 231.12 0.0909 0.763003   
    ## pop:trial           1 231.58 0.5545 0.456505   
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Remove largest non-significant p-value for interaction and update model and continue process
m1_temp <- update(m1_inno_predict_red_fix, ~ . -pop:body_length_sc)  # Remove most non-sig interactions
model_sel_temp2 <- drop1(m1_temp, test = "Chi")  # Update model and check remaining sig interactions
model_sel_temp2
```

    ## Single term deletions
    ## 
    ## Model:
    ## goal_z_lat_LN ~ tot_z_sc + pop + body_length_sc + trial + tot_z_sc:pop + 
    ##     pop:trial
    ##                Df    AIC     LRT  Pr(>Chi)    
    ## <none>            231.12                      
    ## body_length_sc  1 229.47  0.3503 0.5539512    
    ## tot_z_sc:pop    1 240.27 11.1526 0.0008391 ***
    ## pop:trial       1 229.67  0.5513 0.4577859    
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
m2_temp <- update(m1_temp, ~ . -pop:trial)  # same as above
model_sel_temp3 <- drop1(m2_temp, test = "Chi")  # same as above
model_sel_temp3
```

    ## Single term deletions
    ## 
    ## Model:
    ## goal_z_lat_LN ~ tot_z_sc + pop + body_length_sc + trial + tot_z_sc:pop
    ##                Df    AIC     LRT Pr(>Chi)   
    ## <none>            229.67                    
    ## body_length_sc  1 228.09  0.4196 0.517130   
    ## trial           1 228.08  0.4095 0.522243   
    ## tot_z_sc:pop    1 238.44 10.7692 0.001032 **
    ## ---
    ## Signif. codes:  
    ## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Final model of innovation predictors
inno_pred_reduc <- update(inno_full_pred, ~ .  # Update model formula
                            -pop:body_length_sc -pop:trial)
```

## Final model of predictors of innovation

Fit final model

``` r
# Relevel population for testing total zones slope in other population
# data_analysis_NA_inno$pop <- relevel(data_analysis_NA_inno$pop, "Upper Aripo")  # Upper Aripo baseline
# data_analysis_NA_inno$pop <- relevel(data_analysis_NA_inno$pop, "Lower Aripo")  # Lower Aripo baseline, original 

# Change trial contrasts in order to get marginal effects for average trial 
contrasts(data_analysis_NA_inno$trial) <- c(-1,1)
#contrasts(data_analysis_NA_inno$trial) # check

# For changing trial back to dummy coding
# contrasts(data_analysis_NA_inno$trial) <- c(0,1)
# contrasts(data_analysis_NA_inno$trial) # check

# Fit final reduced model
m_inno_predict_reduc <- 
  lme(inno_pred_reduc,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno)

# Model summary
summary(m_inno_predict_reduc)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: data_analysis_NA_inno 
    ##        AIC     BIC    logLik
    ##   243.2391 269.303 -110.6196
    ## 
    ## Random effects:
    ##  Formula: ~1 | group
    ##         (Intercept)  Residual
    ## StdDev:   0.4860656 0.3338968
    ## 
    ## Variance function:
    ##  Structure: Different standard deviations per stratum
    ##  Formula: ~1 | site_uni * pop 
    ##  Parameter estimates:
    ## Lower Aripo 1*Lower Aripo Upper Aripo 2*Upper Aripo 
    ##                  1.000000                  2.096746 
    ## Lower Aripo 2*Lower Aripo Upper Aripo 3*Upper Aripo 
    ##                  3.033792                  2.253602 
    ## Fixed effects: list(inno_pred_reduc) 
    ##                             Value  Std.Error DF
    ## (Intercept)              4.083566 0.23692451 40
    ## tot_z_sc                -0.747702 0.19251774 39
    ## popUpper Aripo          -0.734853 0.28064841 39
    ## body_length_sc          -0.075530 0.11729392 39
    ## trial1                  -0.044648 0.06844658 40
    ## tot_z_sc:popUpper Aripo  0.917738 0.24549205 39
    ##                           t-value p-value
    ## (Intercept)             17.235727  0.0000
    ## tot_z_sc                -3.883808  0.0004
    ## popUpper Aripo          -2.618412  0.0125
    ## body_length_sc          -0.643940  0.5234
    ## trial1                  -0.652308  0.5179
    ## tot_z_sc:popUpper Aripo  3.738360  0.0006
    ##  Correlation: 
    ##                         (Intr) tt_z_s ppUppA bdy_l_
    ## tot_z_sc                 0.608                     
    ## popUpper Aripo          -0.856 -0.494              
    ## body_length_sc           0.125 -0.192 -0.204       
    ## trial1                   0.006 -0.003  0.011  0.002
    ## tot_z_sc:popUpper Aripo -0.470 -0.795  0.277  0.207
    ##                         trial1
    ## tot_z_sc                      
    ## popUpper Aripo                
    ## body_length_sc                
    ## trial1                        
    ## tot_z_sc:popUpper Aripo -0.015
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3 
    ## -2.4189402 -0.8045606  0.1283831  0.6176297 
    ##        Max 
    ##  2.2319472 
    ## 
    ## Number of Observations: 85
    ## Number of Groups: 44

``` r
# R2
MuMIn::r.squaredGLMM(m_inno_predict_reduc)
```

    ##            R2m       R2c
    ## [1,] 0.5882877 0.8680057

Tidy final model summary

``` r
m_inno_predict_final_tidy <-
  m_inno_predict_reduc %>%
  broom.mixed::tidy() %>%
  filter(effect == "fixed") %>%
  mutate(across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("term", "estimate","std.error", "statistic", "p_value")

m_inno_predict_final_tidy
```

    ## # A tibble: 6 x 5
    ##   term           estimate std.error statistic p_value
    ##   <chr>          <chr>    <chr>     <chr>     <chr>  
    ## 1 (Intercept)    4.08     0.24      17.24     <0.001 
    ## 2 tot_z_sc       -0.75    0.19      -3.88     <0.001 
    ## 3 popUpper Aripo -0.73    0.28      -2.62     0.013  
    ## 4 body_length_sc -0.08    0.12      -0.64     0.52   
    ## 5 trial1         -0.04    0.07      -0.65     0.52   
    ## 6 tot_z_sc:popU… 0.92     0.25      3.74      <0.001
