
# Analysis: Population comparisons

  - Author: Paul Q. Sims
  - Contact: <paul.q.sims@gmail.com>
  - Date: 2020
  - Purpose: Population comparison analyses for Sims and Reader 2020

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
library(broom)  # for tidying model output

# Load personalized functions
source("R/custom-functions.R")

# Read in data
data_analysis <-
  read_mod_data("data_Sims-Reader_2020")
```

## Population comparisons

### Body length, total zones entered

Create list of models for both variables

``` r
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
```

Body length population comparison model summary

``` r
data_analysis %>%
filter(trial == 1) %>%
lm(body_length_LN ~ pop, data = .) %>%
summary(.)
```

    ## 
    ## Call:
    ## lm(formula = body_length_LN ~ pop, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.23062 -0.07586  0.00599  0.03655  0.32960 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.00321    0.02970 101.107   <2e-16 ***
    ## popUpper Aripo  0.06838    0.03724   1.837   0.0734 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1188 on 42 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.07433,    Adjusted R-squared:  0.05229 
    ## F-statistic: 3.373 on 1 and 42 DF,  p-value: 0.07337

Total zones entered population comparison model summary

``` r
data_analysis %>%
filter(trial == 1) %>%
lm(tot_z_LN ~ pop, data = .) %>%
summary(.)
```

    ## 
    ## Call:
    ## lm(formula = tot_z_LN ~ pop, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.73944 -0.16210  0.04391  0.24886  0.49637 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     4.26580    0.07965  53.560  < 2e-16 ***
    ## popUpper Aripo  0.27520    0.09984   2.756  0.00861 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3186 on 42 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.1532, Adjusted R-squared:  0.133 
    ## F-statistic: 7.598 on 1 and 42 DF,  p-value: 0.008609

### Innovation: Goal zone latency

Create a data set without NAs - `lme()` won’t remove them

``` r
data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat_LN, pop, group,
         site_uni, trial) %>%
  drop_na() 
```

Fit innovation model

``` r
# Change trial contrasts in order to get marginal effects for average trial 
contrasts(data_analysis_NA_inno$trial) <- c(-1,1)
# contrasts(data_analysis_NA_inno$trial) # check

# Fit reduced model
m_inno_pop_comp <- 
  lme(goal_z_lat_LN ~ pop + trial,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno,
      method = "REML")

# Tidy innovation predictor model
m_inno_pop_comp_tidy <-
  m_inno_pop_comp %>%
  broom.mixed::tidy() %>%
  filter(effect == "fixed",
         term %in% c("(Intercept)", "popUpper Aripo")) %>%
  mutate(response = "goal_z_lat_LN",
         across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("response", "term", "estimate","std.error", "statistic", "p_value") 
```

### Learning: Improvement ratio

Create dataset without NAs - `lme()` won’t remove them

``` r
data_analysis_NA_learn <-
  data_analysis %>%
  filter(trial == 1) %>%
  select(learn_prop_LN, pop, group, site_uni) %>%
  drop_na() 
```

Build full model

``` r
m_learn_pop_comp <- 
  gls(learn_prop_LN ~ pop,
      weights = varIdent(form = ~ 1|site_uni * pop),
      data = data_analysis_NA_learn,
      method = "REML")
```

Population differences in learning model summary

``` r
summary(m_learn_pop_comp)
```

    ## Generalized least squares fit by REML
    ##   Model: learn_prop_LN ~ pop 
    ##   Data: data_analysis_NA_learn 
    ##        AIC      BIC    logLik
    ##   130.8779 140.8593 -59.43896
    ## 
    ## Variance function:
    ##  Structure: Different standard deviations per stratum
    ##  Formula: ~1 | site_uni * pop 
    ##  Parameter estimates:
    ## Lower Aripo 1*Lower Aripo Upper Aripo 2*Upper Aripo Lower Aripo 2*Lower Aripo Upper Aripo 3*Upper Aripo 
    ##                  1.000000                  2.463715                  2.833290                  2.570414 
    ## 
    ## Coefficients:
    ##                      Value Std.Error    t-value p-value
    ## (Intercept)    -0.01330064 0.1771933 -0.0750629  0.9405
    ## popUpper Aripo -0.22889969 0.2801857 -0.8169571  0.4189
    ## 
    ##  Correlation: 
    ##                (Intr)
    ## popUpper Aripo -0.632
    ## 
    ## Standardized residuals:
    ##          Min           Q1          Med           Q3          Max 
    ## -2.236285624 -0.660417333 -0.005223549  0.801020364  2.743517375 
    ## 
    ## Residual standard error: 0.4428314 
    ## Degrees of freedom: 41 total; 39 residual

``` r
# Tidy learning population comparison model
m_learn_pop_comp_tidy <-
  m_learn_pop_comp %>%
  broom.mixed::tidy() %>%
  mutate(response = "improvement_ratio",
         across(.cols = c(estimate:statistic), ~round_est(.x)),
         p_value = round_pval(p.value)) %>%
  select("response", "term", "estimate","std.error", "statistic", "p_value") 
```

Reduced learning model (no population differences)

``` r
m_learn <- update(m_learn_pop_comp, ~ 1)  # update model to intercept only

# Model summary: learning overall
summary(m_learn)
```

    ## Generalized least squares fit by REML
    ##   Model: learn_prop_LN ~ 1 
    ##   Data: data_analysis_NA_learn 
    ##        AIC      BIC    logLik
    ##   128.7657 137.2101 -59.38286
    ## 
    ## Variance function:
    ##  Structure: Different standard deviations per stratum
    ##  Formula: ~1 | site_uni * pop 
    ##  Parameter estimates:
    ## Lower Aripo 1*Lower Aripo Upper Aripo 2*Upper Aripo Lower Aripo 2*Lower Aripo Upper Aripo 3*Upper Aripo 
    ##                  1.000000                  2.427747                  2.877063                  2.741791 
    ## 
    ## Coefficients:
    ##                   Value Std.Error    t-value p-value
    ## (Intercept) -0.09488451 0.1357196 -0.6991215  0.4885
    ## 
    ## Standardized residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.2559302 -0.7385783 -0.1283975  0.7661007  2.6926018 
    ## 
    ## Residual standard error: 0.4353539 
    ## Degrees of freedom: 41 total; 40 residual

## Cleanup and final results

``` r
# Bind all population comparisons together
m_pop_comp_final_tidy <-
  bind_rows(m_learn_pop_comp_tidy, m_inno_pop_comp_tidy,
            m_length_totz_popcomp_tidy) %>%
  rename(predictor = "term")

m_pop_comp_final_tidy  # See reduced inno predictor model for trial measure of learning
```

    ## # A tibble: 8 x 6
    ##   response          predictor      estimate std.error statistic p_value
    ##   <chr>             <chr>          <chr>    <chr>     <chr>     <chr>  
    ## 1 improvement_ratio (Intercept)    -0.01    0.18      -0.08     0.94   
    ## 2 improvement_ratio popUpper Aripo -0.23    0.28      -0.82     0.42   
    ## 3 goal_z_lat_LN     (Intercept)    3.96     0.19      20.67     <0.001 
    ## 4 goal_z_lat_LN     popUpper Aripo -0.56    0.22      -2.53     0.015  
    ## 5 body_length_LN    (Intercept)    3        0.03      101.11    <0.001 
    ## 6 body_length_LN    popUpper Aripo 0.07     0.04      1.84      0.073  
    ## 7 tot_z_LN          (Intercept)    4.27     0.08      53.56     <0.001 
    ## 8 tot_z_LN          popUpper Aripo 0.28     0.1       2.76      0.009
