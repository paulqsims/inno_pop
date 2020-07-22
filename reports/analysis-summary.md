Analysis Summary for Sims and Reader 2020
================

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
  select(goal_z_lat_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
  drop_na() 
```

Reduced innovation predictor model (see analysis\_inno-predict.Rmd for
how it was obtained)

``` r
# Relevel population for testing the slope of total zones entered in other population
# data_analysis_NA$pop <- relevel(data_analysis_NA$pop, "Upper Aripo")  # Upper Aripo baseline
# data_analysis_NA$pop <- relevel(data_analysis_NA$pop, "Lower Aripo")  # Lower Aripo baseline, original baseline

# Change trial contrasts in order to get marginal effects for average trial 
contrasts(data_analysis_NA_inno$trial) <- c(-1,1)
# contrasts(data_analysis_NA_inno$trial) # check

# For changing trial back to dummy coding
# contrasts(mydata$trial.F) <- c(0,1)
# contrasts(mydata$trial.F) # check

# Fit reduced model
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
  select("response", "term", "estimate","std.error", "statistic", "p_value") 
```

### Learning: Improvement ratio

Create dataset without NAs - `lme()` won’t remove them

``` r
data_analysis_NA_learn <-
  data_analysis %>%
  filter(trial == 1) %>%
  select(learn_prop_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc) %>%
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
    ## Lower Aripo 1*Lower Aripo Upper Aripo 2*Upper Aripo Lower Aripo 2*Lower Aripo 
    ##                  1.000000                  2.463715                  2.833290 
    ## Upper Aripo 3*Upper Aripo 
    ##                  2.570414 
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
    ## Lower Aripo 1*Lower Aripo Upper Aripo 2*Upper Aripo Lower Aripo 2*Lower Aripo 
    ##                  1.000000                  2.427747                  2.877063 
    ## Upper Aripo 3*Upper Aripo 
    ##                  2.741791 
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

## Cleanup and final results

``` r
# Bind all population comparisons together
m_pop_comp_final_tidy <-
  bind_rows(m_learn_pop_comp_tidy, m_inno_predict_final_tidy,
            m_length_totz_popcomp_tidy) %>%
  rename(predictor = "term")

m_pop_comp_final_tidy  # See reduced inno predictor model for trial measure of learning
```

    ## # A tibble: 8 x 6
    ##   response          predictor      estimate std.error statistic p_value
    ##   <chr>             <chr>          <chr>    <chr>     <chr>     <chr>  
    ## 1 improvement_ratio (Intercept)    -0.01    0.18      -0.08     0.94   
    ## 2 improvement_ratio popUpper Aripo -0.23    0.28      -0.82     0.42   
    ## 3 goal_z_lat_LN     (Intercept)    4.08     0.24      17.24     <0.001 
    ## 4 goal_z_lat_LN     popUpper Aripo -0.73    0.28      -2.62     0.013  
    ## 5 body_length_LN    (Intercept)    3        0.03      101.11    <0.001 
    ## 6 body_length_LN    popUpper Aripo 0.07     0.04      1.84      0.073  
    ## 7 tot_z_LN          (Intercept)    4.27     0.08      53.56     <0.001 
    ## 8 tot_z_LN          popUpper Aripo 0.28     0.1       2.76      0.009

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

    ##                          Model df      AIC      BIC    logLik   Test L.Ratio
    ## m_inno_predict_full          1 13 246.2637 276.7331 -110.1318               
    ## m1_inno_predict_red_rand     2 12 246.9630 275.0886 -111.4815 1 vs 2 2.69932
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
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Final model of innovation predictors
inno_pred_reduc <- update(inno_full_pred, ~ .  # Update model formula
                            -pop:body_length_sc -pop:trial)
```

## Final model of predictors of innovation

Fit final model

``` r
# Relevel population for testing total zones slope in other population
# data_analysis_NA$pop <- relevel(data_analysis_NA$pop, "Upper Aripo")  # Upper Aripo baseline
# data_analysis_NA$pop <- relevel(data_analysis_NA$pop, "Lower Aripo")  # Lower Aripo baseline, original 

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
    ## Lower Aripo 1*Lower Aripo Upper Aripo 2*Upper Aripo Lower Aripo 2*Lower Aripo 
    ##                  1.000000                  2.096746                  3.033792 
    ## Upper Aripo 3*Upper Aripo 
    ##                  2.253602 
    ## Fixed effects: list(inno_pred_reduc) 
    ##                             Value  Std.Error DF   t-value p-value
    ## (Intercept)              4.083566 0.23692451 40 17.235727  0.0000
    ## tot_z_sc                -0.747702 0.19251774 39 -3.883808  0.0004
    ## popUpper Aripo          -0.734853 0.28064841 39 -2.618412  0.0125
    ## body_length_sc          -0.075530 0.11729392 39 -0.643940  0.5234
    ## trial1                  -0.044648 0.06844658 40 -0.652308  0.5179
    ## tot_z_sc:popUpper Aripo  0.917738 0.24549205 39  3.738360  0.0006
    ##  Correlation: 
    ##                         (Intr) tt_z_s ppUppA bdy_l_ trial1
    ## tot_z_sc                 0.608                            
    ## popUpper Aripo          -0.856 -0.494                     
    ## body_length_sc           0.125 -0.192 -0.204              
    ## trial1                   0.006 -0.003  0.011  0.002       
    ## tot_z_sc:popUpper Aripo -0.470 -0.795  0.277  0.207 -0.015
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.4189402 -0.8045606  0.1283831  0.6176297  2.2319472 
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
    ##   term                    estimate std.error statistic p_value
    ##   <chr>                   <chr>    <chr>     <chr>     <chr>  
    ## 1 (Intercept)             4.08     0.24      17.24     <0.001 
    ## 2 tot_z_sc                -0.75    0.19      -3.88     <0.001 
    ## 3 popUpper Aripo          -0.73    0.28      -2.62     0.013  
    ## 4 body_length_sc          -0.08    0.12      -0.64     0.52   
    ## 5 trial1                  -0.04    0.07      -0.65     0.52   
    ## 6 tot_z_sc:popUpper Aripo 0.92     0.25      3.74      <0.001

# Figures

  - Author: Paul Q. Sims
  - Contact: <paul.q.sims@gmail.com>
  - Date: 2020
  - Purpose: Plots for Sims and Reader 2020

<!-- end list -->

``` r
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file(),
                     eval = TRUE, echo = TRUE, message = FALSE,
                     warning = FALSE)
knitr::opts_chunk$set(root.dir = rprojroot::find_rstudio_root_file(),
                      fig.path = here::here("figs/"),
                     eval = TRUE, echo = TRUE, message = FALSE,
                     warning = FALSE, dev = c("pdf"), dpi = 300)
```

## Setup

``` r
# Load libraries
library(tidyverse)  # for cleaning and modifying data
library(nlme)  # formixed models and generalized least squares
library(ggplot2)  # for plots
library(ggsignif)  # for sig stars
library(effects)  # for marginal effects

# Load personalized functions
source("R/custom-functions.R")

# Read in data
data_analysis <-
  read_mod_data("data_Sims-Reader_2020")

# Create dataset without NAs - lme won't remove them
data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat, goal_z_lat_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
  drop_na() 

# Change trial contrasts in order to get marginal effects for average trial 
contrasts(data_analysis_NA_inno$trial) <- c(-1,1)
# contrasts(data_analysis_NA_inno$trial) # check

# For changing trial back to dummy coding
# contrasts(mydata$trial.F) <- c(0,1)
# contrasts(mydata$trial.F) # check

# Create reduced innovation model 
m_inno_predict_reduc <- 
  lme(goal_z_lat_LN ~ tot_z_sc * pop + body_length_sc + 
        trial,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno)

# Obtain raw values for innovation, averaged between trial 1 and trial 2
data_inno_mean_plot <-
  data_analysis_NA_inno %>%
  group_by(group) %>%
  mutate(inno_mean = mean(goal_z_lat,
                          na.rm = TRUE),
         Population = pop) %>% # rename for total zones predictor plot for plot legend
  filter(trial == 1) 
```

## Population comparison of innovation

``` r
# Obtain predicted values for each population
#   marginal means, mean of trial, and total zones entered

data_inno_means <- 
  as.data.frame(
    Effect("pop", m_inno_predict_reduc,
           data_analysis_NA_inno)) %>%
    mutate(across(.cols = fit:upper, ~ exp(.x))) # back-transform to normal

# Plot estimated mean of innovation for each population
# data points represent raw latency values, averaged across trials

p <- ggplot(data_inno_mean_plot,  
            aes(x = pop,
                y = inno_mean,
                color = pop,
                shape = pop,
                fill = pop)) + 
  geom_point(size = 2.8,  # raw data points
             alpha = 0.65,
             position = position_jitterdodge(),
             show.legend = F) +
  geom_point(data = data_inno_means,  # population means
             aes(x = pop,
                 y = fit,
                 color = pop,
                 shape = pop,
                 fill = pop),
             size=5.8) +
  geom_errorbar(data = data_inno_means,  # population error bars
                aes(x = pop,
                    y = fit,
                    ymin=lower,
                    ymax=upper),
                width=.3,                    
                position = position_dodge(width = 0.9),
                size = 1.25) +
  geom_signif(comparisons = list(c("Lower Aripo", "Upper Aripo")),  # significance brackets 
              map_signif_level = T, annotation = "*", vjust = 0.65, 
              y_position = 6.6, color = "black", size = 1,
              textsize = 12, fontface = "bold") +
  xlab("Population") +
  ylab(expression(paste(bold(Goal~zone~latency~log)[italic(e)]~bold((sec))~bold(""%+-%"")*bold("95"*"%"~C.I.)))) +
  scale_color_manual(values = c("#E41A1C", "#377EB8","#377EB8"),
                     name = "Population") +
  scale_shape_discrete(name = "Population") + 
  scale_y_continuous(trans = "log",  # transforming Y axis to log scale
                     breaks = scales::trans_breaks("log", function(x) exp(x)), 
                     labels = scales::number_format(accuracy = 1)) +
  coord_trans(y = 'log') +  # transforming Y axis to log scale
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(face = "bold"),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(size = 19),
        legend.position = "none") 

print(p)
```

![](/Users/Paul/Google%20Drive/PhD/inno_pop/figs/pop-comp-inno-1.pdf)<!-- -->

## Innovation predicted by total zones entered

``` r
# Get min and max values for total zones entered for lower and upper aripo 
#   populations, restrict line plot values by raw data

tot_z_range <-  
  data_analysis_NA_inno %>%
  group_by(pop) %>%
  select(tot_z_sc) %>%
  summarize(min_totz = min(tot_z_sc, na.rm = TRUE),  # get real ranges of tot z
            max_totz = max(tot_z_sc, na.rm = TRUE)) %>%
  pivot_wider(names_from = pop, values_from = c(min_totz,  # reshape data to wide
                                                max_totz)) %>%
  rename(min_LA = `min_totz_Lower Aripo`, max_LA = `max_totz_Lower Aripo`,
         min_UA = `min_totz_Upper Aripo`, max_UA = `max_totz_Upper Aripo`)

# Obtain predicted values for each population
#   marginal means, mean of trial and total zones entered

data_inno_means <- 
  as.data.frame(
    Effect(c("tot_z_sc", "pop"), m_inno_predict_reduc,  # marginal means
           data_analysis_NA_inno)) %>%
  mutate(across(.cols = fit:upper, ~ exp(.x)),  # backtransform to normal
         tot_z_sc = ifelse(  # remove tot z values outside real range
           pop == "Lower Aripo" & tot_z_sc < tot_z_range$min_LA |
           pop == "Lower Aripo" & tot_z_sc > tot_z_range$max_LA, NA,
           ifelse(
             pop == "Upper Aripo" & tot_z_sc < tot_z_range$min_UA |
             pop == "Upper Aripo" & tot_z_sc > tot_z_range$max_UA, NA,
               tot_z_sc))) %>%
  rename(Population = "pop") # crucial for plot legend to say Population

# Plot estimated innovation predicted by total zones entered by population
#   data points represent raw latency values, averaged across trials and total
#   zones entered for trial 1

# Get custom y label values and breaks and add to custom_labels.y
# round(exp(seq(from = 1,to = 6, by = 1)), digits = 0)
custom_labels.y <- c(expression(atop("3","(e"^1*")")),
                     expression(atop("7","(e"^2*")")),
                     expression(atop("20","(e"^3*")")),
                     expression(atop("55","(e"^4*")")),
                     expression(atop("148","(e"^5*")")),
                     expression(atop("403","(e"^6*")")))
custom_breaks.y <- round(exp(seq(from = 1,to = 6, by = 1)), digits = 0)

# Plot 
p <- ggplot(data_inno_mean_plot,
            aes(x = tot_z_sc,
                y = inno_mean,
                color = Population,
                shape = Population)) + 
  geom_point(size = 3.5,  # raw data
             alpha = 0.9) +
  geom_line(data = data_inno_means,  # predicted values
            aes(x = tot_z_sc,
                y = fit,
                color = Population,
                linetype = Population),
            size = 1.4) +
  xlab("Total zones entered (standardized)") +
  ylab(expression(paste(bold(Goal~zone~latency~log)[italic(e)]~bold((sec))))) +
  scale_color_manual(values = c("#E41A1C", "#377EB8","#377EB8"),
                     name = "Population") +
  scale_shape_discrete(name = "Population") + 
  scale_y_continuous(trans = "log",  # log transformed y-axis
                     breaks = custom_breaks.y, 
                     labels = custom_labels.y) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(face = "bold"),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(size = 19),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14,
                                    face = "bold"),
        legend.position = c(0.85, 0.87),
        plot.margin = margin(1, 0.5, 0.5, 0.25, "cm")) 

print(p)
```

![](/Users/Paul/Google%20Drive/PhD/inno_pop/figs/inno-predict-total-zones-1.pdf)<!-- -->
