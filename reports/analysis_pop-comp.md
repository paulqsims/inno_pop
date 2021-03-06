
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
  rd_tidy_out(.) %>%  # round variables
  select(-c(data, model))  # extract relevant columns 
```

Body length population comparison model summary

``` r
data_analysis %>%
  filter(trial == 1) %>%
  lm(body_length_LN ~ pop, data = .) %>%
  pretty_PredictTab(.)
```

| term           | estimate | std.error | statistic | p.value |
| :------------- | :------- | :-------- | :-------- | :------ |
| (Intercept)    | 3        | 0.03      | 101.11    | \<0.001 |
| popUpper Aripo | 0.07     | 0.04      | 1.84      | 0.073   |

Total zones entered population comparison model summary

``` r
data_analysis %>%
  filter(trial == 1) %>%
  lm(tot_z_LN ~ pop, data = .) %>%
  pretty_PredictTab(.)
```

| term           | estimate | std.error | statistic | p.value |
| :------------- | :------- | :-------- | :-------- | :------ |
| (Intercept)    | 4.27     | 0.08      | 53.56     | \<0.001 |
| popUpper Aripo | 0.28     | 0.1       | 2.76      | 0.009   |

### Innovation: Goal zone latency

Fit innovation model

``` r
# Create a data set without NAs - `lme()` won't remove them
data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat_LN, pop, group,
         site_uni, trial) %>%
  drop_na() 

# Fit population comparison model
m_inno_pop_comp <- 
  lme(goal_z_lat_LN ~ pop + trial,
      weights = varIdent(form = ~ 1|site_uni * pop),  # control for heteroscedasticity
      random = ~ 1 | group,
      contrasts = list(trial = c(-1,1)),  # Change trial contrasts for marginal effects for average trial 
      data = data_analysis_NA_inno,
      method = "REML")
```

Innovation population comparison model summary

``` r
# Tidy innovation predictor model
m_inno_pop_comp_tidy <-
  m_inno_pop_comp %>%
  pretty_PredictTab(., mixedModel = TRUE, kable = FALSE) %>%
  mutate(response = "goal_zone_lat_LN") %>%
  relocate(response) %>%
  filter(term != "trial1") %>%  # remove trial output as it is not of interest here
  select(-df)

m_inno_pop_comp_tidy %>%
  knitr::kable(.) 
```

| response            | term           | estimate | std.error | statistic | p.value |
| :------------------ | :------------- | :------- | :-------- | :-------- | :------ |
| goal\_zone\_lat\_LN | (Intercept)    | 3.96     | 0.19      | 20.67     | \<0.001 |
| goal\_zone\_lat\_LN | popUpper Aripo | \-0.56   | 0.22      | \-2.53    | 0.015   |

### Learning: Improvement ratio

Fit learning population comparison model

``` r
# Create dataset without NAs - `lme()` won't remove them
data_analysis_NA_learn <-
  data_analysis %>%
  filter(trial == 1) %>%
  select(learn_prop_LN, pop, group, site_uni) %>%
  drop_na() 

m_learn_pop_comp <- 
  gls(learn_prop_LN ~ pop,
      weights = varIdent(form = ~ 1|site_uni * pop),
      data = data_analysis_NA_learn,
      method = "REML")
```

Population differences in learning model summary

``` r
# Tidy learning population comparison model
m_learn_pop_comp_tidy <-
  m_learn_pop_comp %>%
  pretty_PredictTab(., mixedModel = TRUE, kable = FALSE) %>%
  mutate(response = "improvement_ratio") %>%
  relocate(response)

# Model summary
m_learn_pop_comp_tidy %>%
  knitr::kable(.) 
```

| response           | term           | estimate | std.error | statistic | p.value |
| :----------------- | :------------- | :------- | :-------- | :-------- | :------ |
| improvement\_ratio | (Intercept)    | \-0.01   | 0.18      | \-0.08    | 0.94    |
| improvement\_ratio | popUpper Aripo | \-0.23   | 0.28      | \-0.82    | 0.42    |

Reduced learning model

  - Remove population to see if there was learning overall

<!-- end list -->

``` r
# update model to intercept only
m_learn <- update(m_learn_pop_comp, ~ 1)  

# Model summary: learning overall
pretty_PredictTab(m_learn)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | :------- | :-------- | :-------- | :------ |
| (Intercept) | \-0.09   | 0.14      | \-0.7     | 0.49    |

## All population comparison model summaries

``` r
# Bind all population comparisons together
m_pop_comp_final_tidy <-
  bind_rows(m_learn_pop_comp_tidy, m_inno_pop_comp_tidy,
            m_length_totz_popcomp_tidy) %>%
  rename(predictor = "term")

m_pop_comp_final_tidy %>% # See reduced inno predictor model for trial measure of learning
  knitr::kable(.) 
```

| response            | predictor      | estimate | std.error | statistic | p.value |
| :------------------ | :------------- | :------- | :-------- | :-------- | :------ |
| improvement\_ratio  | (Intercept)    | \-0.01   | 0.18      | \-0.08    | 0.94    |
| improvement\_ratio  | popUpper Aripo | \-0.23   | 0.28      | \-0.82    | 0.42    |
| goal\_zone\_lat\_LN | (Intercept)    | 3.96     | 0.19      | 20.67     | \<0.001 |
| goal\_zone\_lat\_LN | popUpper Aripo | \-0.56   | 0.22      | \-2.53    | 0.015   |
| body\_length\_LN    | (Intercept)    | 3        | 0.03      | 101.11    | \<0.001 |
| body\_length\_LN    | popUpper Aripo | 0.07     | 0.04      | 1.84      | 0.073   |
| tot\_z\_LN          | (Intercept)    | 4.27     | 0.08      | 53.56     | \<0.001 |
| tot\_z\_LN          | popUpper Aripo | 0.28     | 0.1       | 2.76      | 0.009   |
