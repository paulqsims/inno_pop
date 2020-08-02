
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
         p.value = round_pval(p.value)) %>%  
  select(-c(data, model))  # extract relevant columns 
```

Body length population comparison model summary

``` r
data_analysis %>%
filter(trial == 1) %>%
lm(body_length_LN ~ pop, data = .) %>%
pretty_PredictTab(.)
```

<table>

<caption>

</caption>

<thead>

<tr>

<th style="text-align:left;">

term

</th>

<th style="text-align:left;">

estimate

</th>

<th style="text-align:left;">

std.error

</th>

<th style="text-align:left;">

statistic

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

3

</td>

<td style="text-align:left;">

0.03

</td>

<td style="text-align:left;">

101.11

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

0.07

</td>

<td style="text-align:left;">

0.04

</td>

<td style="text-align:left;">

1.84

</td>

<td style="text-align:left;">

0.073

</td>

</tr>

</tbody>

</table>

Total zones entered population comparison model summary

``` r
data_analysis %>%
filter(trial == 1) %>%
lm(tot_z_LN ~ pop, data = .) %>%
pretty_PredictTab(.)
```

<table>

<caption>

</caption>

<thead>

<tr>

<th style="text-align:left;">

term

</th>

<th style="text-align:left;">

estimate

</th>

<th style="text-align:left;">

std.error

</th>

<th style="text-align:left;">

statistic

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

4.27

</td>

<td style="text-align:left;">

0.08

</td>

<td style="text-align:left;">

53.56

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

0.28

</td>

<td style="text-align:left;">

0.1

</td>

<td style="text-align:left;">

2.76

</td>

<td style="text-align:left;">

0.009

</td>

</tr>

</tbody>

</table>

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
# Fit reduced model, see predictors of innovation section for how this model was obtained
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

<table>

<thead>

<tr>

<th style="text-align:left;">

response

</th>

<th style="text-align:left;">

term

</th>

<th style="text-align:left;">

estimate

</th>

<th style="text-align:left;">

std.error

</th>

<th style="text-align:left;">

statistic

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

goal\_zone\_lat\_LN

</td>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

3.96

</td>

<td style="text-align:left;">

0.19

</td>

<td style="text-align:left;">

20.67

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

goal\_zone\_lat\_LN

</td>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

\-0.56

</td>

<td style="text-align:left;">

0.22

</td>

<td style="text-align:left;">

\-2.53

</td>

<td style="text-align:left;">

0.015

</td>

</tr>

</tbody>

</table>

### Learning: Improvement ratio

Create dataset without NAs - `lme()` won’t remove them

``` r
data_analysis_NA_learn <-
  data_analysis %>%
  filter(trial == 1) %>%
  select(learn_prop_LN, pop, group, site_uni) %>%
  drop_na() 
```

Fit learning population comparison model

``` r
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

<table>

<thead>

<tr>

<th style="text-align:left;">

response

</th>

<th style="text-align:left;">

term

</th>

<th style="text-align:left;">

estimate

</th>

<th style="text-align:left;">

std.error

</th>

<th style="text-align:left;">

statistic

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

improvement\_ratio

</td>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

\-0.01

</td>

<td style="text-align:left;">

0.18

</td>

<td style="text-align:left;">

\-0.08

</td>

<td style="text-align:left;">

0.94

</td>

</tr>

<tr>

<td style="text-align:left;">

improvement\_ratio

</td>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

\-0.23

</td>

<td style="text-align:left;">

0.28

</td>

<td style="text-align:left;">

\-0.82

</td>

<td style="text-align:left;">

0.42

</td>

</tr>

</tbody>

</table>

Reduced learning model

  - Remove population to see if there was learning overall

<!-- end list -->

``` r
# update model to intercept only
m_learn <- update(m_learn_pop_comp, ~ 1)  

# Model summary: learning overall
pretty_PredictTab(m_learn)
```

<table>

<caption>

</caption>

<thead>

<tr>

<th style="text-align:left;">

term

</th>

<th style="text-align:left;">

estimate

</th>

<th style="text-align:left;">

std.error

</th>

<th style="text-align:left;">

statistic

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

\-0.09

</td>

<td style="text-align:left;">

0.14

</td>

<td style="text-align:left;">

\-0.7

</td>

<td style="text-align:left;">

0.49

</td>

</tr>

</tbody>

</table>

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

<table>

<thead>

<tr>

<th style="text-align:left;">

response

</th>

<th style="text-align:left;">

predictor

</th>

<th style="text-align:left;">

estimate

</th>

<th style="text-align:left;">

std.error

</th>

<th style="text-align:left;">

statistic

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

improvement\_ratio

</td>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

\-0.01

</td>

<td style="text-align:left;">

0.18

</td>

<td style="text-align:left;">

\-0.08

</td>

<td style="text-align:left;">

0.94

</td>

</tr>

<tr>

<td style="text-align:left;">

improvement\_ratio

</td>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

\-0.23

</td>

<td style="text-align:left;">

0.28

</td>

<td style="text-align:left;">

\-0.82

</td>

<td style="text-align:left;">

0.42

</td>

</tr>

<tr>

<td style="text-align:left;">

goal\_zone\_lat\_LN

</td>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

3.96

</td>

<td style="text-align:left;">

0.19

</td>

<td style="text-align:left;">

20.67

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

goal\_zone\_lat\_LN

</td>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

\-0.56

</td>

<td style="text-align:left;">

0.22

</td>

<td style="text-align:left;">

\-2.53

</td>

<td style="text-align:left;">

0.015

</td>

</tr>

<tr>

<td style="text-align:left;">

body\_length\_LN

</td>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

3

</td>

<td style="text-align:left;">

0.03

</td>

<td style="text-align:left;">

101.11

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

body\_length\_LN

</td>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

0.07

</td>

<td style="text-align:left;">

0.04

</td>

<td style="text-align:left;">

1.84

</td>

<td style="text-align:left;">

0.073

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z\_LN

</td>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:left;">

4.27

</td>

<td style="text-align:left;">

0.08

</td>

<td style="text-align:left;">

53.56

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z\_LN

</td>

<td style="text-align:left;">

popUpper Aripo

</td>

<td style="text-align:left;">

0.28

</td>

<td style="text-align:left;">

0.1

</td>

<td style="text-align:left;">

2.76

</td>

<td style="text-align:left;">

0.009

</td>

</tr>

</tbody>

</table>
