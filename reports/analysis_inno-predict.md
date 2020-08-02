
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

<table>

<caption>

Likielihood ratio test for group random effect

</caption>

<thead>

<tr>

<th style="text-align:left;">

call

</th>

<th style="text-align:left;">

Model

</th>

<th style="text-align:left;">

df

</th>

<th style="text-align:left;">

AIC

</th>

<th style="text-align:left;">

BIC

</th>

<th style="text-align:left;">

logLik

</th>

<th style="text-align:left;">

Test

</th>

<th style="text-align:left;">

L.Ratio

</th>

<th style="text-align:left;">

p-value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

lme.for…

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

13

</td>

<td style="text-align:left;">

246.26

</td>

<td style="text-align:left;">

276.73

</td>

<td style="text-align:left;">

\-110.13

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

gls(mod…

</td>

<td style="text-align:left;">

2

</td>

<td style="text-align:left;">

12

</td>

<td style="text-align:left;">

246.96

</td>

<td style="text-align:left;">

275.09

</td>

<td style="text-align:left;">

\-111.48

</td>

<td style="text-align:left;">

1 vs 2

</td>

<td style="text-align:left;">

2.7

</td>

<td style="text-align:left;">

0.1

</td>

</tr>

</tbody>

</table>

## Fixed-effect selection

Test significance of interactions

``` r
m1_inno_predict_red_fix <-  # Fit full model with ML 
  update(m_inno_predict_full, method = "ML")

# Find largest non-significant p-value for interaction
model_sel_temp1 <- drop1(m1_inno_predict_red_fix, test = "Chi")
model_sel_temp1 %>%
  rd_stepwise_out(.) %>%
  knitr::kable(.) 
```

<table>

<thead>

<tr>

<th style="text-align:left;">

Variable

</th>

<th style="text-align:left;">

Df

</th>

<th style="text-align:left;">

AIC

</th>

<th style="text-align:left;">

LRT

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

\<none\>

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

233.03

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z\_sc:pop

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

240.94

</td>

<td style="text-align:left;">

9.92

</td>

<td style="text-align:left;">

0.002

</td>

</tr>

<tr>

<td style="text-align:left;">

<pop:body_length_sc>

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

231.12

</td>

<td style="text-align:left;">

0.09

</td>

<td style="text-align:left;">

0.76

</td>

</tr>

<tr>

<td style="text-align:left;">

<pop:trial>

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

231.58

</td>

<td style="text-align:left;">

0.55

</td>

<td style="text-align:left;">

0.46

</td>

</tr>

</tbody>

</table>

``` r
# Remove largest non-significant p-value for interaction and update model and continue process
m1_temp <- update(m1_inno_predict_red_fix, ~ . -pop:body_length_sc)  # Remove most non-sig interactions
model_sel_temp2 <- drop1(m1_temp, test = "Chi")  # Update model and check remaining sig interactions
model_sel_temp2 %>%
  rd_stepwise_out(.) %>%
  knitr::kable(., caption = "No evidence for learning (i.e. trial) differences between populations (pop:trial)")
```

<table>

<caption>

No evidence for learning (i.e. trial) differences between populations
(<pop:trial>)

</caption>

<thead>

<tr>

<th style="text-align:left;">

Variable

</th>

<th style="text-align:left;">

Df

</th>

<th style="text-align:left;">

AIC

</th>

<th style="text-align:left;">

LRT

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

\<none\>

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

231.12

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

body\_length\_sc

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

229.47

</td>

<td style="text-align:left;">

0.35

</td>

<td style="text-align:left;">

0.55

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z\_sc:pop

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

240.27

</td>

<td style="text-align:left;">

11.15

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

<pop:trial>

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

229.67

</td>

<td style="text-align:left;">

0.55

</td>

<td style="text-align:left;">

0.46

</td>

</tr>

</tbody>

</table>

``` r
m2_temp <- update(m1_temp, ~ . -pop:trial)  # same as above
model_sel_temp3 <- drop1(m2_temp, test = "Chi")  # same as above
model_sel_temp3 %>%
  rd_stepwise_out(.) %>%
  knitr::kable(.)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

Variable

</th>

<th style="text-align:left;">

Df

</th>

<th style="text-align:left;">

AIC

</th>

<th style="text-align:left;">

LRT

</th>

<th style="text-align:left;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

\<none\>

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

229.67

</td>

<td style="text-align:left;">

NA

</td>

<td style="text-align:left;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

body\_length\_sc

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

228.09

</td>

<td style="text-align:left;">

0.42

</td>

<td style="text-align:left;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

trial

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

228.08

</td>

<td style="text-align:left;">

0.41

</td>

<td style="text-align:left;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z\_sc:pop

</td>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

238.44

</td>

<td style="text-align:left;">

10.77

</td>

<td style="text-align:left;">

0.001

</td>

</tr>

</tbody>

</table>

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

<table>

<caption>

Predictors of goal zone latency

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

df

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

4.08

</td>

<td style="text-align:left;">

0.24

</td>

<td style="text-align:left;">

40

</td>

<td style="text-align:left;">

17.24

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z\_sc

</td>

<td style="text-align:left;">

\-0.75

</td>

<td style="text-align:left;">

0.19

</td>

<td style="text-align:left;">

39

</td>

<td style="text-align:left;">

\-3.88

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

\-0.73

</td>

<td style="text-align:left;">

0.28

</td>

<td style="text-align:left;">

39

</td>

<td style="text-align:left;">

\-2.62

</td>

<td style="text-align:left;">

0.013

</td>

</tr>

<tr>

<td style="text-align:left;">

body\_length\_sc

</td>

<td style="text-align:left;">

\-0.08

</td>

<td style="text-align:left;">

0.12

</td>

<td style="text-align:left;">

39

</td>

<td style="text-align:left;">

\-0.64

</td>

<td style="text-align:left;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

trial1

</td>

<td style="text-align:left;">

\-0.04

</td>

<td style="text-align:left;">

0.07

</td>

<td style="text-align:left;">

40

</td>

<td style="text-align:left;">

\-0.65

</td>

<td style="text-align:left;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

tot\_z\_sc:popUpper Aripo

</td>

<td style="text-align:left;">

0.92

</td>

<td style="text-align:left;">

0.25

</td>

<td style="text-align:left;">

39

</td>

<td style="text-align:left;">

3.74

</td>

<td style="text-align:left;">

\<0.001

</td>

</tr>

</tbody>

</table>

``` r
# R squared (delta)
knitr::kable(MuMIn::r.squaredGLMM(m_inno_predict_reduc),
      digits = 2, align = "l",
      caption = "Marginal and conditional R2")
```

<table>

<caption>

Marginal and conditional R2

</caption>

<thead>

<tr>

<th style="text-align:left;">

R2m

</th>

<th style="text-align:left;">

R2c

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

0.59

</td>

<td style="text-align:left;">

0.87

</td>

</tr>

</tbody>

</table>
