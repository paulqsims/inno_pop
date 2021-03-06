################################################################################
# Purpose: Custom functions for analyses and formatting
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
###############################################################################

#### Data manipulation ####

# Mean centers and standardizes (1 SD) vector with NA value removal option
#  - Note that the default is False so removal must be specified
#  - scale() from base R does not allow this

na_rm_scale <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
}

# Reads in data and converts characters to factors
#  - Also mutates variables
#  - Only need to input file name without extension in quotes
#  - Assumes file is in the data directory and files are .csv 

read_mod_data <- function(data) {
  read_csv(paste0("data/", data, ".csv", sep = "")) %>%
    mutate(across(where(is.character), ~ as_factor(.x)),
           trial = as.factor(trial),
           across(.cols = c(goal_z_lat, body_length,  # log transformations
                            tot_z),  
                  ~ log(.x),
                  .names = "{col}_LN"),  # add log suffix
           across(.cols = c(body_length, tot_z),  
                  ~ na_rm_scale(.x, na.rm = T), # mean center and scale variables by 1 SD
                  .names = "{col}_sc"))  # add scale suffix
}

#### Rounding functions ####

# Rounds a vector of p-values
#  - Rounds based on p-value size
#  - Distinguishes between 0 and <0.001
#  - Output is character vector!

round_pval <- function(p_value) {
  p_value <- ifelse(p_value < 0.001, "<0.001",
                   ifelse(p_value > 0.001 & p_value <= 0.1,
                          round(p_value, 3),
                          round(p_value,2)))
  p_value <- as.character(p_value)  # change to character to make compatible w/ df that have P <0.001
  }

# Rounds a continuous vector statistic to 2 decimal places
#  - Distinguishes between 0 and < 0.01 
#  - Output is character vector!

round_est <- function(estimate) {
  estimate <- ifelse(estimate < 0.01 & estimate > 0, "<0.01",
                     ifelse(estimate > -0.01 & estimate < 0,
                            "<-0.01",
                            round(estimate,2)))
  estimate <- as.character(estimate) # change to character to make compatible w/ df that have estimates < 0.01
}

# Round tidy model output  
#  - Rounds estimates to 2, p-values to 3
rd_tidy_out <- function(tidyOutput) {
  tidyOutput %>%
    mutate(across(.cols = c(estimate:statistic), ~ round_est(.x)),  # round est
           p.value = round_pval(p.value)) %>%  # round p-value
    return(.)
}

# Round stepwise selection output  
#  - Rounds estimates to 2, p-values to 3
rd_stepwise_out <- function(stepwiseOutput) {
  stepwiseOutput %>%
    rownames_to_column(., var = "Variable") %>%  # col of variable names
    mutate(across(.cols = Df:LRT, ~ round_est(.x)),  # round est
           p.value = round_pval(`Pr(>Chi)`)) %>%  # round p-value
    select(-`Pr(>Chi)`) %>%
    return(.)
}

#### Kable and Tidy functions ####

# Kable title and alignment
#  - Creates title and aligns table location on page
kable_title <- function(kableTable, title) {
  knitr::kable(kableTable, align = "l",  # align to the left
               caption = if (!is.null(title)) { 
                 paste(title)
               } else { paste("") }) %>%  # paste blank if no title specified
  return(.)
}

# Tidy predictor tables
#  - Tidies model output depending on mixed or non-mixed
#  - Creates title and output can be df or kable for pretty kable
pretty_PredictTab <- function(modelOutput, title = NULL,
                              mixedModel = FALSE, kable = TRUE) {
  # Arguments
  #  kable = whether or not a kable should be printed
  if (mixedModel == TRUE) {
    broom.mixed::tidy(modelOutput,
                      effects = "fixed") %>%
    rd_tidy_out(.) %>%  # round tidy output
      {
        if (kable == TRUE) {  # return kable
        kable_title(., title)  # modify kable title and alignment
      } else { print(.) }
        }  # return dataframe, not kable
  } else {  # if not a mixed model
    broom::tidy(modelOutput) %>%
      rd_tidy_out(.) %>%  # round tidy output
        {if (kable == TRUE) {
          kable_title(., title)  # modify kable title and alignment
        } else { print(.) }}  # return dataframe, not kable
  }
}
