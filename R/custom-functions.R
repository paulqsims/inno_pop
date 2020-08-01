################################################################################
# Purpose: Custom functions for analyses and formatting
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
###############################################################################

# Mean centers and standardizes (1 SD) vector with NA value removal option
#
# Note that the default is False so removal must be specified
# scale() from base R does not allow this

na_rm_scale <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
}

# Reads in data and converts characters to factors
# Also mutates variables
#
# Only need to input file name without extension in quotes
# Assumes file is in the data directory and files are .csv 

read_mod_data <- function(data) {
  read_csv(paste0("data/", data, ".csv", sep = "")) %>%
    mutate(across(where(is.character), ~ as_factor(.x)),
           trial = as.factor(trial),
           across(.cols = c(goal_z_lat, body_length,  # log transformations
                            learn_prop, tot_z),  
                  ~ log(.x),
                  .names = "{col}_LN"),
           across(.cols = c(body_length, tot_z),  # mean center and scale variables by 1 SD
                  ~ na_rm_scale(.x, na.rm = T),
                  .names = "{col}_sc"))
}

# Rounding functions ----

# Rounds a vector of p-values
#
# Distinguishes between 0 and <0.001
# Output is character vector!

round_pval <- function(p_value) {
  p_value <- ifelse(p_value < 0.001, "<0.001",
                   ifelse(p_value > 0.001 & p_value <= 0.1,
                          round(p_value, 3),
                          round(p_value,2)))
  p_value <- as.character(p_value)  # change to character to make compatible w/ df that have P <0.001
  }

# Rounds a continuous vector statistic
# 
# Distinguishes between 0 and < 0.01
# Output is character vector!

round_est <- function(estimate) {
  estimate <- ifelse(estimate < 0.01 & estimate > 0, "<0.01",
                     ifelse(estimate > -0.01 & estimate < 0,
                            "<-0.01",
                            round(estimate,2)))
  estimate <- as.character(estimate) # change to character to make compatible w/ df that have estimates < 0.01
}

# Print pretty kable tables

pretty_kable <- function(kableOutput) {
  kableOutput %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              position = "left")
}

pretty_PredictTab <- function(modelOutput, tableCaption) {
  broom.mixed::tidy(modelOutput,
                    effects = "fixed") %>%
    mutate(across(.cols = c(estimate:statistic), ~round_est(.x)),
           p.value = round_pval(p.value)) %>%
    knitr::kable(., align = "l",
          caption = paste(tableCaption)) %>%
    pretty_kable(.)
}
