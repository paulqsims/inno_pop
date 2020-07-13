################################################################################
# Function: Custom functions
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
# Purpose: Custom functions for analyses and formatting
################################################################################

# Mean centers and standardizes (1 SD) vector with NA value removal option
#
# Note that the default is False so removal must be specified
# scale() from base R does not allow this

na_rm_scale <- function(x, na.rm = FALSE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
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

# Rounds a continous vector statistic
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

