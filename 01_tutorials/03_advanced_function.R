#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Shows that you can create the functions as complex as you want
# - Shows a small note in tidy eval
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

## Packages
library(tidyverse)

## Functions
source("R/utils.R")

# 2. Load data ------------------------------------------------------------

## Load data
iris_tbl <- as_tibble(iris)

# 3. Analyze data ---------------------------------------------------------

## Calculate mean of numeric variables in Iris dataset, and sort them

iris_tbl %>% 
    cal_iris_mean()

# 4. General function -----------------------------------------------------

## https://ggplot2.tidyverse.org/reference/tidyeval.html

##Function 


## Apply to other datasets

airquality %>% 
    calc_numeric_mean(
        group = Month
    )

iris_tbl %>% 
    calc_numeric_mean(
        group = Species
    )

