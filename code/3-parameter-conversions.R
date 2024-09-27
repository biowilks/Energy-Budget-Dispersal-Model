rm(list=ls())
setwd('~/Energy-Budget-Model/data')

# Load packages ----------
library("tidyverse")

# Desired units for parameters

# Body mass, M_C: kg
# Energy storage, E_0: J
# Basal metabolic rate, BMR: J/s
# Field metabolic rate, FMR_disp: J/s
# Locomotion costs, LCOT: J/s
# Movement speed, v_C: m/s
# Field metabolic rate, FMR: J/s
# Time, t: s
# Max dispersal distance, disp_dist:  m

# Load original parameters and conversion constants ----------

# allometric parameters
pars_old_df <- read.csv("pars_old.csv") %>% select(where(function(x) any(!is.na(x))))

# conversion constants (i.e. gammas)
gammas_old_df <- read.csv("gammas_old.csv") %>% select(where(function(x) any(!is.na(x))))

# Setup a function to convert the intercepts into common units ----------

# Extract parameters from tables and put them in a named list
input_list <- c(setNames(pars_old_df$par_value_old, pars_old_df$par_name), setNames(gammas_old_df$par_value_old, gammas_old_df$par_name)) %>% as.list

# Make the named list entries accessible to all functions in the global environment
list2env(input_list, .GlobalEnv)

# Converts the original intercept to standard units using regular expression that are saved in the original dataframe
calc_conv <- function(x) {sapply(x, \(x) eval(parse(text = as.expression(x)))) }


# Final table with old and converted parameter values ----------
# Add column of converted intercepts to parameters table
pars_new_df <- pars_old_df %>% mutate(par_val_conv = calc_conv(pars_old_df$conversion_equation))
# Extract parameter name and converted value needed for Bioenergetic dispersal model
converted_pars <- pars_new_df %>% select(par_name,par_val_conv)

setwd("~/Energy-Budget-Model/output")
write_csv(converted_pars, "convertedparameters.csv")

