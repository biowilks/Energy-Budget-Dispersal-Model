rm(list=ls())
setwd("C:/Users/xr49abiw/Documents/Energy-Budget-Model/output")

# Load packages ----------
library("rstudioapi")
library("tidyverse")

# Import converted parameters ----------
conv_para <- read.csv("convertedparameters.csv")
conv_para_list <- setNames(conv_para$par_val_conv, conv_para$par_name)

# Description of input variables, parameters and outputs ----------

## 1) INPUT VARIABLES
## m_C = body mass in g
## locomotion_mode = flying, running or swimming
## disp_dist = dispersal distance in m
## lambda = % of energy storage needed for survival after dispersing

## 2) OTHER VARIABLES
##  A) ENERGY STORAGE
##     E_0 = Energy storage in J
##     a1 = intercept: fat mass of birds
##     b1 = exponent: fat mass of birds
##     a2 = intercept energy storage of mammals
##     b2 = exponent: fat mass of mammals
##     a3 = intercept: energy density of fish
##     b3 = exponent: energy density of fish
##
##     E_alpha = Energy available for dispersal in J


##  B) ENERGY LOSS
##     BMR = basal metabolic rate in J/h
##     a4 = intercept: BMR of birds
##     b4 = exponent: BMR of birds
##     a5 = intercept: BMR of mammals
##     b5 = exponent: BMR of mammals
##     a6 = intercept: BMR of fish
##     b6 = exponent: BMR of fish

##     COT = costs of transport in J/m
##     a7 = intercept: COTmin of flying
##     b7 = exponent: COTmin of flying
##     a8 = intercept: COTmin of running
##     b8 = exponent: COTmin of running
##     a9 = intercept: COTmin of swimming
##     b9 = exponent: COTmin of swimming

##     FMR_disp = field metabolic rate in J/h

##  C) SPEED VARIABLES
##     v_C = movement speed in m/h
##     v_0_fly = intercept: locomotion rate of flying
##     v_0_run = intercept: locomotion rate of running
##     v_0_swim = intercept: locomotion rate of swimming
##     c = exponent: locomotion rate
##     k = intercept: heat-dissipation time
##     d = exponent: heat-dissipation time

##  3) OUTPUTS
##    t = time in h
##    E_C = total energy cost in J
##    E_M = energy cost per meter in J
##    E_R = energy remaining after moving the disp_dist in J
##    E_E = relative energy efficiency (relative to a species available energy) in J


# Bioenergetic dispersal model energy function ----------
energy_fun <- function(m_C,locomotion_mode,disp_dist,lambda) {

##  A) Energy storage
  if (locomotion_mode == "flying") {
    E_0 = conv_para_list[["a1"]] * m_C^conv_para_list[["b1"]] } else if (locomotion_mode == "running") {
    E_0 = conv_para_list[["a2"]] * m_C^conv_para_list[["b2"]] } else if (locomotion_mode == "swimming") {
    E_0 = conv_para_list[["a3"]] * m_C^conv_para_list[["b3"]] }

  # Energy available for dispersal
   E_alpha = ((1-lambda) * E_0)

##  B) Energy loss via dispersal
 # Basal metabolic rate
   if (locomotion_mode == "flying") {
     BMR = conv_para_list[["a4"]] * m_C^conv_para_list[["b4"]] } else if (locomotion_mode == "running") {
     BMR = conv_para_list[["a5"]] * m_C^conv_para_list[["b5"]] } else if (locomotion_mode == "swimming") {
     BMR = conv_para_list[["a6"]] * m_C^conv_para_list[["b6"]] }


  # Cost of transport
   if (locomotion_mode == "flying") {
     COT = conv_para_list[["a7"]] * m_C^conv_para_list[["b7"]] } else if (locomotion_mode == "running") {
     COT = conv_para_list[["a8"]] * m_C^conv_para_list[["b8"]] } else if (locomotion_mode == "swimming") {
     COT = conv_para_list[["a9"]] * m_C^conv_para_list[["b9"]] }

##  C) Speed
   if(locomotion_mode == "flying") {
     v_0 = conv_para_list[["v_0_fly"]] } else if (locomotion_mode == "running"){
     v_0 = conv_para_list[["v_0_run"]] } else {v_0 = conv_para_list[["v_0_swim"]]}

   c = conv_para_list[["c"]]
   d = conv_para_list[["k"]]
   k = conv_para_list[["d"]]
   v_C = (((1/k)*m_C^c)/((m_C^(c+d)) + (1/(k*v_0))))

# Field metabolic rate while dispersing
  FMR_disp = BMR + COT * v_C

##  OUTPUTS
  #  Calculate time in h
  t = disp_dist/v_C
  #  Calculate total energy cost in J
  E_C = (FMR_disp*t)
  #  Calculate energy cost per meter in J
  E_M = E_C/disp_dist
  #  Calculate energy remaining in J
  E_R = (E_0-E_C)
  #  Calculate relative energy remaining
  E_E = (1-(E_C/E_alpha))

  dsenergy.disp <- cbind(E_0, COT, BMR, v_C, E_C, E_M, E_R, E_E, t) #change this to delete parameters once tested the conversions worked
  return(dsenergy.disp)
}


energy_fun(m_C = 10000, locomotion_mode = "running", disp_dist = 100000, lambda = 10)

# new code
#        E_0      COT      BMR      v_C        E_C      E_M         E_R      E_E        t
# [1,] 20047489 47.79514 56577.79 4.157371 1365682728 13656.83 -1345635238 8.569154 24053.66

# old code
#         E_0      COT      BMR      v_C     E_C      E_M      E_R     E_E        t
# [1,] 20047489 51.21342 56577.79 1822.491 8225763 82.25763 11821727 1.04559 54.86995
