rm(list=ls())
setwd("~/Energy-Budget-Model/output")

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

##  A) SPEED VARIABLES
##     v_C = movement speed in m/h
##     v_0_fly = intercept: locomotion rate of flying
##     v_0_run = intercept: locomotion rate of running
##     v_0_swim = intercept: locomotion rate of swimming
##     c = exponent: locomotion rate
##     k = intercept: heat-dissipation time
##     d = exponent: heat-dissipation time

##  B) ENERGY STORAGE
##     E_0 = Energy storage in J
##     a1 = intercept: fat mass of birds
##     b1 = exponent: fat mass of birds
##     a2 = intercept energy storage of mammals
##     b2 = exponent: fat mass of mammals
##     a3 = intercept: energy density of fish
##     b3 = exponent: energy density of fish
##
##     E_alpha = Energy available for dispersal in J


##  C) ENERGY LOSS
##     BMR = basal metabolic rate in J/h
##     a4 = intercept: BMR of birds
##     b4 = exponent: BMR of birds
##     a5 = intercept: BMR of mammals
##     b5 = exponent: BMR of mammals
##     a6 = intercept: BMR of fish
##     b6 = exponent: BMR of fish

##     LCOT = locomotion costs
##     a7 = intercept:
##     a8 = intercept:
##     a9 = intercept:
##     a10 = intercept:
##     b7 = exponent:
##     b8 = exponent:
##     b9 = exponent:
##     b10 = exponent:

##     FMR_disp = field metabolic rate in J/h


##  3) OUTPUTS
##    t = time in h
##    E_C = total energy cost in J
##    E_M = energy cost per meter in J
##    E_R = energy remaining after moving the disp_dist in J
##    E_E = relative energy efficiency (relative to a species available energy) in J


# Bioenergetic dispersal model energy function ----------
energy_fun <- function(m_C,locomotion_mode,disp_dist,lambda) {


##  C) Speed
  if(locomotion_mode == "flying") {
    v_0 = conv_para_list[["v_0_fly"]] } else if (locomotion_mode == "running"){
      v_0 = conv_para_list[["v_0_run"]] } else {v_0 = conv_para_list[["v_0_swim"]]}

  c = conv_para_list[["c"]]
  d = conv_para_list[["d"]]
  k = conv_para_list[["k"]]
  v_C = (((1/k)*m_C^c)/((m_C^(c+d)) + (1/(k*v_0))))

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

   ## Locomotion costs - change this after without the conversions!!!
   m_C_kg = m_C/1000
   v_C_ms = v_C/3600

  if (locomotion_mode == "flying") {
  LCOT = ((32*m_C_kg^-0.34*v_C_ms^-1 + 0.0033*m_C_kg^-0.34*v_C_ms^2.5 + 0.0058*m_C_kg^-0.51*v_C_ms^2.5)*m_C_kg)*3600 } else if (locomotion_mode == "running") {
  LCOT = ((conv_para_list[["a8"]] * m_C^conv_para_list[["b8"]]) * v_C)} else if (locomotion_mode == "swimming") {
  LCOT = ((( 0.116*exp(1.884*m_C_kg^-0.36 * v_C_ms))*m_C_kg)*3600) }

   # Field metabolic rate while dispersing
   if(locomotion_mode == "flying") {
     FMR_disp = (1.1* BMR) + LCOT } else if (locomotion_mode == "running"){FMR_disp = BMR + LCOT} else {FMR_disp = BMR + LCOT}


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

  dsenergy.disp <- cbind(E_C, E_M, E_R, E_E, t)
  return(dsenergy.disp)
}


energy_fun(m_C = 10000, locomotion_mode = "running", disp_dist = 100000, lambda = 0.1)
