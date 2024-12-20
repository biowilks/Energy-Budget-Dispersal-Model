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
## m_C = body mass in kg
## locomotion_mode = flying, running or swimming
## lambda = % of energy storage needed for survival after dispersing

## 2) PARAMETERS

##  A) ENERGY STORAGE
##     E_0 = Energy storage in J
##     a1 = intercept: energy storage  of birds
##     b1 = exponent: energy storage  of birds
##     a2 = intercept energy storage of mammals
##     b2 = exponent: energy storage of mammals
##     a3 = intercept: energy storage of fish
##     b3 = exponent: energy storage of fish
##
##     E_alpha = Energy available for dispersal in J


##  B) ENERGY LOSS
##     BMR = basal metabolic rate in J/s
##     a4 = intercept: BMR of birds
##     b4 = exponent: BMR of birds
##     a5 = intercept: BMR of mammals
##     b5 = exponent: BMR of mammals
##     a6 = intercept: BMR of fish
##     b6 = exponent: BMR of fish

##     v_C = movement speed in m/s
##     v_0_fly = intercept: locomotion rate of flying
##     v_0_run = intercept: locomotion rate of running
##     v_0_swim = intercept: locomotion rate of swimming
##     c = exponent: locomotion rate
##     k = intercept: heat-dissipation time
##     d = exponent: heat-dissipation time

##     LCOT = locomotion costs
##     a7, a8, a9  = intercept: LCOT of flying
##     b7, b8, b9, b10 = exponent: LCOT of flying
##     a10 = intercept: LCOT of running
##     b11 = exponent: LCOT of running
##     a11, a12 = intercept: LCOT of swimming
##     b12 = exponent: LCOT of swimming

##     FMR_disp = field metabolic rate in J/s
##     a13 = intercept: postural cost of flying
##     a14 = intercept: postural cost of running

##  3) OUTPUTS
##    t_2 = time in s
##    disp_dist = maximum dispersal distance in m




# Bioenergetic dispersal model dispersal function ----------

## 1) INPUT VARIABLES
disp_fun <- function(m_C,locomotion_mode,lambda) {

## 2) PARAMETERS

## A) Energy storage
  if (locomotion_mode == "flying") {
    E_0 = conv_para_list[["a1"]] * m_C^conv_para_list[["b1"]] } else if (locomotion_mode == "running") {
      E_0 = conv_para_list[["a2"]] * m_C^conv_para_list[["b2"]] } else if (locomotion_mode == "swimming") {
        E_0 = conv_para_list[["a3"]] * m_C^conv_para_list[["b3"]] }

## Energy available for dispersal
  E_alpha = ((1-lambda) * E_0)


## B) Energy loss via dispersal

## Basal metabolic rate
  if (locomotion_mode == "flying") {
    BMR = conv_para_list[["a4"]] * m_C^conv_para_list[["b4"]] } else if (locomotion_mode == "running") {
      BMR = conv_para_list[["a5"]] * m_C^conv_para_list[["b5"]] } else if (locomotion_mode == "swimming") {
        BMR = conv_para_list[["a6"]] * m_C^conv_para_list[["b6"]] }

## Speed
  if(locomotion_mode == "flying") {
    v_0 = conv_para_list[["v_0_fly"]] } else if (locomotion_mode == "running"){
      v_0 = conv_para_list[["v_0_run"]] } else {v_0 = conv_para_list[["v_0_swim"]]}

  c = conv_para_list[["c"]]
  d = conv_para_list[["d"]]
  k = conv_para_list[["k"]]
  v_C = (((1/k)*m_C^c)/((m_C^(c+d)) + (1/(k*v_0))))

## Locomotion costs
  if (locomotion_mode == "flying") {
    LCOT = conv_para_list[["a7"]]*m_C^conv_para_list[["b7"]]*v_C^conv_para_list[["b8"]] + conv_para_list[["a8"]]*m_C^conv_para_list[["b7"]]*v_C^conv_para_list[["b9"]] + conv_para_list[["a9"]]*m_C^conv_para_list[["b10"]]*v_C^conv_para_list[["b9"]]}
  else if (locomotion_mode == "running") {LCOT = conv_para_list[["a10"]] * m_C^conv_para_list[["b11"]] * v_C}
  else if (locomotion_mode == "swimming") {LCOT = m_C*(conv_para_list[["a11"]]*exp(conv_para_list[["a12"]]*m_C^conv_para_list[["b12"]] * v_C))}

## Field metabolic rate while dispersing
  if(locomotion_mode == "flying") {
    FMR_disp = conv_para_list[["a13"]]* BMR + LCOT } else if (locomotion_mode == "running")
    {FMR_disp = conv_para_list[["a14"]]* BMR + LCOT} else {FMR_disp = BMR + LCOT}

## 3) OUTPUTS
## t_2 = time in s
  t_2 = E_alpha/ FMR_disp

## disp_dist = dispersal distance in m
  disp_dist = t_2*v_C


  ds.disp <- cbind(disp_dist, t_2, E_0, BMR, v_C, LCOT)
  return(ds.disp)
}


disp_fun(m_C = 2, locomotion_mode = "swimming", lambda = 0.1)

