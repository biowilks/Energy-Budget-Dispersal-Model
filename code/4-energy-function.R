rm(list=ls())

library("rstudioapi")
library("tidyverse")

setwd(dirname(getActiveDocumentContext()$path))
source("3-parameter-conversions.R")

## 1) INPUT VARIABLES
## m_C = body mass in g
## movement_mode = running, flying or swimming
## disp_dist = dispersal distance in m
## lambda = % of energy storage needed for survival after dispersing

## 2) OTHER VARIABLES
##  A) ENERGY STORAGE
##     E_0 = Energy storage in J
##     E0_(taxonomic group)[[1]] = converted norm constant
##     E0_(taxonomic group)[[2]]] = allometric exponent

##     E_alpha = Energy available for dispersal in J


##  B) ENERGY LOSS
##     BMR = basal metabolic rate in J/h
##     BMR_(taxonomic group)[[1]] = converted norm constant
##     BMR_(taxonomic group)[[2]]] = allometric exponent

##     COT = costs of transport in J/m
##     COT_(locomotion mode)[[1]] = converted norm constant
##     COT_(locomotion mode)[[2]]] = allometric exponent

##     FMR_disp = field metabolic rate in J/h

##  C) SPEED VARIABLES
##     v_C = movement speed in m/h

##  3) OUTPUTS
##    t = time in h
##    E_C = total energy cost in J
##    E_M = energy cost per meter in J
##    E_R = energy remaining after moving the disp_dist in J
##    E_E = relative energy efficiency (relative to a species available energy) in J





energy_fun <- function(m_C,movement_mode,disp_dist,lambda) {

##  A) Energy storage: Mammals and birds (Antol & Kozlowski. 2020)), fish (Martin et al. 2017)
  if(movement_mode == "running") {
    E_0 =  E0_mammals[[1]]*m_C^E0_mammals[[2]]   } else if (movement_mode == "flying"){
      E_0 =  E0_birds[[1]]*m_C^E0_birds[[2]]  } else {E0_fish[[1]]*m_C^E0_fish[[2]]}

  # Energy available for dispersal
   E_alpha = ((1-lambda) * E_0)

##  B) Energy loss via dispersal
 # Basal metabolic rate: Mammals and birds (Gavrilov et al. 2022), fish (Watanabe & Payne. 2023)
   if(movement_mode == "running") {
     BMR =  BMR_mammals[[1]]*m_C^BMR_mammals[[2]]   } else if (movement_mode == "flying"){
       BMR =  BMR_birds[[1]]*m_C^BMR_birds[[2]]  } else {BMR_fish[[1]]*m_C^BMR_fish[[2]]}


  # Cost of transport: Running, flying and swimming (Alexander. 2003)
   if(movement_mode == "running") {
     COT =  COT_running[[1]]*m_C^COT_running[[2]]   } else if (movement_mode == "flying"){
       COT =  COT_flying[[1]]*m_C^COT_flying[[2]]  } else {COT_swimming[[1]]*m_C^COT_swimming[[2]]}

##  C) Speed: Running, flying and swimming (Dyer et al. 2023)
   if(movement_mode == "running") {
     v_0 = 0.28 } else if (movement_mode == "flying"){
       v_0 = 30.54} else { v_0 = 0.39}
   c = 0.27
   d = 0.24
   k = 0.033
   v_C = (((1/k)*m_C^c)/((m_C^(c+d)) + (1/(k*v_0))))*3600

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


energy_fun(m_C = 10000, movement_mode = "running", disp_dist = 161747.88, lambda = 0)
