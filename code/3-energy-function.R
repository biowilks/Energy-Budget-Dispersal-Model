rm(list=ls())

library("rstudioapi")
library("tidyverse")
library("cowplot")
library("ggbreak")
library("scales")

setwd(dirname(getActiveDocumentContext()$path))

## 1) INPUT VARIABLES
## m_C = body mass in g
## movement_mode = running, flying or swimming
## disp_dist = dispersal distance in m  

## 2) OTHER VARIABLES
##  A) BODY MASS CONVERSION
##     m_C_kg = body mass in kg
##  B) SPEED VARIABLES
##     v_C = movement speed in m/h
##  C) ENERGY LOSS
##     BMR = basal metabolic rate in J/h
##     COT = costs of transport in J/m
##     FMR_disp = field metabolic rate in J/h
##  D) ENERGY STORAGE
##     E_0 = Energy storage in J



energy_fun <- function(m_C,movement_mode,disp_dist) {
  ## A) Body mass conversion
  m_C_kg = m_C/1000
  
  ## B) Speed variables (Dyer et al. 2023)
  if(movement_mode == "running") {
    v_0 = 0.28 } else if (movement_mode == "flying"){
      v_0 = 30.54} else { v_0 = 0.39}
  c = 0.27
  d = 0.24
  k = 0.033
  v_C = (((1/k)*m_C_kg^c)/((m_C_kg^(c+d)) + (1/(k*v_0))))*3600 
  
  #  Parameter reference: 
  #  All movement modes from Dyer et al. (2023)
  #  Given in m/s, converted to m/h
  
  
  ## C) Energy loss via dispersal
  if(movement_mode == "running") {
    BMR = (3.248*m_C^0.735)*20 } else if (movement_mode == "flying"){
      BMR = (7.434*m_C^0.648)*20 } else {BMR = (((10^1.87)*m_C_kg^0.95)/1.33)*20 }
  
  #  Parameter reference:   
  #  Mammal (running) and 
  #  Bird (flying)    BMR from (Gavrilov et al. 2022) 
  #                   Given in mL o2/h, body mass in g, converted to J/h
  #                   Conversion factor taken from Peters. (1986)  
  
  #  Fish (swimming) BMR from (Watanabe & Payne. 2023) 
  #                   Given in mg o2/h, body mass in kg, to mL o2/h and then to J/h
  #                   Conversion factor taken from Peters. (1986)
  
  
  if(movement_mode == "running") {
    COT = 10.7*m_C_kg^0.68 } else if (movement_mode == "flying"){
      COT = 3.6*m_C_kg^0.69 } else {COT = 1.1*m_C_kg^0.62}
  
  #  Parameter reference:
  #  All movement modes from Alexander. (2003) in J/m
  
  
  FMR_disp = BMR + COT*v_C
  

  ## D) Energy storage
  if(movement_mode == "running") {
    E_0 =  (((10^-1.30)*m_C^1)/1000)*40*10^6  } else if (movement_mode == "flying"){
      E_0 =  (((10^-1.20)*m_C^0.98)/1000)*40*10^6 } else {E_0 = ((10^0.62)*m_C^0.02)*m_C*1000 }
  
  # Parameter reference:
  # Mammal (running) and
  # Bird (flying)   E_0 from Antol & Kozlowski. (2020) 
  #                 Gives energy in log10 g converted to kg and then J 
  #                 Conversion factor from Peters. (1986)
  
  # Fish (swimming) E_0 calculated using data from Martin et al. (2018) 
  #                 Refitted energy density/length data, converting length to mass 
  #                 Gives energy density in kJ/g converted to J
  #                 Conversion factor from Webb. (1975) in Peters. (1986) 
  
  
  
  ## Calculate time in h
  t = disp_dist/v_C
  
  ## Calculate total energy cost in J
  E_C = (t*FMR_disp)
  ## Calculate energy remaining after moving the disp dist in J
  E_R = (E_0-E_C)
  dsenergy.disp <- cbind(m_C_kg, E_C, E_R, t)
  return(dsenergy.disp)
}


energy_fun(m_C = 4000000, movement_mode = "running", disp_dist = 161747.88)

# Calculate energy  across different distances (for ~ 2kg  individual)
# For each movement mode
ds.energyrun  <- data.frame()

for(disp_dist in seq(0,5000000
               , length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 2000,movement_mode = "running", disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrun = rbind(ds.energyrun, mass_disp)
}

ds.energyfly  <- data.frame()

for(disp_dist in seq(0,5000000
               , length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 2000,movement_mode = "flying",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyfly = rbind(ds.energyfly, mass_disp)
}

ds.energyswim  <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 2000,movement_mode = "swimming",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyswim = rbind(ds.energyswim, mass_disp)
}

##plotting energy remaining against distance for different movement modes
energy <- ggplot(ds.energyrun, aes(x = disp_dist, y = E_R)) +
  geom_line(color = "chartreuse4", linewidth = 1) +
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma, limits=c(0, 10000000))+  
  theme_minimal() +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_line(data = ds.energyfly, aes(x = disp_dist, y = E_R), color = "red", linewidth = 1) +
  geom_line(data = ds.energyswim, aes(x = disp_dist, y = E_R), color = "blue", linewidth = 1) +
  labs(y = "Energy remaining (J)", x = "Distance (m)")+
  ggtitle("") +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))

energy


# Calculate energy  across different distances for running mammals
# For different body masses
ds.energyrunsmall  <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 45000,movement_mode = "running",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrunsmall = rbind(ds.energyrunsmall, mass_disp)
}

ds.energyrunlarge <- data.frame()

for(disp_dist in seq(0,5000000, length = 10000)) {
  disp = as.data.frame(energy_fun(m_C = 4000000,movement_mode = "running",disp_dist))
  mass_disp = cbind(disp_dist, disp)
  ds.energyrunlarge = rbind(ds.energyrunlarge, mass_disp)
}

# Filter out negative values from both datasets
ds.energyrunsmall_filtered <- ds.energyrunsmall[ds.energyrunsmall$E_R >= 0, ]
ds.energyrunlarge_filtered <- ds.energyrunlarge[ds.energyrunlarge$E_R >= 0, ]

# Define breaks for the y-axis (using only the maximum value of the small dataset)
breaks <- max(ds.energyrunsmall_filtered$E_R)

# Define the range for the y-axis with extra space
y_range <- c(breaks, max(ds.energyrunlarge_filtered$E_R)) 

p <- ggplot() +
  geom_line(data = ds.energyrunsmall_filtered, aes(x = disp_dist, y = E_R), color = "#a4cc7dff", size = 1) +
  geom_line(data = ds.energyrunlarge_filtered, aes(x = disp_dist, y = E_R), color = "#4c900aff", size = 1) +
  geom_hline(yintercept = breaks, color = "white", size = 1) +  
  geom_hline(yintercept = 90000000, linetype = "dashed", color = "black", size = 0.1) +   
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 2400000, by = 200000)) +
  scale_y_continuous(labels = scales::comma) +
  scale_y_cut(breaks = breaks, space = 0) +
  coord_cartesian(ylim = y_range) + 
  theme_minimal() +
  labs(y = "Energy remaining (J)", x = "Distance (m)")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))


p
