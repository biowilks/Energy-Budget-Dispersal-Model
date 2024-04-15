rm(list=ls())

library(rstudioapi)
library(tidyverse)
library(cowplot)
library(viridis)

setwd(dirname(getActiveDocumentContext()$path))

## 1) INPUT VARIABLES
## m_C = body mass in g
## movement_mode = running, flying or swimming
## lambda = % of energy storage needed for survival after dispersing

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



disp_fun <- function(m_C,movement_mode,lambda) {
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
#  Given in m/s, converted to m/h, body mass in kg

 
  
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
#                   Given in mg o2/h, body mass in kg, converted to mL o2/h and then to J/h
#                   Conversion factor taken from Peters. (1986)
  
                                 
   if(movement_mode == "running") {
    COT = 10.7*m_C_kg^0.68 } else if (movement_mode == "flying"){
      COT = 3.6*m_C_kg^0.69 } else {COT = 1.1*m_C_kg^0.62}
  
#  Parameter reference:
#  All movement modes from Alexander. (2003) in J/m, body mass in kg
   
   
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
   t = ((1-lambda) * E_0 )/ FMR_disp

## Calculate dispersal distance in m  
  disp_dist = (t*v_C) 
  ds.disp <- cbind(m_C_kg, E_0, v_C, t, FMR_disp, disp_dist, BMR, COT,lambda)
  return(ds.disp)
}


disp_fun(m_C = 20000,movement_mode = "running", lambda = 0.1)


### Importing empirical data
dispdata <- read.csv("DispersalTransformed.csv")

##  Filtering data for maximum dispersal distance data for running mammals, flying birds and swimming fish
#   Removing NA, NaN and Inf values
dispdatamax <- dispdata |>
  filter(Movement.Mode != "Mixed" & Family_gbif != "Delphinidae" & Family_gbif != "Physeteridae"&
           ((Class_gbif == "Mammalia" & Movement.Mode == "Running") |
              (Class_gbif == "Aves" & Movement.Mode == "Flying") | Movement.Mode == "Swimming")) |>
  filter(Statistic == "Maximum") |>
  filter(!is.na(Value) & !is.infinite(Value) & !is.nan(Value),
         !is.na(Body.mass) & !is.infinite(Body.mass) & !is.nan(Body.mass),
         !is.na(Movement.Mode) )

##filtering for each movement mode
run_max<- dispdatamax|> filter (Movement.Mode == "Running") |> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)
fly_max <- dispdatamax |> filter (Movement.Mode == "Flying")|> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)
swim_max <- dispdatamax |> filter (Movement.Mode == "Swimming")|> select(Species_ID_gbif,Value,Body.mass,Movement.Mode)

run_mass_max <- run_max$Body.mass
fly_mass_max <- fly_max$Body.mass
swim_mass_max <- swim_max$Body.mass

# Calculate dispersal distances across body mass gradient (mass values based on empirical data available)
# For each movement mode

ma  disp = as.data.frame(disp_fun(mass_max,movement_mode = "running",lambda = 0.1))



ds.dispfly  <- data.frame()

for(m_C in seq(3.1,11800
               , length = 100)) {
  disp = as.data.frame(disp_fun(m_C,movement_mode = "flying",lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.dispfly = rbind(ds.dispfly, mass_disp)
}

ds.dispswim  <- data.frame()

for(m_C in seq(220, 550000, length = 100)) {
  disp = as.data.frame(disp_fun(m_C,movement_mode = "swimming",lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.dispswim = rbind(ds.dispswim, mass_disp)
}

# Calculate dispersal distances across body mass gradient (mass values based on empirical data available)
# For each movement mode
ds.disprun  <- data.frame()

for(m_C in seq(7.6,3940034.28
, length = 100)) {
  disp = as.data.frame(disp_fun(m_C,movement_mode = "running",lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.disprun = rbind(ds.disprun, mass_disp)
}

ds.dispfly  <- data.frame()

for(m_C in seq(3.1,11800
, length = 100)) {
  disp = as.data.frame(disp_fun(m_C,movement_mode = "flying",lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.dispfly = rbind(ds.dispfly, mass_disp)
}

ds.dispswim  <- data.frame()

for(m_C in seq(220, 550000, length = 100)) {
  disp = as.data.frame(disp_fun(m_C,movement_mode = "swimming",lambda = 0.1))
  mass_disp = cbind(m_C, disp)
  ds.dispswim = rbind(ds.dispswim, mass_disp)
}

# Plot dispersal distance against body mass
ggplot(ds.disprun, aes(x=m_C, y = disp_dist)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_minimal() +
  geom_line(data = ds.disprun,aes(x=m_C, y = disp_dist),color = "chartreuse4",linewidth=1)+
  geom_line(data = ds.dispfly,aes(x=m_C, y = disp_dist),color = "red",linewidth=1)+
  geom_line(data = ds.dispswim,aes(x=m_C, y = disp_dist),color = "blue",linewidth=1)+
  labs(y= "Dispersal distance (m)", x = "Body size (g)")

# Model summary for each movement mode
model.disprun <- lm(log10(disp_dist) ~ log10(m_C), data = ds.disprun)
summary(model.disprun)

model.dispfly <- lm(log10(disp_dist) ~ log10(m_C), data = ds.dispfly)
summary(model.dispfly)

model.dispswim <- lm(log10(disp_dist) ~ log10(m_C), data = ds.dispswim)
summary(model.dispswim)


## Plot parameters against body mass 
#custom label needed for speed graph
custom_labels <- c(expression(10^2.5), expression(10^3), expression(10^3.5), expression(10^4), expression(10^4.5))
custom_breaks <- c(10^2.5, 10^3, 10^3.5, 10^4, 10^4.5)

vC_plot <- ggplot(ds.disprun, aes(x = m_C, y = v_C)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(breaks = custom_breaks, labels = custom_labels, trans = "log10") +
  theme_minimal() +
  geom_line(data = ds.disprun, aes(x = m_C, y = v_C), color = "chartreuse4", linewidth = 1) +
  geom_line(data = ds.dispfly, aes(x = m_C, y = v_C), color = "red", linewidth = 1) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = v_C), color = "blue", linewidth = 1) +
  labs(y = "Movement speed (m/h)", x = "Body size (g)") +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))


BMR_plot <- ggplot(ds.disprun, aes(x = m_C, y = BMR, color = viridis(3)[3]),  linewidth = 1) +
  geom_line() +
  geom_line(data = ds.dispfly, aes(x = m_C, y = BMR, color = viridis(3)[2]), linewidth = 1) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = BMR, color = viridis(3)[1]),  linewidth = 1) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "Basal metabolic rate (J/h)",
    x = "Body size (g)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

BMR_plot

COT_plot <- ggplot(ds.disprun, aes(x=m_C, y = COT)) +
  geom_line() +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_minimal() +
  geom_line(data = ds.disprun,aes(x=m_C, y = COT),color = "chartreuse4",linewidth=1)+
  geom_line(data = ds.dispfly,aes(x=m_C, y = COT),color = "red",linewidth=1)+
  geom_line(data = ds.dispswim,aes(x=m_C, y = COT),color = "blue",linewidth=1)+
  labs(y= "Cost of transport (J/m)", x = "Body size (g)")+
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"))

E0_plot <- ggplot(ds.disprun, aes(x = m_C, y = E_0, color = viridis(3)[3]), linewidth = 1) +
  geom_line() +
  geom_line(data = ds.dispfly, aes(x = m_C, y = E_0, color = viridis(3)[2]), linewidth = 1) +
  geom_line(data = ds.dispswim, aes(x = m_C, y = E_0, color = viridis(3)[1]), linewidth = 1) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(
    y = "Basal metabolic rate (J/h)",
    x = "Body size (g)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25),
    title = element_text(size = 25, face = "bold"),
    legend.position = "none"
  )

E0_plot

plot_grid(E0_plot, BMR_plot, labels = c('a','b'))
plot_grid(COT_plot,vC_plot, labels = c('c','d'))

plot_grid(E0_plot, BMR_plot, COT_plot,vC_plot, labels = c('a','b','c','d'))

# Export model predictions
run <- ds.disprun |>
        select(m_C,disp_dist)
write_csv(run,"rundispdistance.csv")

fly <- ds.dispfly |>
  select(m_C,disp_dist)
write_csv(fly,"flydispdistance.csv")

swim <- ds.dispswim |>
  select(m_C,disp_dist)
write_csv(swim,"swimdispdistance.csv")





