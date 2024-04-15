library(tidyverse)
library(dplyr)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

energy <- read.csv("EnergyDensityDatebaseDryad.csv")


scatter_plot <- ggplot(energy, aes(x = Length.mm.TL, y = EnergyDensity.kJ.g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "log10 length (mm)", y = "log10 Energy Density (kJ/g)") +
  scale_y_log10() +
  scale_x_log10()+
  theme_minimal()

print(scatter_plot)

energy1 <- energy |>
  mutate(Length.m = Length.mm.TL/1000,
         Weight = (10*Length.m^3)*1000)


scatter_plot <- ggplot(energy1, aes(x = Weight, y = EnergyDensity.kJ.g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "log10 weight (g)", y = "log10 Energy Density (kJ/g)") +
  scale_y_log10() +
  scale_x_log10()+
  theme_minimal()

print(scatter_plot)

energy1$log10_Energy <- log10(energy1$EnergyDensity.kJ.g)
energy1$log10_Weight <- log10(energy1$Weight)


regression  <- lm(log10_Energy ~ log10_Weight, data = energy1)
summary(regression)


bodymass <- read_tsv("FishBase.txt")
bodymass <- bodymass |>
  select(-'SpecCode')

energy1 <- energy |>
  left_join(bodymass, by = c(`Species` = "Species")) |>
  select(Species,EnergyDensity.kJ.g,Weight,Length.mm.TL) %>% mutate(Length.m = Length.mm.TL/1000)

energy2 <- energy1 |>
    mutate(Weight2 = case_when(is.na(Weight) ~ (10*Length.m^3)*1000,
                               T ~ NA)) 

energy3 <- energy2 |>
  mutate(Weight_final = case_when(is.na(Weight) ~ Weight2,
                             T ~ Weight)) 

# Log10 transform
energy3$log10_Energy <- log10(energy3$EnergyDensity.kJ.g)
energy3$log10_Weight <- log10(energy3$Weight_final)


regression  <- lm(log10_Energy ~ log10_Weight, data = energy3)
summary(regression)
#Intercept 0.74, slope -0.025

scatter_plot <- ggplot(energy3, aes(x = log10_Weight, y = log10_Energy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "log10 Weight (g)", y = "log10 Energy Density (kJ/g)") +
  theme_minimal()

print(scatter_plot)

#number of datapoints
energy1 |> 
  pull(`Species`) |>
  n_distinct()
