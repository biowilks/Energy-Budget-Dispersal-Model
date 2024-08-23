rm(list=ls())
setwd('~/Energy-Budget-Model/data')

# Load all packages ----------
library(tidyverse)
library(rfishbase)

# 1) FishBase/SeaLifeBase data extraction ----------

# Load all taxa data
fishbase_all <- load_taxa(server = "fishbase")
sealifebase_all <- load_taxa(server = "sealifebase")

# Get species data from FishBase and SeaLifeBase
fishbase <- species(server = "fishbase")
sealifebase <- species(server = "sealifebase")

# Join datasets and filter NA for both
fishbase <- fishbase |>
  left_join(fishbase_all|> select(SpecCode, Species), by = "SpecCode") |>
  select("SpecCode", "Species", "Weight") |>
  filter(!is.na(Weight))


sealifebase <- sealifebase |>
  left_join(sealifebase_all|> select(SpecCode, Species), by = "SpecCode")|>
  select("SpecCode", "Species", "Weight")|>
  filter(!is.na(Weight))

# extract body mass data
write_tsv(fishbase, "FishBase.txt")
write_tsv(sealifebase, "SeaLifeBase.txt")

# 2) Transforming energy-length regressions to energy-weight ----------
# Load data from fish energy storage data provided in (Martin et al. 2017)
energy <- read.csv("EnergyDensityDatebaseDryad.csv")

# Plot energy-length regression
scatter_plot <- ggplot(energy, aes(x = Length.mm.TL, y = EnergyDensity.kJ.g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "log10 length (mm)", y = "log10 Energy Density (kJ/g)") +
  scale_y_log10() +
  scale_x_log10()+
  theme_minimal()

# Make conversions from length (mm) to mass in (g)
# Convert fish length from millimeters to meters and then estimate the mass (in grams)
##  using the formula: Weight = 10 * Length^3 * 1000. This assumes that the fish has
##  a density of 10 g/cm^3 (or 1000 g/m^3).
energy1 <- energy |>
  mutate(Length.m = Length.mm.TL/1000,
         Weight = (10*Length.m^3)*1000)


scatter_plot1 <- ggplot(energy1, aes(x = Weight, y = EnergyDensity.kJ.g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "log10 weight (g)", y = "log10 Energy Density (kJ/g)") +
  scale_y_log10() +
  scale_x_log10()+
  theme_minimal()

# Perform log10 transformations
energy1$log10_Energy <- log10(energy1$EnergyDensity.kJ.g)
energy1$log10_Weight <- log10(energy1$Weight)

# Fit a linea regression model to log-transformed data
regression  <- lm(log10_Energy ~ log10_Weight, data = energy1)
summary(regression)

# Load body mass data from FishBase
bodymass <- read_tsv("FishBase.txt")
bodymass <- bodymass |>
  select(-'SpecCode')

# Combine energy data with FishBase body mass data matching by species name
energy1 <- energy |>
  left_join(bodymass, by = c(`Species` = "Species")) |>
  select(Species,EnergyDensity.kJ.g,Weight,Length.mm.TL) %>% mutate(Length.m = Length.mm.TL/1000)

# For species with missing weight data, estimate the weight using the formula based on length
energy2 <- energy1 |>
    mutate(Weight2 = case_when(is.na(Weight) ~ (10*Length.m^3)*1000,
                               T ~ NA))
# Add in missing weight data when not available
energy3 <- energy2 |>
  mutate(Weight_final = case_when(is.na(Weight) ~ Weight2,
                             T ~ Weight))

# Log10 transform final weight data
energy3$log10_Energy <- log10(energy3$EnergyDensity.kJ.g)
energy3$log10_Weight <- log10(energy3$Weight_final)

# Fit a linear regression to final log-transformed data
regression1  <- lm(log10_Energy ~ log10_Weight, data = energy3)
summary(regression1)
# Results: intercept 0.74, slope -0.025

# Plot the final regression model
scatter_plot3 <- ggplot(energy3, aes(x = log10_Weight, y = log10_Energy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "log10 Weight (g)", y = "log10 Energy Density (kJ/g)") +
  theme_minimal()

