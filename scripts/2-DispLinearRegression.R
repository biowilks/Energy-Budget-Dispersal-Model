rm(list=ls())

library(tidyverse)
library(dplyr)
library(nlme)
library(sjPlot)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

## Importing empirical data
dispdata <- read.csv("DispersalTransformed.csv")

##  Filtering data for maximum dispersal distance data for running mammals, flying birds and swimming fish
#   log10 conversion for dispersal distance and body mass
#   Removing NA, NaN and Inf values
dispdatamax <- dispdata |>
  filter(Movement.Mode != "Mixed" &
        ((Class_gbif == "Mammalia" & Movement.Mode == "Running") |
          (Class_gbif == "Aves" & Movement.Mode == "Flying") | Movement.Mode == "Swimming")) |>
  filter(Statistic == "Maximum") |>
  mutate(log10_Body.mass = log10(Body.mass),log10_Distance = log10(Value)) |>
  filter(!is.na(log10_Distance) & !is.infinite(log10_Distance) & !is.nan(log10_Distance),
         !is.na(log10_Body.mass) & !is.infinite(log10_Body.mass) & !is.nan(log10_Body.mass),
         !is.na(Movement.Mode) )

run_max<- dispdatamax|> filter (Movement.Mode == "Running")
fly_max <- dispdatamax |> filter (Movement.Mode == "Flying")
swim_max <- dispdatamax |> filter (Movement.Mode == "Swimming")

## Importing energy-budget dispersal model predictions
#  log10 conversion for dispersal distance and body mass 
modelrun <- read.csv("rundispdistance.csv") |>
  mutate(log10_Body.mass = log10(m_C), log10_Distance = log10(disp_dist),
         Movement.Mode = "Running")

modelfly <- read.csv("flydispdistance.csv") |>
  mutate(log10_Body.mass = log10(m_C), log10_Distance = log10(disp_dist),
         Movement.Mode = "Flying")

modelswim <- read.csv("swimdispdistance.csv") |>
  mutate(log10_Body.mass = log10(m_C), log10_Distance = log10(disp_dist),
         Movement.Mode = "Swimming")
# Combine the model datasets
modelcombined <- rbind(modelfly,modelrun, modelswim)

## Statistical model - empirical
dispdatamax$Movement.Mode <- as.factor(dispdatamax$Movement.Mode)
dispdatamax$New.Sampling.Method <- as.factor(dispdatamax$New.Sampling.Method)

# Mixed-effects linear regression 
model.disp_lme <- lme(log10_Distance ~ Movement.Mode/(log10_Body.mass+1) ,
                  random = ~1|New.Sampling.Method,
                  data = dispdatamax)

summary(model.disp_lme)

# Linear regression
model.disp_lm <- lm(log10_Distance ~ log10_Body.mass*Movement.Mode ,
                  random = ~1|New.Sampling.Method, #can't be used in lm - argument is disregarded
                  data = dispdatamax)
#model.disp_lm <- lm(log10_Distance ~ log10_Body.mass * Movement.Mode + New.Sampling.Method, 
#                 data = dispdatamax)

summary(model.disp_lm)
plot_model(model.disp_lm, type = "pred", terms = c("log10_Body.mass", "Movement.Mode"))

## Statistical model - energy budget model
energybudget_lm <- lm(log10_Distance ~ log10_Body.mass*Movement.Mode ,
                      data = modelcombined)
summary(energybudget_lm)

## Plot both empirical and energy budget maximum dispersal distances
empiricalplot <- plot_model(model.disp_lme, type = "pred", terms = c("log10_Body.mass", "Movement.Mode"))+
                labs(title = "Empirical maximum dispersal distance", x = "log10 Body mass [g]", y = "log10 Dispersal distance [m]") +
                scale_y_continuous(limits = c(2, 8)) +
                scale_color_discrete(name = "Movement Mode")
print(empiricalplot)

energybudget_plot <- plot_model(energybudget_lm, type = "pred", terms = c("log10_Body.mass", "Movement.Mode")) +
  labs(title = "Predicted maximum dispersal distance", x = "log10 Body mass [g]", y = "log10 Dispersal distance [m]") +
  scale_y_continuous(limits = c(2, 8)) +
  scale_color_discrete(name = "Movement Mode")
print(energybudget_plot)

#theme(legend.position = "none") - removes legend

## Migration and dispersal all data (mean, median and maximum)
dispmigrdata <-read.csv("Combined.csv")

dispmigrdata <- dispmigrdata |>
  filter(Movement.Mode != "Mixed" &
           ((Class_gbif == "Mammalia" & Movement.Mode == "Running") |
              (Class_gbif == "Aves" & Movement.Mode == "Flying") | Movement.Mode == "Swimming")) |>
  mutate(log10_Body.mass = log10(Body.mass),log10_Distance = log10(Value)) |>
  filter(!is.na(log10_Distance) & !is.infinite(log10_Distance) & !is.nan(log10_Distance),
         !is.na(log10_Body.mass) & !is.infinite(log10_Body.mass) & !is.nan(log10_Body.mass),
         !is.na(Movement.Mode) )

run_migr<- dispmigrdata |> filter (Movement.Mode == "Running")
fly_migr <- dispmigrdata |> filter (Movement.Mode == "Flying")
swim_migr <- dispmigrdata |> filter (Movement.Mode == "Swimming")

# Statistical model 
dispmigrdata$Movement.Mode <- as.factor(dispmigrdata$Movement.Mode)
dispmigrdata$New.Sampling.Method <- as.factor(dispmigrdata$New.Sampling.Method)

# Mixed-effects linear regression 
model.dispmigr_lm <- lm(log10_Distance ~ log10_Body.mass*Movement.Mode ,
                       data = dispmigrdata)

summary(model.dispmigr_lm)

empiricaldispmigrplot <- plot_model(model.dispmigr_lm, type = "pred", terms = c("log10_Body.mass", "Movement.Mode"))+
  labs(title = "Empirical dispersal and migration distance", x = "log10 Body mass [g]", y = "log10 Dispersal distance [m]") +
  scale_y_continuous(limits = c(2, 8)) +
  scale_color_discrete(name = "Movement Mode")
print(empiricaldispmigrplot)

