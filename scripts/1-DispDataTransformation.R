rm(list=ls())
library(tidyverse)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

db <- read.csv("DispersalUntransformed.csv")

db1 <- db |>
  filter(Units != "km/h", Units != "ha") |>
  filter(
    !is.na(Value), !is.na(Body.mass))
  

####Converting units for body mass and distance####
#converting units to meters
db2 <- db1 |>
  mutate(Value = case_when(Units == "km"  ~ Value * 1000, 
                           Units == "miles"  ~ Value * 1609.34,
                           Units == "m"~ Value,
                           TRUE ~ NA))

db3 <- db2 |>
  mutate(Units = "m")

 
#converting kg to grams for body masses
db4 <- db3 |>
    mutate(Body.mass = case_when(Body.mass.Units == "kg"  ~ Body.mass * 1000,  TRUE ~ Body.mass))

db5 <- db4 |>
  mutate(Body.mass.Units = "g")

#Method checking
db6 <- db5 |>
  group_by(Class_gbif == "Aves") %>% mutate(New.Sampling.Method = case_when(is.na(New.Sampling.Method) &&
                              Sampling.method %in% c("Radio-tracking", "Radio tracking", "Satellite tracking") ~ "Tracking",
                              is.na(New.Sampling.Method) &&
                              Sampling.method %in% c("Mark-Recapture", "Banding shot", "Mark recapture", "Banding recapture", "Ringing", "Colour marking","Wing tagging","Trapping") ~ "Mark-recapture",
                              is.na(New.Sampling.Method) &&
                              Sampling.method %in% c("Nest box observation", "Observation") ~ "Direct observation",
                              is.na(New.Sampling.Method) &&
                                Sampling.method %in% c("Radio tracking/banding") ~ "Mixed", 
                              !is.na(New.Sampling.Method) ~ New.Sampling.Method,
                              TRUE ~ "Unknown")) %>% ungroup()

db7 <- db6 |>
  group_by(is.na(Class_gbif)) %>% mutate(New.Sampling.Method = case_when(Sampling.method %in% c("Radio-tracking") ~ "Tracking",
                                         Sampling.method %in% c("Mark-Recapture") ~ "Mark-recapture", 
                                         T ~  "Unknown")) %>% ungroup()


write_csv(db7, "DispersalFull.csv")

##Making combined values for McCaslin and Swift - as many maximum values for one species
# For McCaslin et al. 2020
dbcombmc <- db7 %>%
  filter(Reference == "McCaslin et al. 2020") %>%
  group_by(Species_ID_gbif) %>%
  summarise(
    Value_mean = mean(Value, na.rm = TRUE),
    Source = "McCaslin et al. 2020",
    across(everything(), ~ first(.)),
    .groups = 'drop'
  ) %>%
  distinct()

# For Swift et al. 2021
dbcombs <- db7 %>%
  filter(Reference == "Swift et al. 2021") %>%
  group_by(Species_ID_gbif) %>%
  summarise(
    Value_mean = mean(Value, na.rm = TRUE),
    Source = "Swift et al. 2021",
    across(everything(), ~ first(.)),
    .groups = 'drop'
  ) %>%
  distinct()

# Combine the results
combfinal <- bind_rows(dbcombs, dbcombmc)

# Replace the original data for McCaslin et al. 2020 and Swift et al. 2021 with summarised values
db8 <- db7 %>%
  filter(!Reference %in% c("McCaslin et al. 2020", "Swift et al. 2021")) %>%
  bind_rows(
    db7 %>%
      filter(Reference == "McCaslin et al. 2020") %>%
      group_by(Species_ID_gbif) %>%
      summarise(
        Value = mean(Value, na.rm = TRUE),
        Source = "McCaslin et al. 2020",
        across(everything(), ~ first(.)),
        .groups = 'drop'
      ) %>%
      distinct(),
    db7 %>%
      filter(Reference == "Swift et al. 2021") %>%
      group_by(Species_ID_gbif) %>%
      summarise(
        Value = mean(Value, na.rm = TRUE),
        Source = "Swift et al. 2021",
        across(everything(), ~ first(.)),
        .groups = 'drop'
      ) %>%
      distinct()
  ) %>%
  distinct()

write_csv(db8, "DispersalTransformed.csv")
