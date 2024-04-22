rm(list=ls())

library(tidyverse)

setwd("C:/Users/xr49abiw/Documents/Energy-Budget-Model/data")
db <- read.csv("DispersalUntransformed.csv")

## Filtering data to include only maximum dispersal distance
db1 <- db |>
  filter( Statistic == "Maximum")

## Making combined values for McCaslin and Swift references - selecting the highest value for one species
# For McCaslin et al. 2020
dbcombmc <- db1 %>%
  filter(Reference == "McCaslin et al. 2020") %>%
  group_by(Species_ID_gbif) %>%
  summarise(
    Value_max = max(Value, na.rm = TRUE),
    Source = "McCaslin et al. 2020",
    across(everything(), ~ first(.)),
    .groups = 'drop'
  ) %>%
  distinct()

# For Swift et al. 2021
dbcombs <- db1 %>%
  filter(Reference == "Swift et al. 2021") %>%
  group_by(Species_ID_gbif) %>%
  summarise(
    Value_max = max(Value, na.rm = TRUE),
    Source = "Swift et al. 2021",
    across(everything(), ~ first(.)),
    .groups = 'drop'
  ) %>%
  distinct()

# Combine the results
combfinal <- bind_rows(dbcombs, dbcombmc)

# Replace the original data for McCaslin et al. 2020 and Swift et al. 2021 with just the maximum value
db2 <- db1 %>%
  filter(!Reference %in% c("McCaslin et al. 2020", "Swift et al. 2021")) %>%
  bind_rows(
    db1 %>%
      filter(Reference == "McCaslin et al. 2020") %>%
      group_by(Species_ID_gbif) %>%
      summarise(
        Value = max(Value, na.rm = TRUE),
        Source = "McCaslin et al. 2020",
        across(everything(), ~ first(.)),
        .groups = 'drop'
      ) %>%
      distinct(),
    db1 %>%
      filter(Reference == "Swift et al. 2021") %>%
      group_by(Species_ID_gbif) %>%
      summarise(
        Value = max(Value, na.rm = TRUE),
        Source = "Swift et al. 2021",
        across(everything(), ~ first(.)),
        .groups = 'drop'
      ) %>%
      distinct()
  ) %>%
  distinct()


## Excluding rows with km/h and ha in 'Units' column and with NA in 'Value' and 'Body mass' columns
db3 <- db2 |>
  filter(Units != "km/h", Units != "ha") |>
  filter(
    !is.na(Value), !is.na(Body.mass))

## Converting units for body mass and distance
 # Converting all distance values to meters
db4 <- db3 |>
  mutate(Value = case_when(Units == "km"  ~ Value * 1000,
                           Units == "miles"  ~ Value * 1609.34,
                           Units == "m"~ Value,
                           TRUE ~ NA)) |>
  mutate(Units = "m")


 # Converting all body mass values to grams
db5 <- db4 |>
    mutate(Body.mass = case_when(Body.mass.units == "kg"  ~ Body.mass * 1000,
                                 TRUE ~ Body.mass)) |>
    mutate(Body.mass.Units = "g")


setwd("C:/Users/xr49abiw/Documents/Energy-Budget-Model/output")
write_csv(db5, "DispersalTransformed.csv")
