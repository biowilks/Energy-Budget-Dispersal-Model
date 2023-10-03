library(tidyverse)
library(dplyr)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

db <- read.csv("DispersalUntransformed.csv")


####Summarising data#### 
result <- db |>
  group_by(Class_gbif) |>
  summarize(
    na_count = sum(is.na(Body.mass)),
    total_count = n(),
    proportion_available = 1 - sum(is.na(Body.mass)) / total_count
  )

total_result <- result|>
  summarise(
    Taxon = "Total",
    na_count = sum(na_count),
    total_count = sum(total_count),
    proportion_available = sum(total_count - na_count) / sum(total_count)
  )

final_result <- bind_rows(result, total_result)

print(final_result)

#counting data 
db |> 
  pull(`Class_gbif`) |>
  n_distinct() 

#MetaRef (15), Reference (1502) - may be an over-estimation, species (1608), gbif taxonomic class (10)

####Tidying up the data####
#converting units to meters
# Conversion factors
conversion_factors <- data.frame(
  unit = c("ha", "km", "miles"),
  factor = c(10000, 1000, 1609.34)
)

db1 <- db |>
  left_join(conversion_factors, by = c("Units" = "unit")) |>
  mutate(
    Value = case_when(
      !is.na(Value) & Units == "km/h" ~ Value * (1000 / 3600), # Convert km/h to m/h
      !is.na(Value) & Units %in% c("ha", "km", "miles") ~ Value * factor, # Convert ha, km, miles to m
      TRUE ~ Value
    ),
    Unit = case_when(
      !is.na(Value) & Units == "km/h" ~ "m/h", # Change unit for km/h to m/h
      !is.na(Value) & Units %in% c("ha", "km", "miles") ~ "m", # Change unit for ha, km, miles to m
      TRUE ~ Units
    )
  ) |>
  relocate(Unit, .after = `Value`) |>
  select(-Units, -factor)


#converting trait database body mass values
# Conversion factors
conversion_factors1 <- data.frame(
  unit = c("kg", "log(10)g"),
  factor = c(1000, 1) # Conversion to grams
)

db2 <- db1 |>
  left_join(conversion_factors1, by = c("Body.mass.database.Unit" = "unit")) |>
  mutate(
    Body.mass = case_when(
      Body.mass.database.Unit %in% c("kg", "log(10)g") & !is.na(Body.mass) ~ Body.mass * factor,
      TRUE ~ Body.mass
    ),
    Body.mass.db.Unit.convert = case_when(
      Body.mass.database.Unit %in% c("kg", "log(10)g") ~ "g",
      TRUE ~ Body.mass.database.Unit
    )
  ) |>
  relocate(Body.mass.db.Unit.convert, .after = Body.mass.Units) |>
  select(-Body.mass.database.Unit, -factor)


#Converting movement database body mass values
# Conversion factor for kg to g
conversion_factor_kg_to_g <- 1000

db3 <- db2 |>
  mutate(
    Body.mass = case_when(
      Body.mass.Units == "kg" & !is.na(Body.mass) ~ Body.mass * conversion_factor_kg_to_g,
      TRUE ~ Body.mass
    ),
    Body.mass.Units.convert = case_when(
      Body.mass.Units == "kg" ~ "g",
      TRUE ~ Body.mass.Units
    )
  ) |>
  relocate(Body.mass.Units.convert, .after = Body.mass.Units) |>
  select(-Body.mass.Units)

#Further filtering
#Removing m/h so we just have m - this removes all the Reptile data (n = 17)
#Remove rows with missing, zero, or infinite values for dispersal distance and body mass
db4 <- db3 |>
  filter(Unit != "m/h") |>
  select(-Body.mass.Units.convert, Body.mass.unit = Body.mass.db.Unit.convert) |>
  filter(
    !is.na(Value),
    !is.infinite(Value),
    Value != 0,          # Remove rows with Value equal to 0
    !is.na(Body.mass),
    !is.infinite(Body.mass),
    Body.mass > 0
  )

write_csv(db4, "DispersalTransformed.csv")

