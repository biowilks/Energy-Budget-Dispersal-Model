library(tidyverse)
library(dplyr)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

db <- read.csv("DispersalUntransformed.csv")


####Figuring out data gaps#### 
count(db, is.na(Body.mass)) # Na = 582 dp

dbna <- filter(db, is.na(Body.mass))

count(dbna, Taxon)#Amphibian (149), bird (2), fish (247), invertebrate (116), mammal(68)

dbna |> 
  pull(Species_ID_gbif) |>
  n_distinct() 
#215 species 



####Tidying up the data####
#Remove rows with missing, zero, or infinite values for dispersal distance and body mass
db1 <- db |>
  filter(
    !is.na(Value),
    !is.infinite(Value),
    Value != 0,          # Remove rows with Value equal to 0
    !is.na(Body.mass),
    !is.infinite(Body.mass),
    Body.mass > 0
  )
#converting units to meters
# Conversion factors
conversion_factors <- data.frame(
  unit = c("ha", "km", "miles"),
  factor = c(10000, 1000, 1609.34)
)

db2 <- db1 |>
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

db3 <- db2 |>
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

db4 <- db3 |>
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
db5 <- db4 |>
  filter(Unit != "m/h")

write_csv(db5, "DispersalTransformed.csv")
