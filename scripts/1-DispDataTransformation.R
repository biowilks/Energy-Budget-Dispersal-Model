library(tidyverse)
library(dplyr)

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



####Filtering data####

#Remove - m/h so we just have m - this removes all the Reptile data (n = 17)
#       - rows with missing values for dispersal distance and body mass


write_csv(db7, "DispersalTransformed.csv")


###Summarising data#### 
#Body mass coverage for data
Bresult <- db |>
  group_by(Class_gbif) |>
  summarize(
    na_count = sum(is.na(Body.mass)),
    total_count = n(),
    proportion_available = 1 - sum(is.na(Body.mass)) / total_count
  )

Btotal_result <- Bresult|>
  summarise(
    Class_gbif = "Total",
    na_count = sum(na_count),
    total_count = sum(total_count),
    proportion_available = sum(total_count - na_count) / sum(total_count)
  )

Bfinal_result <- bind_rows(Bresult, Btotal_result)

print(Bfinal_result)

#Movement mode coverage for data
Mresult <- db |>
  group_by(Class_gbif) |>
  summarize(
    na_count = sum(is.na(Movement.Mode)),
    total_count = n(),
    proportion_available = 1 - sum(is.na(Movement.Mode)) / total_count
  )

Mtotal_result <- Bresult|>
  summarise(
    Class_gbif = "Total",
    na_count = sum(na_count),
    total_count = sum(total_count),
    proportion_available = sum(total_count - na_count) / sum(total_count)
  )

Mfinal_result <- bind_rows(Mresult, Mtotal_result)

print(Mfinal_result)

#counting data 
db |> 
  pull(`Movement.Mode`) |>
  n_distinct() 

unique(db$Movement.Mode)

#Observations (6790) 
#Species (1605)
#Class (10)
#Family (314)
#MetaRef (13, minus 1 for Empirical)
#Reference (1502) - may be an over-estimation
#Movement mode (4 - minus 1 for NA)


