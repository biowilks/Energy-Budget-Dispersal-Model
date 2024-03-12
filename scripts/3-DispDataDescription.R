library(tidyverse)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')

db5 <- read.csv("DispersalTransformed.csv")

#### Filtering and summarising data for mean, median, and maximum distances
maxdb <- filter(db5, Statistic == "Maximum")
meandb <- filter(db5, Statistic == "Mean")
meddb <- filter(db5, Statistic == "Median")


#Summarising data for max
maxdb <- maxdb |>
  filter(!is.na(Movement.Mode))

maxdb |> 
  pull(`Reference`) |>
  n_distinct() 

maxdb |>
  count(Movement.Mode)

#Observations (3382) 
#Species (814)
#Class (4)
#Family (185)
#MetaRef (7, 8 minus 1 for Empirical)
#Reference (760) - may be an over-estimation

# Movement.Mode    n
#        Flying 2562
#         Mixed   64
#       Running  670
#      Swimming  184


###Summarising data### 
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
  pull(MetaRef) |>
  n_distinct() 

unique(db$Class_gbif)

#Observations (6790) 
#Species (1605)
#Class (10)
#Family (314)
#MetaRef (13, minus 1 for Empirical)
#Reference (1502) - may be an over-estimation
#Movement mode (4 - minus 1 for NA)




#Summarising data for mean
meandb <- meandb |>
  filter(!is.na(Movement.Mode))

meandb |> 
  pull(`Reference`) |>
  n_distinct() 

meandb |> 
  count(`Movement.Mode`)

#Observations (1405) 
#Species (595)
#Class (4)
#Family (135)
#MetaRef (6, 7 minus 1 for Empirical)
#Reference (505) - may be an over-estimation
#  Movement.Mode   n
#        Flying 490
#         Mixed  21
#       Running 467
#      Swimming 464

#Summarising data for median
meddb <- meddb |>
  filter(!is.na(Movement.Mode))

meddb |> 
  pull(`Species_ID_gbif`) |>
  n_distinct() 

meddb |> 
  count(`Movement.Mode`)

#Observations (451) 
#Species (166)
#Class (3)
#Family (62)
#MetaRef (4, 5 minus 1 for Empirical)
#Reference (253) - may be an over-estimation
#  Movement.Mode   n
#        Flying 134
#         Mixed  20
#       Running 290
#      Swimming   8


