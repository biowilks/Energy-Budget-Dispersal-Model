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
  pull(`Movement.Mode`) |>
  n_distinct() 

maxdb |> 
  count(`Movement.Mode`)

#Observations (3507) 
#Species (863)
#Class (4)
#Family (192)
#MetaRef (8, 9 minus 1 for Empirical)
#Reference (890) - may be an over-estimation

# Movement.Mode    n
#        Flying 2560
#         Mixed   74
#       Running  699
#      Swimming  174

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
#        Flying 487
#         Mixed  24
#       Running 469
#      Swimming 425

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
#        Flying 133
#         Mixed  20
#       Running 290
#      Swimming   8


