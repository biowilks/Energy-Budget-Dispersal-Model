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


