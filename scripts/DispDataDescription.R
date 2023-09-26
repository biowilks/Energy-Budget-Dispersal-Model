library(tidyverse)

setwd('C:/Users/xr49abiw/Documents/DispersalProject/data')
database <-read.csv('DispersalTransformed.csv')


distinct_counts <- database |>
  summarize(
    Species_ID_gbif_count = n_distinct(Species_ID_gbif),
    Taxon_count = n_distinct(Taxon),
    Class_gbif_count = n_distinct(Class_gbif),
    Reference_count = n_distinct(Reference),
    MetaRef_count = n_distinct(MetaRef)
  )

distinct_counts
