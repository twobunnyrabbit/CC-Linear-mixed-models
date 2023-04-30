library(tidyverse)
library(here)

load(here::here('./dragons.RData'))
dragons <- as_tibble(dragons)

dragons |> names()

dragons |> 
  group_by(site, mountainRange) |> 
  tally()

count(dragons, site)

# distribution of testScore
