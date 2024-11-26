library(tidycensus)
library(tidyverse)

census_api_key("14e8e9f0d7daa64ad88c2d874fdc1f43344d39b9")
readRenviron("~/.Renviron")
options(tigris_use_cache = TRUE)
test <- get_acs(
  state = "MA",
  county = "Suffolk",
  geography=050,
  variables = NULL,
  geometry = TRUE,
  year = 2020
)

get_decennial(geography = "county", state = "MA",county = "Suffolk",year = 2020)
