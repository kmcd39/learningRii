# install necessary packages ----------------------------------------------

# install.packages("tidyverse")
# install.packages("sf")
# install.packages("Hmisc")
# install.packages(c("tidycensus", "tigris"))
# install.packages("devtools")
# devtools::install_github("kmcd39/censusrx")

# setup census API key ----------------------------------------------------

#' instructions from the tidycensus author:
#'
#' https://walker-data.com/tidycensus/articles/basic-usage.html

# set your api key!
# tidycensus::census_api_key()

# load packages -----------------------------------------------------------

library(tidyverse)
library(tidycensus)

# pull some census data --------------------------------------------------------

# census fips codes for the 5 boroughs of NYC
county.fips <- c("085", "005", "047", "061", "081")

ny.census <-
  censusrx::get.tract.attrs(
   state = "NY"
  ,cofps = county.fips
  ,year = 2021
  ,geo = "tract"
  ,get.demographics.and.commute = T
  )

ny.census
ny.census %>% glimpse()





