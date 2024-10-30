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


# peeks -------------------------------------------------------------------

ny.census
ny.census %>% colnames()
ny.census %>% glimpse()

ny.census <- ny.census %>%
  mutate(boro = substr(geoid, 1, 5))

# The Bronx is Bronx County (ANSI / FIPS 36005)
# Brooklyn is Kings County (ANSI / FIPS 36047)
# Manhattan is New York County (ANSI / FIPS 36061)
# Queens is Queens County (ANSI / FIPS 36081)
# Staten Island is Richmond County (ANSI / FIPS 36085)

# brainstorm uses of the data ---------------------------------------------

#' what is amount of affordable housing?
#'
#' let's do a distribution of median housing costs per neighborhood.


ny.census %>%
  ggplot(
    aes(
       x =
         #med.hcosts
          med.crent
      ,fill = factor(boro)
      ,weight = pop
      )
  ) +
  geom_histogram(
    binwidth = 500
  ) +
  facet_wrap(vars(boro)
             )




