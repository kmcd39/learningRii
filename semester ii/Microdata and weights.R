library(tidyverse)


# example pulling from ACS ------------------------------------------------

# by state
state.populations <-
  tidycensus::get_acs(
     geography = "state"
    ,table = "B01001"
  )

state.populations %>%
  filter(variable ==
           "B01001_001"
         )

# by tract (neighborhoods in Alabama)
al.populations <-
  tidycensus::get_acs(
    state = "AL"
    ,county = "001" # just get a single county
    ,geography = "tract"
    ,table = "B01001"
  )

al.populations

# pull labels
labls <-
  tidycensus::load_variables(year = 2022
                             ,dataset = "acs5"
  ) %>%
  mutate(label = gsub("!!", " ", label)) %>%
  mutate(label = gsub("Estimate ", "", label)) %>%
  mutate(label = gsub(":$",
                      "", label))

# we can notice how relative MOEs are so much bigger at the tract level compared
# to state-level
al.populations %>%
  filter(variable %in%
           c("B01001_034", "B01001_010")
         )
state.populations %>%
  filter(variable %in%
           c("B01001_034", "B01001_010")
  )


# merge them in
labls
state.populations

# let's match the labels to the actual data
state.populations %>%
  rename(state = NAME) %>%
  left_join(labls
            ,by = c("variable" = "name")
            )

# -------------------------------------------------------------------------

# peek at avialbel microdata variables
tidycensus::pums_variables %>% count(year)
tidycensus::pums_variables %>%
  filter(year == 2022) %>%
  filter(survey == "acs1") %>%
  View( )

tidycensus::pums_variables %>%
  filter(variable %in%
           c("TEN", "HHL"))

#' use tidycensus to pull some microdata
#'
#' Census bureau has documentaiton on PUMS areas
#'
#' https://www.census.gov/geographies/reference-maps/2010/geo/2010-pumas/new-york.html
pums <- tidycensus::get_pums(
  state = "36"
  ,puma = "03807" # NYC-Manhattan Community District 4 & 5--Chelsea, Clinton & Midtown Business District PUMA
  ,year = 2021
  ,survey = "acs1"
  ,variables = c("TEN", "HHL")
  ,rep_weights = "housing"
  ,recode = T
)


pums %>%
  glimpse()


# a quick analysis -- -----------------------------------------------------


tmp <- pums %>%
  group_by(TEN, TEN_label,
           HHL, HHL_label) %>%
  summarise(estimate = sum(WGTP)
            ) %>%
  ungroup()

tmp %>%
  ggplot() #......


# margins of error --------------------------------------------------------



#' (more advanced...) -- getting margins of error -- see
#'
#' https://walker-data.com/census-r/analyzing-census-microdata.html

