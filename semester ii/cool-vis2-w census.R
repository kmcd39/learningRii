library(tidyverse)
library(sf)

rm(list = ls())

options(tigris_use_cache = T)



# let's get data for the NYC metro area -----------------------------------

# to do that, we want to get the counties in the metro area.


# (how do we do that again?)
nycos <-
  tigris::counties(
    year = 2021,
    state = c("NY", "NJ", "PA")
  ) %>%
  rename_with(tolower)

# quick maps & peeks:
plot(nycos[1])
nycos[1] %>% plot() # (reminder of how the Pipe (%>%) works, by sending the object to the function)
nycos[nycos$name == "Pike",]
nycos %>% mapview::mapview(zcol = "geoid")
nycos %>% select(geoid)
nycos %>% glimpse()

#' how do we get from here to all the counties in the NY metro area
cbsasf <-
  tigris::core_based_statistical_areas(
    year = 2021
  ) %>%
  rename_with(tolower)

#devtools::install_github("kmcd39/mapglview")
cbsasf %>%
  mapview(zcol = "name")

# (where are cbsasfps == NA?)
nycos %>%
  filter(is.na(cbsafp)
         ) %>%
  mapview(zcol = "aland")

cbsasf

ny.metro <- cbsasf %>%
  filter(grepl("New York", name)
  )

library(mapview)
nycos %>%
  mapview(zcol= "name") +
  mapview(st_boundary(ny.metro)
          ,color = "pink", lwd =3 )

nycos %>%
  filter(grepl("Queens|Kings", name))
ny.metro

## filtering to counties in the metro --------------------------------------

# (quick digression: spatial ("sf") data is sometimes treated differently than
# non-spatial  data, defined by tibble or data.frame)
nycos %>% class()
nycos %>% data.frame() %>% class()

nycos %>% tibble() %>% count(cbsafp)
nycos %>%
  count(cbsafp) %>%
  mapview(zcol = "n")

mapview(
  nycos, zcol = "cbsafp"
  ,col.regions =
    visaux::jewel.pal()
) +
  mapview(st_boundary(ny.metro), color = "pink"
          , lwd = 5)

# filter by used cbsafp (metro-area identifier)
ny.metro.counties <- nycos %>%
  filter(cbsafp ==
           ny.metro$cbsafp)

ny.metro.counties


## spatial filter ----------------------------------------------------------

nycos %>% mapview()

maybe.ny.counties <-
  nycos %>%
  st_filter(ny.metro)

maybe.ny.counties %>%
  mapview(zcol = "cbsafp")+
  mapview(st_boundary(ny.metro), color = "pink"
          , lwd = 5)

# osmextract is an r package to ~extract~ open street map (osm) data

### or , w a filtering join -------------------------------------------------

tibble(nycos) %>%
  semi_join(tibble(ny.metro)
            ,by = "cbsafp")


# (pull variables we're interested in) -------------------------------------

# (let's take 10-15 minutes to pick variables and pull them)
ny.metro.counties


## decide on variables ------------------------------------------------------

acs.vars <- tidycensus::load_variables(
  year = 2021
  ,dataset = "acs5"
)

acs.vars <- acs.vars %>%
  mutate(label =
           str_replace_all(label, "!!", " "))

acs.vars %>% View()



# to pull in a table we're interested in... -------------------------------

ny.metro.counties %>% glimpse()
ny.metro.counties %>% mapview()
ny.data <-

  tidycensus::get_acs(
    geography = "tract",
    table = "C16002",
    state = ny.metro.counties$statefp,
    county =  ny.metro.counties$countyfp
    ,year = 2021
    ,cache_table = T
  )

ny.data <- ny.data %>%
  rename_with(tolower)

# make sure we pulled only the appropriate counties.
ny.data$geoid %>% head()
ny.metro.counties$geoid %>% head()
ny.data <-
  ny.data %>%
  filter( substr(geoid, 1 ,5 ) %in%
           ny.metro.counties$geoid
           )

ctsf <-
  unique(ny.metro.counties$statefp) %>%
  map_dfr(~tigris::tracts(state = .x)) %>%
  rename_with(tolower)


ny.data %>%
  filter(variable == "C16002_001"
         ) %>%
  left_join(ctsf, by = "geoid") %>%
  st_sf() %>%
  mapview(zcol= "estimate")

# a quick exercise making a viz from the data we pulll --------------------

#' scatterplots, distributions, maps, etc.

