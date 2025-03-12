library(tidyverse)
library(sf)

# to install `sf`
# install.packages("sf")

# let's start by pulling states -------------------------------------------

statesf <-
  tigris::states()

statesf %>% glimpse()

# make a quick map
statesf %>%
  ggplot() +
  geom_sf()



# let's pull some chicago data -------------------------------------------

# how are we goanna think about what states we want to pull data for ? If we're
# interested in the greater chicago area?
statesf %>% glimpse()

il.countiesf <-
  tigris::counties(
  state = 17
  ,year = 2021
)

# or to pull ALL counties for states that have any of the Chicago metro area
il.in.wi.countiesf <-
  c("IL","IN","WI") %>%
  map_dfr(
    ~tigris::counties(
      state = .x
      ,year = 2021
    )
  )

# il.in.wi.countiesf %>% mapview::mapview(zcol = "STATEFP")
il.countiesf %>% tibble() %>% count(CBSAFP)

il.countiesf %>%
  ggplot() +
  geom_sf()

# look at counties on an interactive map
mapview::mapview(il.countiesf
                , zcol = "NAME")

# let's also pull the metro areas
metrosf <-
  tigris::core_based_statistical_areas(
    year =2021
    )

# let's find the chicago metro
metrosf %>% glimpse()

chicago.metro <-
  metrosf %>%
  filter(
    grepl("Chicago", NAME)
    )

chicago.metro

# pull cities
il.citiesf <-
  tigris::places(
    state= "IL")

il.citiesf %>%
  ggplot() +
  geom_sf()


## let's do a spatial filter -----------------------------------------------

#' this will let us get all the cities + towns + municipalities w/in the Chicago
#' metro area.

chicago.metro
il.citiesf


cities.in.chicago <-
  il.citiesf %>%
  st_filter(chicago.metro)

cities.in.chicago %>% nrow()
il.citiesf %>% nrow()

chicago.metro

mapview::mapview(
  chicago.metro
  ) +
  mapview::mapview(cities.in.chicago
                   ,zcol = "NAME")

tigris::school_districts()



# let's map something by tracts in chicago metro --------------------------

# first let's figure out what counties are in the metro area
chicago.metro %>% glimpse()
il.countiesf %>% glimpse()
il.countiesf %>% filter( !is.na(CBSAFP) )


chicago.metro.cosf <-
  #il.countiesf %>%
  il.in.wi.countiesf %>%
  filter(CBSAFP ==
           chicago.metro$CBSAFP)

chicago.metro.cosf$GEOID


# let's pull some population/demographic info at tract level.

# to see census variables: tidycensus::load_variables()
il.in.wi.countiesf$GEOID %>% substr(3, 5)

demos <-
  tidycensus::get_acs(
        geography = "tract"
        ,year = 2021
        ,state = 17
        ,variables =
          c( tot.pop = "B03002_001"
             ,latino.pop = "B03002_012"
          )
        ,geometry = T
      )
  # to get for all areas
  # chicago.metro.cosf$GEOID %>%
  # map_dfr(
  #   ~tidycensus::get_acs(
  #     geography = "tract"
  #     ,year = 2021
  #     ,state = substr(.x, 1,2)
  #     ,county = substr(.x, 3,5)
  #     ,variables =
  #       c( tot.pop = "B03002_001"
  #          ,latino.pop = "B03002_012"
  #       )
  #     ,geometry = T
  #   )
  # )


il.countiesf

demos %>% glimpse()

demos <- demos %>% select(-NAME)

# how could we calculate % latino by tract?
demos <- demos %>%
  pivot_wider(
    names_from = "variable"
    ,values_from = c("estimate", "moe")
  )

demos <-
  demos %>%
  mutate(
    perc.latino =
      estimate_latino.pop / estimate_tot.pop
  )

# let's crop to (illinois-portion) of the metro area
chicago.metro.cosf$GEOID
demos$GEOID %>% head(14) %>% substr(1, 5)
demos <-
  demos %>%
  filter(
    substr(GEOID, 1, 5)  %in%
           chicago.metro.cosf$GEOID
           )


# finally we have neighborhoods in the Chicago metro area, w/in illinois
demos %>% glimpse()
demos %>%
  ggplot() +
  geom_sf(
    aes(
      fill = perc.latino
    )
    ,color = NA
  ) +
  scale_fill_viridis_c() +
  geom_sf(data = st_boundary(cities.in.chicago)
          ,color = "white"
          ,linewidth = .05)

demos %>%
  mapview::mapview(zcol = "perc.latino")


#' mapgl package links to some more contemporary versions of leaflet, and mapview
#' is basically a "quick" way to create leaflet maps.
