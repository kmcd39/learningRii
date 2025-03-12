library(tidyverse)
library(sf)

options(tigris_use_cache=T)

rm(list = ls())

library(mapview)







# pull different geographies for NYC and the metro area -------------------


## CBSAs -------------------------------------------------------------------

#' CBSAs, or Core-based statistical areas, frequently represent the Metropolitan
#' area: or large geographic areas centered on a city that the census bureau
#' determines is very geographically connected, by commute patterns,
#' infrastructure etc. CBSAs are like clusters of cities and their outlying
#' suburbs.
#'
#' Like many, but not all, census geographies, CBSAs are ~co-terminous~ with
#' counties. This means that counties won't be cut off in the middle by CBSA
#' boundaries.
#'
#' Notably, however, CBSAs will often span multiple states, even while they are
#' composed neatly of counties.
#'
cbsas <- tigris::core_based_statistical_areas(
  year = 2021) %>%
  rename_with(tolower)

cbsas %>% mapview()
# get the NYC metro area
ny.cbsa <- cbsas %>%
  filter( geoid == '35620')


## get counties for NYC, NJ, PA ----------------------------------------------------

cos <- tigris::counties(
  year = 2021
  ,state = c('NY', 'NJ', 'PA')
)

cos <- cos %>% rename_with(tolower)

cos['statefp'] %>% plot()

# and state boundaries
statesf <- tigris::states(year = 2021) %>%
  rename_with(tolower)

## get census tracts for those states as well ------------------------------

# Tigris asks us to get only 1 state of tracts at a time... which we can resolve
# by using the map function

cts <- map_dfr(
  c('NY', 'NJ', 'PA')
  ,~tigris::tracts(
     year = 2021
    ,state = .x
  )) %>%
  rename_with(tolower)

# a quick map
cts[2] %>% plot()

# compare across the geographies ------------------------------------------


## CBSA and Counties -------------------------------------------------------

mapview(cos) +
  mapview(st_boundary(ny.cbsa)
          ,color= 'violet'
          ,lwd = 6)


# how do we select all counties in the metro area? ------------------------

#' Luckily, Tigris gave us a cbsafp column in the counties data that we can use
#' to filter to just the NY metro area!
#'
ggplot() +
  geom_sf(
    data = cos,
    fill = '#c2a7fa'
    ,color = 'white'
  ) +
  geom_sf(
    data = st_boundary(
      filter(statesf, stusps %in%
               c('NY', 'NJ', 'PA')))
    ,linewidth = 2
    ,color = 'white'
  ) +
  geom_sf(data = st_boundary(ny.cbsa)
          ,aes(color = name)
          #,color = '#52a885'
          ,linewidth = 2
          ) +
  scale_color_manual( #name = NULL
    guide = 'none'
    ,values = '#52a885'
    ) +
  theme_void() +
  theme( legend.position = 'top'
        ,legend.text = element_text(size = 14)
        ,text = element_text(family = 'Arial')
        ) +
  labs(
    title = str_wrap(
      'New York-Newark-Jersey City metro area (CBSA), spanning three states and several counties'
      ,width = 75 )
    )

ny.metro.cos <- cos %>%
  filter(cbsafp == '35620')

# there they are:
ny.metro.cos['geoid'] %>% plot()

ny.metro.cos


# what about all census tracts in the metro area? -------------------------

# in those 3 states, we have 11,038 census tracts (compared to 150 counties)
nrow(cts)
nrow(cos)
# This number can get unwieldy if we don't know what we're doing!

cts

#' similar the the cbsafp column that was in the counties data, the tract data
#' has statefp and countyfp:
cts %>% select(1:5)
cos %>% select(1:5)
#' FP is short for FIPS codes, which the a unique identifier for a census
#' geography.
#'
#' Importantly, many census geographies are co-terminous, which means smaller
#' ones are nested inside larger ones. For these co-terminous geographies, the
#' GEOIDs are often constructed by concatenating (pasting together) FIPS codes
#' for the different geographies.
#'
#' For example, the GEOID for counties are the STATEFP + COUNTYFP
ny.metro.cos %>% select(1:5)
#' Nassau county has a GEOID of 36059, composed of the STATEFP 36 for NY and the
#' Countyfp 059.
#'
#' Countyfps will be unique within a given state -- but will be duplicated
#' across the entire country. So we need to combine the countyfp with the
#' statefp to have a unique national identifier for the given county!
#'
#' This process repeats for other smaller geographies: Census tracts are nested
#' within counties (and have a GEOID that contains FIPS codes for their state,
#' their county, and the individual census tract within the county.)
#'
cts %>% select(1:5)
#' Even smaller than tracts are: Block Groups and Blocks, which will
#' have the same naming conventions: extending their GEOIDs from their
#' containing census tracts slightly further.

# pull block groups - we can map through the counties containing our metro area
# to pull just block groups for the area from the start.
nybgs <-
  map2_dfr(ny.metro.cos$statefp
           ,ny.metro.cos$countyfp
           ,~tigris::block_groups(
             year = 2021,
             state = .x,
             county = .y
           )
  ) %>%
  rename_with(tolower)

nybgs

# visualize these hierarchies ---------------------------------------------

# (in this context, hierarchy refers to how geographies are nested inside of one
# another)

cts
# get census tracts in the NY/Newark/JC metro area:
nycts <- cts %>%
  semi_join( tibble(ny.metro.cos)[c('statefp', 'countyfp')] )

# (alternate approach to do the same thing:
# use first 5 digits of tract geoid to
# match to county geoid)
nycts <- cts %>%
  filter( substr(geoid, 1, 5) %in%
            ny.metro.cos$geoid )

#' we can use many layers in a ggplot with different data in each one to layer on
#' the map.
#'
#' The final layer crops the map to brooklyn.
ggplot() +
  geom_sf(data = nycts
          ,fill = 'grey35'
          ,color = '#22AAFF'
          ,linewidth = .5
          ) +
  geom_sf(data = st_boundary(nybgs)
          ,color = 'white'
          ,linewidth = .1
  ) +
  geom_sf(
    data = st_boundary(ny.metro.cos)
    ,color = 'violet'
    ,linewidth = 1
  ) +
  visaux::bbox2ggcrop(filter(ny.metro.cos
                             ,grepl('Kings',name)))


#' We talked a little about this last semester: Is all census data (including
#' all ACS tables) available at all geographic levels (states, counties, CBSAs,
#' tracts, block groups, etc?)
#'
#' Can you think of trade-offs when getting data at the block-group level
#' instead of larger areal levels?


# A data viz using census geographies: ------------------------------------

#' compare 5 Boros median income to that of the metro area:
#'

co.incs <-
  map2_dfr(ny.metro.cos$statefp
           ,ny.metro.cos$countyfp
           ,~tidycensus::get_acs(
             geography = 'county'
             ,state = .x
             ,county = .y
             ,variables = c('medhhinc' = 'B19013_001')
             ,year = 2022
             ,survey = 'acs5'
           )
  )%>%
  rename_with(tolower)

co.incs %>% arrange(name)

# for metro area:
cbsa.hh.inc <-
  tidycensus::get_acs(
    geography = 'cbsa'
    ,variables = c('medhhinc' = 'B19013_001')
    ,year = 2022) %>%
  rename_with(tolower)

ny.cbsa

cbsa.hh.inc <- cbsa.hh.inc %>%
  filter(geoid == '35620')

co.incs

boros.regx <- 'Kings|Queens|^New York|Richmond|Bronx'
boro.incs <- co.incs %>%
  filter(grepl(boros.regx, name))

boro.incs %>%
  mutate(name =
           gsub(' County, New York', '', name)
         ) %>%
  arrange( estimate ) %>%
  mutate(name =
           factor(name, levels = .$name)
         ) %>%
  ggplot(
    aes( y = name
        ,x = estimate
        ,fill = estimate)
  ) +
  geom_col() +
  geom_vline(xintercept = cbsa.hh.inc$estimate
             ,linetype = 'dashed'
             ,color = 'grey35') +
  geom_label(
    data = cbsa.hh.inc
    ,aes(x = estimate)
    ,y = .8
    #,hjust = 1.05
    ,color = 'grey20'
    ,fontface = 'bold'
    ,label = 'Metro area median'
  ) +
  scale_x_continuous(
    name = 'Median household income',
    labels = scales::label_comma(prefix = '$')
    ) +
  scale_y_discrete(name = NULL
                   ) +
  scale_fill_continuous(
    guide = 'none'
  ) +
  hrbrthemes::theme_ipsum(
    grid = 'X'
    ,plot_title_size = 14
  ) +
  labs(
  title = str_wrap(width = 65
                   ,"Manhattan and Staten Island have higher median incomes than the Metro area; other NYC boroughs are lower income."
                   )
  )




## or a map: ---------------------------------------------------------------

#install.packages('ggthemes')
#install.packages('ggsflabel')


library(colorspace)

tmp <- co.incs %>%
  rename(fullname = name) %>%
  left_join(ny.metro.cos[c('geoid', 'name')]
            ) %>%
  st_sf()

tmp %>%
  ggplot() +
  geom_sf(aes(fill = estimate)
          ,color = 'white'
          ) +
  geom_sf_text(
    data = st_centroid(tmp),
    aes(label = name)
    ,size = 2.5
    ,check_overlap = T
  ) +
  scale_fill_continuous_diverging(
    palette = 'Green-Brown'
    ,mid = cbsa.hh.inc$estimate
    ,labels = scales::label_comma(prefix = '$')
    ,rev = T
  ) +
  ggthemes::theme_map()

# PT ii: spatial manipulation ----------------------------------------------------


## spatial filters ---------------------------------------------------------

#' Earlier, we trimmed all the counties in the 3 states to just the NY Metro
#' area. We had used the CBSAFP column in the county data.
#'
#' what if we didn't have that column? We could try to manually match the
#' counties or filter to the correct counties. Or we could ~spatial filter.~
#'
#' Spatial filters are great because it's flexible and can be applied to any
#' spatial data!
#'
#' So how do we do it?

# remind ourselves of what we're working with:
mapview(cos[1]) + mapview(st_boundary(ny.cbsa[1]), color = 'white')


# st filter is the basic idea of a spatial filter:
library(sf)

tmp <- cos %>%
  st_filter(ny.cbsa)


ggplot() +
  geom_sf(data = tmp
          ,aes(fill = name)) +
  geom_sf(data = st_boundary(ny.cbsa)
          ,color = 'pink'
          ,linewidth = 2) +
  scale_fill_discrete(
    guide = 'none'
  )

#' what's the issue??????
#'
#' The counties just outside the metro area overlapped at the boundary! How
#' annoying. However, we can resolves this using a different join predicate.
#'
#' The default was st_interects, but we can specify we only want counties
#' covered by the metro area:
#'
tmp <- cos %>%
  st_filter(ny.cbsa
            ,.predicate = st_covered_by
            )

ggplot() +
  geom_sf(data = tmp
          ,aes(fill = name)) +
  geom_sf(data = st_boundary(ny.cbsa)
          ,color = 'pink'
          ,linewidth = 2) +
  scale_fill_discrete(
    guide = 'none'
  )

#' The spatial predicates can be confusing, so another approach could be to get
#' points on the surface (not the boundary) of each county, and then use those
#' to filter to just the counties we want:

# get points on the surface of each county:
cos[1] %>% plot()
pts <- cos %>% st_point_on_surface()
pts[1] %>% plot()
# get a list of which of those points intersects the NY CBSA
sbgp <- st_intersects(pts, ny.cbsa)
# filter to counties the correspond to where those points intersect
tmp <- cos[lengths(sbgp) > 0, ]


ggplot() +
  geom_sf(data = tmp
          ,aes(fill = name)) +
  geom_sf(data = st_boundary(ny.cbsa)
          ,color = 'pink'
          ,linewidth = 2) +
  scale_fill_discrete(
    guide = 'none'
  )
# Visual check confirms this worked.


## Combining what we learned for more complex analysis ---------------------

#' what if we want to look at the population in the NY metro area within a
#' distance threshold from a highway? What if we want to compare the incomes of
#' people living really close to highways or entrance ramps to those farther
#' away?
#'
#' Here's how we can combine some of what we've learned to answer those
#' questions.
#'

# first, we can use tigris once again to pull highways:
?tigris::primary_roads()

#' Function documenation: 'From the Census Bureau: "Primary roads are generally
#' divided, limited-access highways within the Federal interstate highway system
#' or under state management. These highways are distinguished by the presence
#' of interchanges and are accessible by ramps and may include some toll
#' highways."'

hwys <-
  tigris::primary_roads(year = 2021
                        #,state = 36
                        ) %>%
  rename_with(tolower)

# filter to metro area:
nyhwys <- hwys %>%
  st_filter(ny.cbsa)

# nyhwys['fullname'] %>% plot()

# when we're doing count and some other commands with spatial data, remember to
# ask R to treat is as non-spatial data first -- otherwise it will group
# geometries together too, which can be useful sometimes, but will just slow
# down other analysis when we don't need it to happen.
nyhwys %>%
  tibble() %>%
  count(fullname)

nyhwys %>% tibble() %>% count(rttyp, mtfcc)
#' note: for more detailed analysis, we could get all roads with `tigris::roads`
#' and filter to, i.e., hwy entrance ramps based on the MTFCC code.
#'
#' See: https://www2.census.gov/geo/pdfs/reference/mtfccs2021.pdf


## Looking at poopulation proportions very close to a highway  ------------------------

# We want a high level of spatial precision here, so use blocks.

?tidycensus::get_decennial()
#tidycensus::load_variables(year =2020, dataset = 'dp') %>% View()


bgpops <- map2_dfr(
  ny.metro.cos$statefp, ny.metro.cos$countyfp,
  ~tidycensus::get_acs(
     state = .x
    ,county = .y
    ,year = 2022
    ,variables = c('pop' = 'B01001_001')
    ,geography = 'block group'
    )
  ) %>%
  rename_with( tolower )

#?censusrx::get.tract.attrs()
bgpops <- bgpops %>%
  select(-name)

# we can join the geometries in that we got from tigris:
nybgs
bgpopsf <- bgpops %>%
  left_join(nybgs[c(#'statefp', 'countyfp',
                    'geoid', 'aland', 'awater')]
            ,by = c('geoid') ) %>%
  st_sf()

bgpopsf %>% ggplot(aes(fill = estimate)) +geom_sf(linewidth = 0) +scale_fill_viridis_c()

nyhwys <- nyhwys %>%
  st_transform(st_crs(bgpopsf))

bgpopsf$estimate %>% summary()

sbgp <-
  st_is_within_distance(
    bgpopsf
    ,nyhwys
    ,units::set_units(1000, 'feet')
  )

sbgp

bgpopsf <- bgpopsf %>%
  mutate(near.hwy =
           lengths(sbgp) > 0)

bgpopsf

bgpopsf %>%
  ggplot() +
  geom_sf(aes(fill = near.hwy)
          ,linewidth = 0) +
  geom_sf(data = nyhwys
          ,color = 'grey35'
          ,linewidth = .5
          ) +
  theme_void()

bgpopsf %>%
  filter(aland > 0) %>%
  mapview(zcol = 'near.hwy')

# what is population of Bronx where their neighborhood is within 500 feet of a
# highway?
near.hwy.by.county <-
  bgpopsf %>%
  tibble() %>%
  mutate(countyid =
           substr(geoid, 1, 5)
         ,.after = geoid) %>%
  group_by(countyid,
           near.hwy) %>%
  summarise(pop = sum(estimate)) %>%
  group_by(countyid) %>%
  mutate(perc = pop / sum(pop)) %>%
  ungroup()

near.hwy.by.county

ny.metro.cos %>%
  filter(grepl('Bronx', name))

near.hwy.by.county %>%
  filter(countyid == '36005')

# >40% in the Bronx!

near.hwy.by.county %>%
  left_join(tibble(ny.metro.cos)[c('geoid', 'name')]
            ,by = c('countyid' = 'geoid')
            ) %>%
  ggplot(
    aes(fill = near.hwy
        ,y = name
        ,x = perc)
  ) +
  geom_col()


# (assignment! How can we improve this visual? Or this analysis?)

# something like this to extract Community District from the short.name string
# -- the \\d+ means any numbers in regular expressions.
crime.rate %>%
  mutate(cd =
           str_extract(neighborhood, 'CD\\d+'))


# alternate approach: querying using census blocks instead of BGs --------

#' Because we're looking at fairly small distances from highways, a higher
#' spatial precision could be pretty meaningful for this analysis. We could
#' achieve this by using blocks instead of block groups, which requires pulling
#' from the decennial census instead of ACS:
bpops <-
  map2_dfr(ny.metro.cos$statefp
           ,ny.metro.cos$countyfp
           #, ~tidycensus::get_acs(
           , ~tidycensus::get_decennial(
             geography = 'block'
             ,variables = c('pop' = 'P1_001NA')
             ,year = 2020
             ,state = .x
             ,county = .y
             ,survey = "pl"
           )
  ) %>%
  rename_with(tolower)



