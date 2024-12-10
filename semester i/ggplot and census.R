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


## add boro names ----------------------------------------------------------

boro.nms <-
  tibble(
     boro.id = as.character(c(36005, 36047, 36061, 36081, 36085))
    ,boro.nm = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
  )



ny.census %>% glimpse()

ny.census %>% count(boro)

# add in boro names

ny.census <-
  ny.census %>%
  left_join(boro.nms
             ,by = c("boro" = "boro.id")
            #,by = join_by(boro == boro.id)
            )



ny.census %>% glimpse()


# population by boro? ------------------------------------------------------


ny.census %>% count(boro, boro.nm)


ny.census %>% glimpse()

ny.census %>%
  group_by(boro.nm) %>%
  summarise(pop = sum(pop))



# making a plot -----------------------------------------------------------

#' let's do a little more complex calculation
#'
#' and demo diverging and other color palettes

ny.census %>% glimpse()
ny.census %>% colnames()

# what is going on in the crazy outlier?
ny.census %>%
  arrange(desc(unemply.rate)) %>%
  head(5) %>%
  st_sf() %>%
  mapview::mapview(zcol = "unemply.rate")
  #filter(unemply.rate > .5)


ny.census %>%
  filter(pop > 0) %>%
  arrange((n.hh)) %>%
  head(5) %>%  st_sf() %>%
  mapview::mapview(zcol = "unemply.rate")

# rental rate x unemployment
qns <- ny.census %>%
  filter(boro.nm == "Queens")


# first scatterplot w viridis
p <- ny.census %>%
  filter(pop > 100) %>%
  filter(n.hh > 30) %>%
  ggplot(
    aes(
       x = rental.rate
      ,y =
        #n.unemployed
        unemply.rate
      ,color =
        perc_latino
    )
  ) +
  geom_point(
    alpha = .67
    ,size = 1.1
    ,shape = 19
  )  +
  scale_color_viridis_c()

#' other ways to customize our plots
p +
  hrbrthemes::theme_ipsum() +
  theme(legend.justification = "top"
        ) +
  labs(
    title =
      str_wrap(width = 40
      ,"NYC Neighborhoods with higher % latino have higher rental rates")
  )

# to save the vectoried image, you can use ggsave or other functions.
ggsave(
  filename = "quick census plot.png"
)




# back to boro based analysis ---------------------------------------------

ny.boros <-
  censusrx::get.tract.attrs(
    state = "NY"
    ,cofps = county.fips
    ,year = 2021
    ,geo = "county"
    ,get.demographics.and.commute = T
  )

ny.boros <-
  ny.boros %>%
  mutate(boro.id = substr(geoid, 1, 5)
         ) %>%
  left_join(boro.nms) %>%
  filter(!is.na(boro.nm))



# let's say we want to use a diverging palette

# what is the US median income?
natl.medhh.inc <- 70e3

ny.boros %>%
  ggplot(
    aes(x = med.hhinc
        ,y = boro.nm
        ,fill = med.hhinc)
  ) +
  geom_col() +
  geom_vline(
    xintercept = natl.medhh.inc
    ,linetype = "dashed"
  ) +
  colorspace::scale_fill_continuous_divergingx(
     palette = "PRGn"
    ,mid = natl.medhh.inc
  ) +
  theme_dark() +
  theme(
     panel.grid.minor.y = element_blank()
    ,panel.grid.major.y = element_blank()
  )

# interactive version
plotly::ggplotly()

# interactive table htlm version
DT::datatable(ny.boros)

# showing map -------------------------------------------------------------


ctsf <- tigris::tracts(
  state = 36
  ,county = substr( boro.nms$boro.id, 3, 5)
  ,year = 2021
) %>%
  rename_with(tolower) %>%
  select(geoid, geometry)

ny.census <- ny.census %>%
  left_join(ctsf)



# map ---------------------------------------------------------------------


library(sf)

ny.census %>%
  st_sf() %>%
  mapview::mapview(zcol ="med.hhinc")


