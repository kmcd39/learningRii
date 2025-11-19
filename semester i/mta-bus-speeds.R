library(tidyverse)


# pull in data ------------------------------------------------------------

#' for 2015-2019 - from:
#'
#' https://data.ny.gov/Transportation/MTA-Bus-Speeds-2015-2019/cudb-vcni/about_data
#'
#' for 2020-2024 - from:
#'
#' https://data.ny.gov/Transportation/MTA-Bus-Speeds-2020-2024/6ksi-7cxr/about_data
ddir <- "~/R/local-data/NY state open data/MTA bus speeds/"
fns <- list.files(ddir)

buses <-
  map(fns,
    ~vroom::vroom(
      paste0(ddir, .x)
      )
    ) %>%
  set_names(fns)

buses %>% map(colnames)

buses <- buses %>%
  do.call("rbind", .)



# peeks -------------------------------------------------------------------

buses %>% glimpse()

agg.buses <-
  buses %>%
  group_by(month) %>%
  summarise(across(c(total_operating_time, total_mileage, average_speed)
                   ,mean)
            )

buses %>%
  filter(month == min(month)) %>%
  count(route_id
        ,day_type, period)

buses %>%
  ggplot(
    aes( x = month
         ,y = average_speed
         )
  ) +
  geom_path(
    aes(,color = trip_type
        ,group = route_id)
    ,alpha = .05) +
  geom_path(
    data = agg.buses
    ,color = "black"
  ) +
  scale_y_continuous(limits = c(0,30)
                     ) +
  facet_wrap(vars(
    day_type, period)
             ) +
  guides(color = guide_legend(
    override.aes = list(alpha = 1) )
               )



buses %>%
  filter(trip_type %in%
           #c("EXP", "LCL/LTD")
           "LCL/LTD"
         ) %>%
  ggplot(
    aes(x = average_speed
        , color = borough)
  ) +
  geom_freqpoly() +
  scale_x_continuous(
    limits = c(0, 30)
  ) +
  labs(
    subtitle = "Distribution of Bus speeds by borough, local buses only"
    ,caption = "Source: NY State Open data for MTA")
  facet_wrap(
    vars(
      day_type, period, trip_type)
  )

buses %>%
  filter(trip_type == "SBS"
         ) %>%
  ggplot(
    aes( x = month
         ,y = average_speed
    )
  ) +
  geom_path(
    aes(,color = borough
        ,group = route_id)
    ,alpha = .5
    ) +
  geom_path(
    data = agg.buses
    ,color = "black"
  ) +
  # scale_y_continuous(limits = c(0,30)
  # ) +
  facet_wrap(vars(
    day_type, period)
  ) +
  guides(color = guide_legend(
    override.aes = list(alpha = 1) )
  ) +
  labs(
    subtitle = "SBS bus speeds by brough, compared to average of all buses"
  )
