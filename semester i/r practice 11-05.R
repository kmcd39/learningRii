library(tidyverse)


# pull in data ------------------------------------------------------------

airquality


# brainstorming questions -------------------------------------------------------------------

#' we can access documentation for functions or certain datasets with this
#' question mark

#?airquality

#' what can we do with this data?
#'
#' we can look at temperature patterns.
#'
#' Air quality patterns.
#'
#' Does ozone correlate with wind speed?
#'
#' Does temperature increase with Ozone? (or vice versa?)
#'


# coding as a class ------------------------------------------------------------



## peeks -------------------------------------------------------------------

airquality %>% summary()
airquality %>% glimpse()
airquality %>% colnames()
# ?airquality

## transforms --------------------------------------------------------------


# make lowercase (mixed upper/lowercase in column names can be annoying)
airquality <- airquality %>%
  rename_with( tolower )

# turn into a tibble
airquality <- airquality %>% tibble()


## plots -------------------------------------------------------------------



# first plot
airquality %>%
  na.omit() %>%
  ggplot(
    aes( x = ozone
        ,y = month )
  ) +
  geom_point()


# second plot
airquality %>%
  ggplot(
    aes( x = ozone
         ,y = temp )
  ) +
  geom_point() +
  geom_smooth(
    method = "lm"
  )


#' let's try to look at a third thing as well, in the same plot, in addition to
#' temp and ozone.
airquality %>% summary()
airquality %>% glimpse()
airquality



#' third plot
airquality %>%
  ggplot(
    aes(  y = ozone
         ,x = temp
         )
  ) +
  geom_jitter(
    aes(color =
          #factor(month)
          wind
        )
    ) +
  geom_smooth(
     method = "lm"
    ,color = "black"
  ) +
  scale_color_viridis_c() +
  labs(
    caption = "jitter"
  )


# variation on above -- deal with overplotting by making a little smaller
airquality %>%
  ggplot(
    aes(  y = ozone
          ,x = temp
    )
  ) +
  geom_point(
    aes(color =
          #factor(month)
          wind
    )
    ,size = 1.3
    ,alpha = .7
  ) +
  geom_smooth(
    method = "lm"
    ,color = "black"
  ) +
  scale_color_viridis_c() +
  labs(
    caption = "point (smaller)"
  )



# plot with the month as factor as the color
# variation on above -- deal with overplotting by making a little smaller
airquality %>%
  ggplot(
    aes(  y = ozone
          ,x = temp
    )
  ) +
  geom_point(
    aes(color =
          factor(month)
    )
  ) +
  geom_smooth(
    method = "lm"
    ,color = "black"
  ) +
  scale_color_viridis_d() +
  labs()

airquality


## what about histograms ---------------------------------------------------

airquality %>% glimpse()

airquality %>%
  ggplot(
    aes( x = ozone
        #,y = wind
        , fill =
          factor(month)
        )
  ) +
  geom_histogram(
    binwidth = 10
    ,color="white"
  )

# let's categorize based on wind  -----------------------------------------

#' we were trying to use wind as a categorical and noticing it wasn't working
#' out great because it's a number... but can we make categories out of a
#' number??

airquality <-
  airquality %>%
  mutate(
    wind.category =
      cut(wind,
          breaks = c(0, 6, 12, 18, 25, 50)
          )
  )

# alternative approach
airquality %>%
  mutate(wind.category =
           if_else( wind < 12
                    ,"gentle", "breezy or windy")
           )


# histogram
airquality %>%
  ggplot(
    aes( x = ozone
         #,y = wind
         , fill =
           factor(wind.category)
    )
  ) +
  geom_histogram(
    binwidth = 10
    ,color = "white"
  )

# density curves
airquality %>%
  ggplot(
    aes( x = ozone
         #,y = wind
         , fill =
           factor(wind.category)
    )
  ) +
  geom_density(
    alpha = .6
  )




# time series -------------------------------------------------------------


## indexed with the different variables ------------

