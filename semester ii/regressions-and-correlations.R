library(tidyverse)
library(sf)

options(tigris_use_cache=T)

rm(list = ls())

# pull tract-level census info ------------------------------------------

cbsas <- tigris::core_based_statistical_areas()
#cbsas %>% mapview()
#' use Kira's wrapper function to quickly pull a lot of tract-level data at
#' once for Connecticut
cos <- tigris::counties(state = 'CT')

attrs <-
  map_dfr(cos$COUNTYFP,
           ~censusrx::get.tract.attrs(
             state = '09'
             ,cofps = .x
             ,year = 2022)
           )

attrs %>% glimpse()

# a quick scatterplot -----------------------------------------------------


attrs <- attrs %>%
  mutate(med.hhinc =
           med.hhinc / 1e3)

base.plot <-
  attrs %>%
  filter(pop > 0) %>%
  ggplot(
    aes( y = rental.rate
        ,x = med.hhinc
        #,x = log(med.hhinc)
        # ,size = pop
        )
  ) +
  geom_point(
    aes(alpha = pop)
    ,size = .5
  )  +
  scale_alpha_continuous(
    range = c(.2, .8)
  ) +
  scale_x_continuous(
    name = 'Median household income\n(thousands)'
    ,labels = scales::label_comma()
  ) +
  hrbrthemes::theme_ipsum(
    plot_title_size = 14
    ,subtitle_size = 11) +
  labs(title =
         str_wrap(width = 65,
         'Household income and renter/owner status in Connecticut')
       ,subtitle =
         'Each dot represents one neighborhood in CT.'
  )

base.plot


## adding geom_smooth ------------------------------------------------------

#' we can add smoothed lines that try to show the relationship in different
#' ways.
#'
#' The loess line draws a smooth curve...
base.plot +
  geom_smooth(
    method = 'loess'
  )


#' the LM method does a Linear Model, looking at the relationship between the
#' variables...
base.plot +
  geom_smooth(
    method = 'lm'
  ) +
  scale_y_continuous(
    limits = c(0,1)
  )

#' linear models are a common statistical/econometric technique that try to find
#' the relationship between two variables.
#'
#' Our scatterplot looks at the proportion of households renting on the y axis
#' and the median household income on the x axis.
#'
#' You may remember from algebra/HS math classes that the X-axis variable is
#' often called the independent variable and the Y-axis variable is often call
#' the dependent variable. ("The Y value depends on the X value").
#'
#' In a linear model, one or more independent variables (regressors) are used to
#' model or predict a single independent variable.
#'
#' Linear models are often used to say something like "an increase in income of
#' x amount is associated with a increase in homeownership by y percent."
#'
#' The two amounts will be the slope of the linear model!
#'
#' However, we should use a lot of caution when interpreting and presenting the
#' results of a linear model in that way.
#'
#'


# run the model in R ---------------------------------------------------

#' we can run the model in R with the `lm` function:
attrs
model1 <-
  lm(
    formula = 'rental.rate ~ med.hhinc',
    data = attrs
    )

model1
model1 %>% summary()

# we can also run a version of the model that includes population density as a
# second variable and weights observations by tract population:
model2 <-
  lm(
    formula = 'rental.rate ~ med.hhinc + pop_dens',
    data = attrs
    ,weights = pop
  )

model2 %>% summary()
#' the second model has a higher R-squared -- meaning it has potentially more
#' explanatory power, but the coefficient for med.hhinc is a bit smaller in
#' absolute value -- suggesting that some of the effect attributed to that
#' variable can now be accounted for the other variable we added.


# finding correlations ----------------------------------------------------

attrs %>% glimpse()

car.model <-
  lm("perc.no.car ~ log(pop_dens) + log(med.hhinc)"
   ,data = attrs
   ,weights = pop)

car.model %>% summary()

attrs %>%
  ggplot(
    aes(
      y = log(perc.no.car)
      ,x = (med.hhinc)
      ,color = as.numeric(pop_dens)
        ,size = pop)
  ) +
  geom_point(
    alpha = .667
  ) +
  geom_smooth(
    method = 'lm'
  ) +
  scale_color_viridis_c() +
  scale_radius(
    range = c(.1,3)
  ) +
  scale_x_continuous(
    #limits =c(0,40)
  ) +
  scale_y_continuous(
    #limits =c(0,.5)
  ) +
  hrbrthemes::theme_ipsum()



# transforming with natural logs --------------------------------------------

attrs %>%
  filter(pop > 0) %>%
  ggplot(
    aes( y = rental.rate
         ,x = med.hhinc
         #,x = log(med.hhinc)
         # ,size = pop
    )
  ) +
  geom_point(
    aes(alpha = pop)
    ,size = .5
  )  +
  geom_smooth(method = "lm"
              ) +
  scale_alpha_continuous(
    range = c(.2, .8)
  ) +
  scale_x_log10(
    name = 'Median household income\n(thousands)'
    ,labels = scales::label_comma()
  ) +
  scale_y_continuous(
    limits = c(0,1)
  ) +
  hrbrthemes::theme_ipsum(
    plot_title_size = 14
    ,subtitle_size = 11) +
  labs(title =
         str_wrap(width = 65,
                  'Household income and renter/owner status in Connecticut')
       ,subtitle =
         'Each dot represents one neighborhood in CT.'
  )



## running and interpreting the model with natural logs --------------------

model3 <-
  lm(
    formula = 'rental.rate ~ log(med.hhinc)',
    data = attrs
  )


model1 %>% summary()
model2 %>% summary()
model3 %>% summary()

# for model 3, the explanatory variable is ~logged,~ so, the coefficient can be
# interpreted as the change in y for every %change in x. So an increase in hh
# income by 1% yields a .4 decrease in rental.rate.

lm(
  formula = 'rental.rate ~ log(med.hhinc) + pop_dens',
  data = attrs
  ) %>% summary()
