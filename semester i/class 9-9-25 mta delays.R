

# read in initial data ----------------------------------------------------

list.files("local-data/semester i/")

# downloaded from:
# https://data.ny.gov/Transportation/MTA-Subway-Major-Incidents-2020-2024/j6d2-s8m2/about_data

# paste together the directory and the filename to make the path to the file
path <- paste0(
  "local-data/semester i/",
  "MTA_Subway_Trains_Delayed__2020-2024_20250909.csv"
  )

# make sure the file is found
file.exists(path)

# read in the data
subway <- read.csv(path)


# how do we look at data --------------------------------------------------

# one way: pull it up as a table in another window:

# View(subway)

# to use tidyverse functions we have to load it and make sure it's installed
# install.packages("tidyverse")
library(tidyverse)

# all sorts of helper functions
subway %>% colnames()
subway %>% glimpse()

# the count command is extremely helpful for categorical collumns
subway %>% count(reporting_category)
count(subway, reporting_category)


# subsetting and indexing -------------------------------------------------

# first, count is an incrediby helpful way to think about how we might ~want~ or
# be able to filter or subset

# based on the `count` call above, we know that "Crew Availability" is one of
# the reporting categories in that column

# two ways of subsetting: base R and tidyverse

# tidyverse
subway %>%
  filter(reporting_category ==
           "Crew Availability"
         )

# in base R:
subway[subway$reporting_category == "Crew Availability", ]

# to trim by both rows AND columns
subway %>% colnames()
subway %>%
  filter(reporting_category ==
           "Crew Availability" ) %>%
  select(month, line, subcategory, delays)

# in base R:
subway[subway$reporting_category == "Crew Availability",
       c("month", "line", "subcategory", "delays") ]

# we can turn our table into a "tidy table" to make the output neater
subway %>% class()
tibble(subway)
# the tibble is the tidyverse version of the base R data.frame, both of which
# are basically just tables with rows and columns
subway <- tibble(subway)

# adding columns! ---------------------------------------------------------

# use mutate, a tidyverse function

# to save the results of the mutate call, we need to remember to re-assign (or
# newly assign) the results to an object in our environment, using `<-`
subway <-
  subway %>%
  mutate(year =
           lubridate::year(month)
         )

subway %>%
  filter(reporting_category == "Crew Availability"
         ) %>%
  count(year, reporting_category)



# let's think more about group_by and summarise ---------------------------

subway %>% glimpse()

# if we're thinking of rows per category, we can use the `count` command we saw above
subway %>% count(year)

# an equivalent way of doing that, is using group_by summarise
subway %>%
  group_by(year, reporting_category) %>%
  summarise( n.rows = n() ,
             n.delays = sum(delays)
             )


## build intution for group_by by comparing with filter --------------------

subway %>% count(reporting_category)
subway %>% count(year)

# let's filter to look at delays by a single category - how many total delays?
subway %>%
  filter(reporting_category == "Police & Medical") %>%
  filter(year == 2024) %>%
  pull(delays) %>%
  sum()

# okay great! we got the total delays for a given category -- and we could
# revise it to get to a given year too.

# if we want to look at all the categories -- instead of filtering to each one
# -- we can just group by category.
subway %>%
  group_by( year
           ,reporting_category
           ) %>%
  summarise( delays = sum(delays)
             )

# behind the hood of that `count` command -- is actually a group by summarise
subway %>% count(year,reporting_category)

subway %>%
  group_by( year
            ,reporting_category
  ) %>%
  summarise(
    n = n()
    ,delays = sum(delays)
  )

# an ungroup summarise:
subway %>%
  summarise(
    n = n()
    ,delays = sum(delays)
  )


## and a "tidyverse" vs base R refresher -----------------------------------

# the base R equivalant of that first tidyverse command we did with the filters:

# (looking again at the tidyverse way:)
subway %>%
  filter(reporting_category == "Police & Medical") %>%
  pull(delays) %>%
  sum()

# now, base R:
sum(subway[subway$reporting_category == "Police & Medical", ]$delays )



# let's think of a question or two that we're interested in ---------------

subway %>% glimpse()

#' how many delays due to police activity?
#'
#' which lines are the least reliable (most delays?)



# police activity and delays ----------------------------------------------

subway %>%
  filter(reporting_category ==
           "Police & Medical"
         ) %>%
  filter(subcategory ==
           "Public Conduct, Crime, Police Response"
         ) %>%
  group_by(year) %>%
  summarise(delays = sum(delays)
            )

# that's great--- it gives us total -- but CONTEXT is key!!!! So let's look at
# percent of whole as well
delays.from.police.activity <-
  subway %>%
  mutate(from.policy.activity =
           reporting_category ==
           "Police & Medical" &
           subcategory ==
           "Public Conduct, Crime, Police Response"
  ) %>%
  group_by(year, from.policy.activity) %>%
  summarise(delays = sum(delays)
  ) %>%
  mutate(percent =
           100 * delays / sum(delays)
  ) %>%
  ungroup()

delays.from.police.activity

## let's make a plot -------------------------------------------------------

# original scatter plot
delays.from.police.activity %>%
  ggplot(
    aes( x = percent
         ,y = year
         ,color = from.policy.activity
    )
  ) +
  geom_point()


# second plot: using columns or bars
delays.from.police.activity %>%
  ggplot(
    aes( x = percent
        ,y = factor(year)
        ,fill = from.policy.activity
        )
  ) +
  geom_col()



## total delays by category ------------------------------------------------

subway %>%
  group_by(year, reporting_category) %>%
  summarise(total.delays = sum(delays)
            ) %>%
  ggplot(
    aes(
     y = reorder(reporting_category, total.delays)
    ,x = total.delays
    ,fill = reporting_category
    )
  ) +
  geom_col() +
  labs(y = "category"
       ,x = "total delays"
       ) +
  facet_wrap(vars(year))




# teaser! where we'll soon get to -----------------------------------------

subway %>%
  group_by(year,
           reporting_category
           ) %>%
  summarise(n.delays = sum(delays)
            ) %>%
  ungroup() %>%
  ggplot(
    aes(x = year
        , y = n.delays
        , color = reporting_category)
  ) +
  geom_path(
    linewidth = 1.3
  ) +
  scale_color_brewer(type = "qual")



