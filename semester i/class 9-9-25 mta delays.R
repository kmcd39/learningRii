
getwd()


# read in initial data ----------------------------------------------------


list.files("local-data/semester i/")

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


subway %>%
  group_by(year,
           reporting_category == "Crew Availability"
           ) %>%
  summarise( n.rows = n() ,
             n.delays = sum(delays)
  )


subway %>%
  group_by(month, reporting_category,
        subcategory, division, line,
        day_type
        ) %>%
  summarise( n.rows = n() ,
             n.delays = sum(delays)
  ) %>%
  ungroup() %>%
  arrange(desc(n.rows))



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
