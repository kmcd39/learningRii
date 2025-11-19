library(tidyverse)

# pull in data ------------------------------------------------------------

pth <-
  "~/R/local-data/ember/india_monthly_full_release_long_format.csv"

file.exists(pth)

india <- vroom::vroom(pth)

# peeks -------------------------------------------------------------------

india %>% glimpse()

# let's take a minute to wrap our head around the different columns
india %>%
  count(Date)

#' reminder that `count` is short for `group_by() %>% summarise(n())`

# we can start looking at how category and subcategory columns interact as
# below:
india %>%
  count(Category, Subcategory)

# here we go -- to see the relationship between the columns. We have to
# supplement with their metadata to understand how the "aggregate fuels" relate
# to one another.
india %>%
  count(Category, Subcategory, Variable, Unit) %>%
  print(n = 100)

india %>%
  count(`State type`, State
                ) %>%
  print(n = 40)

# let's focus on emissions ------------------------------------------------

india %>% count(Category)

emissions <- india %>%
  filter(Category ==
           "Power sector emissions")

emissions %>%
  count(Subcategory, Variable, Unit) %>%
  print(n = 100)


## let's look at co2 intensity to start ------------------------------------

# let's just make a plot to look at Co2 emissions
tmp <- emissions %>%
  filter(Subcategory == "CO2 intensity"
  )

tmp %>% glimpse()

### India, entire country!  -----------------------------------------------------
tmp %>%
  filter(`State type` ==
           "total") %>%
  ggplot(
    aes( x = Date
        ,y = Value
        )
  ) +
  geom_path() +
  scale_y_continuous(
    limits = c(0,700)
  )


### breakdown by states -----------------------------------------------------

tmp %>% count(`State type`, State) %>% print(n = 70)

tmp %>%
  filter(`State type` !=
           "total") %>%
  filter(`State type` !=
           "Union territory"
         ) %>%
  ggplot(
    aes( x = Date
         ,y = Value
         ,group = State    )
  ) +
  geom_path()

plotly::ggplotly()

# let's think about what's happening in places that look wonky
tmp %>%
  filter(`State code` == "LD")

tmp %>% count(`State code`) %>% arrange(desc(n))
tmp$Date %>% unique( ) %>% length()



