# to install tidyverse:
# install.packages("tidyverse")

library(tidyverse)
# from
# https://www.oecd-ilibrary.org/environment/data/global-plastic-outlook_c0821f81-en
trash <-
  read.csv("local-datasets/OECD,DF_PLASTIC_USE_10,+all.csv") %>%
  tibble()

trash %>% count(STRUCTURE)
trash %>% map_dbl( ~length(unique(.x)) )

trash %>% select(matches("unit"))

# drop all columns were all the values are the same (keep only those containing
# multiple values)
trash <- trash %>%
  select(where( ~length(unique(.x)) > 1 ))

trash


# peeks -------------------------------------------------------------------

trash %>% glimpse()

trash %>% count(Plastics.applications)

trash %>%
  filter(TIME_PERIOD  == 2019) %>%
  summarise(total = sum(OBS_VALUE))

# analysis :) -------------------------------------------------------------

# filter to totals
tots <- trash %>%
  filter(Plastics.applications == "Total")

tots %>% count(Plastics.applications)


## a base r plot -----------------------------------------------------------

#' the basic `plot` function is in base R, rather than using the tidyverse...

plot(tots$TIME_PERIOD, y= tots$OBS_VALUE)


## tidyverse plot (ggplot) ----------------------------------------------------------

#' ..whereas `ggplot` is part of the tidyverse, and is loaded with your
#' `library(tidyverse)` call

tots %>%
  ggplot(
    aes( x = TIME_PERIOD
         ,y = OBS_VALUE)
  ) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "year"
       ,y = "trash")


# tidyverse by application ------------------------------------------------

# filter to all ~except~ totals! (`!=` is "not equal to")
trash <- trash %>%
  filter(Plastics.applications != "Total")

trash %>%
  ggplot(
    aes( x = TIME_PERIOD
         ,y = OBS_VALUE
         ,color = Plastics.applications)
  ) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "year"
       ,y = "trash")

plotly::ggplotly()

# assignment reminder -----------------------------------------------------

some.numbers

