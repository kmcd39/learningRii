library(tidyverse)


# load data ---------------------------------------------------------------

# an example from the Wickham paper
# (https://vita.had.co.nz/papers/tidy-data.pdf) as a csv
rel <-
  read.csv('local-data/untidy-table.csv') %>%
  tibble()

rel

# peek --------------------------------------------------------------------

rel

rel |> colnames()
rel |> glimpse()

# tidy --------------------------------------------------------------------

#' regular expressions are often very useful for tidying data!
#'
#' We'll use the function `?pivot_longer`
#'

rel %>% head()
rel %>%
  pivot_longer(
    cols = 2:ncol(.)
    ) %>%
  head()


# new object can be trel for tidy religion
trel <- rel %>%
  pivot_longer(
    cols = 2:ncol(.)
    ,names_to = 'income.bucket'
    ,values_to = 'freq'
  )

trel



## neaten categories -------------------------------------------------------

trel$income.bucket %>% head() %>% str_replace('(\\.|X)+', ' ')

trel$income.bucket  <-
  trel$income.bucket |>
  str_replace('(\\.|X)+', ' ') |>
  trimws()  |>
    str_replace(
      '\\.', ' to '
  )

trel

# difference in how we can analyize: -------------------------------------

rel
trel


## what if we want the total by religion? -------------------------

# Tidy totals by religion:
trel %>%
  group_by(religion) %>%
  summarise(tot = sum(freq))
# a familiar synatax, we don't have to specify a column range.

# or if we want to keep the disaggregation by income:
trel %>%
  group_by(religion) %>%
  mutate(tot = sum(freq)) %>%
  ungroup()

# Untidy totals by religion:
rowSums(rel[2:ncol(rel)]) %>%
  set_names(rel[[1]])
# it's not too bad, but we need to know which columns we're summing by and
# specify explicitly (all but the first). But we also need another step to keep
# the category names.


# total by income bucket: -------------------------------------------------

# Tidy totals by religion:
trel %>%
  group_by(income.bucket) %>%
  summarise(tot = sum(freq))
# Nothing changes except the grouping variable.

# Untidy totals by income:
colSums(rel[2:ncol(rel)])
# we change which function we're using and specify a set of columns


# with tidy data, we can also add manipulations as new column onto the same
# table:
trel <-
  trel %>%
  group_by(religion) %>%
  mutate(total.by.religion =
           sum(freq)
         ,share.by.religion =
           freq / sum(freq)
         ) %>%
  ungroup()

trel %>%
  group_by(religion) %>%
  summarise(sum(share.by.religion))

# plotting: ---------------------------------------------------------------

# ggplot is oriented towards "tidy" data!
library(hrbrthemes)

# specify factor levels:
trel <- trel %>%
  arrange(total.by.religion) %>%
  mutate(religion =
           factor(religion,
                  levels =
                    unique(.$religion))
         )


trel <- trel %>%
  mutate(income.bucket =
           factor(income.bucket,
                  levels =
                    unique(trel$income.bucket)
                  ))

levels.plot <- trel %>%
  ggplot(
    aes(y = religion
        ,x = freq
        ,fill = income.bucket)
  ) +
  geom_col(position = position_stack(reverse = T)) +
  scale_fill_viridis_d(
    name = "Income range"
  ) +
  theme_ipsum(grid = 'X')

levels.plot

proportions.plot <-
  trel %>%
  ggplot(
    aes(y = religion
        ,x = share.by.religion
        ,fill = income.bucket
        #,color = income.bucket
        #,size = freq
        )
  ) +
  geom_col(position =
             #position_stack(reverse = T)
             position_dodge()
           ) +
  scale_fill_viridis_d(
    name = "Income range"
    ,aesthetics = c('color', 'fill')
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_ipsum(grid = 'X')

proportions.plot

## what are better ways of plotting this? -----------------------------------

# better, or more suited to specific data questions..
levels.plot

proportions.plot

trel %>%
  ggplot(
    aes( y = religion
        ,x = share.by.religion
        #,fill = religion
        ,fill = income.bucket
        #,size = freq
    )
  ) +
  geom_col(position =
            position_stack(reverse = T)
           ) +
  geom_text(
     data = filter(trel, share.by.religion > .05)
    ,aes(label = scales::label_percent(1)(share.by.religion) )
    ,position = position_stack(reverse = T
                               ,vjust = .5)
    ,show.legend = F
    ,color = "white"
    ,fontface = "bold"
    ,check_overlap = T
  ) +
  scale_fill_viridis_d(
    name = "Income range"

    ,aesthetics = c('color', 'fill')
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_ipsum(grid = 'X')
  # facet_wrap(vars(religion)
  #            )








