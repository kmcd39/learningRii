library(tidyverse)


# a basic Pivot -----------------------------------------------------

#' this is bundled with the `tidyr` package, part of the tidyverse:
pop <- tidyr::world_bank_pop

# we can look at the documentation for the dataset the same way we look at
# documentation for a function:
?tidyr::world_bank_pop

# Right now the dataset is long by country and indicator, and wide by year.

#' what if we want to reverse that? Get the table long by country and year, but
#' wide by indicator?
#'
#' That would make time-series analysis in R easier.
#'
#' We can pivot the table longer, by all year columns, using the special "any
#' number" regex character:


popl <- pop %>%
  pivot_longer(
    matches('\\d')
    ,names_to = 'year'
  )

popl

# NYC census race/ethnicity example --------------------------------------------

#' pull example data from ACS using `tidycensus`
#'
#' about the table: https://censusreporter.org/tables/B02001/
?tidycensus::get_acs

demos <-
  tidycensus::get_acs(
    geography = 'county'
    ,table = 'B02001'
    ,year = 2022
    ,state = 36
    ,survey = "acs5") %>%
  rename_with(tolower)

demos

# let's just use the 5 boros of NYC for simplicity -- we can use Regex to
# filter!
boros.regx <- 'Kings|Queens|^New York|Richmond|Bronx'
nydemos <- demos %>%
  filter( grepl(boros.regx, name) )

# make sure we did it right!
nydemos %>% count(geoid, name)

# we can also clean the Boro names... with regex!
nydemos$name %>% unique()
nydemos <- nydemos %>%
  mutate(name = str_extract(name, boros.regx))
# (can you think of a second way of doing this?)
nydemos

# get labels
acs.vars <-
  tidycensus::load_variables(
    year = 2022
    ,dataset = 'acs5')

acs.vars

# clean the labels using regex
acs.vars$label <- acs.vars$label %>%
  str_replace_all('!!', ' ') %>%
  str_replace_all('Estimate |:$|Total: ', '')

# for the tabel we pulled:
acs.vars %>% filter(grepl('^B02001', name))

# We can join the labels to the data now.
nydemos <- nydemos  %>%
  left_join(acs.vars[c('name', 'label')]
            ,by = c('variable' = 'name'))

nydemos

#' from the labels of the variable, we see that 001 is a total -- and 008 is a
#' subtotal of two subcategories of "two or more races"
#'
#' Having totals within a dataframe with the constituent parts can be confusing
#' and pose issues for plotting and analysis.
nydemos %>%
  ggplot(aes(fill = label
             ,y = estimate
             ,x = name)
  ) +
  geom_col()

# (what's the issue with this plot? How do we fix it?)


# (drop totals to avoid double-counting!)
nydemos %>%
  filter(
    !grepl('Total|Two or More Races: ',
           label)
  ) %>%
  ggplot(aes(fill = label
             ,y = estimate
             ,x = name)
  ) +
  geom_col()


# pull and peeks w school demographic data -----------------------------------

# from:
# https://data.cityofnewyork.us/Education/2019-20-Demographic-Snapshot-School/nie4-bv6q

# to install Rsocrata:
#install.packages("RSocrata")

url <-
  'https://data.cityofnewyork.us/resource/nie4-bv6q.csv'
#'https://data.cityofnewyork.us/api/views/nie4-bv6q/rows.csv?date=20240103&accessType=DOWNLOAD'
# read data for url; object is schd for school demographics

# this gets the data directly from the Open Data API, rather than first
# downloading the CSV, saving on the local computer, and reading that:
schd <-
  RSocrata::read.socrata( url )

schd <- schd %>% tibble()

schd


# quick peeks -------------------------------------------------------------
schd

colnames(schd)
glimpse(schd)

# check duplicates of school names or DBNs
schd %>%
  count(dbn, school_name) %>%
  map_dbl( ~sum(duplicated(.x)) )
# (some schools will share the same names)

# (we can peek at them this way)
schd %>%
  select(dbn, school_name) %>%
  distinct() %>%
  filter(school_name %in%
           .[duplicated(.$school_name), ]$school_name
  ) %>%
  arrange(school_name)

# let's say we're interested in race/ethnicity -- we can trim other columns for
# now for simplicity
schd %>% glimpse()

demo.regx <- 'asian|black|hispanic|multiple_race|white'

schd.trimmed <- schd %>%
  select(dbn, school_name, year, total_enrollment,
         matches( demo.regx))

schd.trimmed

## pivoting longer ---------------------------------------------------------


# the dataset has two columns for each demographic group - i.e., "asian" and
# "asian_1." From the documentation, we can see that the ones with the "_1"
# suffix show percent. We can check a few values by dividing the number by group
# by total enrollment to make sure this is right.

#' Right now the dataset is LONG by dbn (a school identifier), school_name, and
#' year. We might want it long by demographic as well, which would help us plot
#' in ggplot.
#'
#' To get it longer, of course we want to pivot long. There will be a few ways
#' of doing it: We could drop percent first, and then recalculate it once it's
#' long:
tmp <- schd.trimmed %>%
  select(-matches('_1$')) %>%
  pivot_longer(
    matches( demo.regx )
    ,names_to = 'demographic'
  ) %>%
  mutate(perc = value / total_enrollment)

tmp

#' or (this will be harder here but will show a technique that may be necessary
#' in other cases) --- we can pivot long by two categories, using a regex to
#' split the columns we're pivoting by:
#'
tmp <- schd.trimmed %>%
  pivot_longer(
    matches( demo.regx )
    # below matches two groups: the first is letters and underscore; the second
    # is either the end of the string or "_1" at the end of the string. We
    # separate the two matched identifiers to two new columns, for demographic
    # and the variable
    ,names_pattern = '([a-z_]+)($|_1$)'
    ,names_to = c('demographic', 'var')
  ) %>%
  mutate(var =
           case_when(
             # now we can rename the variable, which will show either count or
             # percent (depending on whether the "_1" was matched)
             var == '' ~ 'count'
             ,var == '_1' ~ 'perc'
           )
  )


# tidying with regex  -----------------------------------------


## separating columns w regex ----------------------------------------------

#' the package `untidydata` bundles "untidy datasets made for the purpose of
#' teaching the tidyverse."
#'
#' the `pre_post` dataset has a column spec, with "two grouping variables
#' separated by underscores"
?untidydata::pre_post

messy.data <- untidydata::pre_post
messy.data

# how do we get the g1/g2 and the hi/lo as two sets of variables?
separated.spec <- messy.data$spec %>%
  str_split_i('_')

separated.spec %>% head()

messy.data %>%
  mutate(gspec =
           map_chr(separated.spec, 1)
         ,spec2 =
           map_chr(separated.spec, 2)
         ,.after = spec
  )

