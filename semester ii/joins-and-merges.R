library(tidyverse)

rm(list = ls())

# resources ---------------------------------------------------------------

#' also see the Joins section in the R textbook!
#'
#' https://r4ds.hadley.nz/joins.html
#'
#' A motivating OECD data vis example:
#'
#' https://ourworldindata.org/grapher/life-expectancy-vs-health-expenditure


# read OECD data ----------------------------------------------------------
getwd()
# where the data is saved
ddir <- '~/R/local-data/OECD/'

# filenames ending in "csv" in that directory
fns <- ddir %>% list.files(pattern = 'csv$')
fns

# read them in
# source: https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CEconomy%23ECO%23%7CNational%20accounts%23ECO_NAD%23&fs[1]=Topic%2C2%7CEconomy%23ECO%23%7CNational%20accounts%23ECO_NAD%23%7CGDP%20and%20non-financial%20accounts%23ECO_NAD_GNF%23&pg=0&fc=Topic&snb=53&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_NAMAIN10%40DF_TABLE1_EXPENDITURE_HCPC&df[ag]=OECD.SDD.NAD&df[vs]=1.0&pd=1990%2C2022&dq=A.AUS%2BAUT%2BBEL%2BCAN%2BCHL%2BCOL%2BCRI%2BCZE%2BDNK%2BEST%2BFIN%2BFRA%2BDEU%2BGRC%2BHUN%2BISL%2BIRL%2BISR%2BITA%2BJPN%2BKOR%2BLVA%2BLTU%2BLUX%2BMEX%2BNLD%2BNZL%2BNOR%2BPOL%2BPRT%2BSVK%2BSVN%2BESP%2BSWE%2BCHE%2BTUR%2BGBR%2BUSA...B1GQ_POP.......&to[TIME_PERIOD]=false
gdp <-
  paste0(ddir, "oecd gdp per cap usd.csv") %>%
  read.csv() %>% tibble()

# source: https://data-explorer.oecd.org/vis?fs[0]=Topic%2C0%7CHealth%23HEA%23&pg=20&fc=Topic&bp=true&snb=36&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HEALTH_STAT%40DF_LE&df[ag]=OECD.ELS.HD&df[vs]=1.0&pd=1990%2C2022&dq=AUS%2BAUT%2BBEL%2BCAN%2BCHL%2BCOL%2BCRI%2BCZE%2BDNK%2BEST%2BFIN%2BFRA%2BDEU%2BGRC%2BHUN%2BISL%2BIRL%2BISR%2BITA%2BJPN%2BKOR%2BLVA%2BLTU%2BLUX%2BMEX%2BNLD%2BNZL%2BNOR%2BPOL%2BPRT%2BSVK%2BSVN%2BESP%2BSWE%2BCHE%2BTUR%2BGBR%2BUSA%2BIND%2BIDN%2BPER%2BROU%2BRUS%2BZAF%2BHRV%2BCHN%2BBGR%2BBRA%2BARG.A.LFEXP..Y0._T.......&to[TIME_PERIOD]=false&vw=ov&lb=nm
lxp <-
  paste0(ddir, "oecd life exp.csv") %>%
  read.csv() %>% tibble()

# initial peeks and trims ----------------------------------------------------

lxp %>% glimpse()

# trim useless columns
lxp %>%
  select( where( ~length(unique(.x)) == 1) ) %>%
  glimpse()

lxp <- lxp %>%
  select( where( ~length(unique(.x)) > 1))

gdp %>%
  select( where( ~length(unique(.x)) == 1)) %>%
  glimpse()

gdp <- gdp %>%
  select( where( ~length(unique(.x)) > 1))

lxp %>% glimpse()
gdp %>% glimpse()


# plot theme --------------------------------------------------------------

plot.theme <- function() {
  hrbrthemes::theme_ipsum()
}


# life expectancy alone ---------------------------------------------------

lxp %>% glimpse()
lxp %>% count(AGE, Age, Sex)
lxp %>% count(REF_AREA, Reference.area)

lxp %>%
  filter(AGE == "Y0"
         ) %>%
  ggplot(
    aes(
       x = TIME_PERIOD
      ,y = OBS_VALUE
      ,colour = Sex
      ,group = paste0(Reference.area, SEX)
    )
  ) +
  geom_point(
    size = .01
  ) +
  geom_line(
    linewidth = .1
  ) +
  facet_wrap(vars(Measure)
             ,scales = "free_y"
             ,ncol = 1)

# plotly::ggplotly()


## gdp alone ---------------------------------------------------------------

gdp %>% glimpse()
gdp %>% count(SECTOR, TRANSACTION)
gdp %>% count(SECTOR, Institutional.sector, TRANSACTION, Transaction)
gdp %>% count(Institutional.sector , Transaction, Expenditure)

gdp %>%
  filter(SECTOR == "S14") %>%
  filter(TRANSACTION == "P3_POP"
         ) %>%
  ggplot(
    aes(x = TIME_PERIOD
        , y = OBS_VALUE
        ,color = REF_AREA != "CHN"
        ,group = REF_AREA)
  ) +
  geom_line(
    linewidth = .1
  ) +
  geom_point(size =.1) +
  plot.theme() +
  theme(legend.position = "none")

#' `plotly` can be useful to quickly make interactive plots.
#'
#' use `plotly::ggplotly` for interactive ggplots :)
# plotly::ggplotly()




# combining the datasets! ------------------------------------------------------------------


## joining -----------------------------------------------------------------

#' (here is where we left off!)

lxp %>%
  left_join(gdp)





# what do we notice about the two datasets? (think about the column names, the
# data structure.)
lxp %>% glimpse()
gdp %>% glimpse()

# ...


#' They have identical column names.
#'
#' They're both long by country and year.
#'
#' The other variable (gdp per capita or life expectancy) is always called
#' "OBS_VALUE".

lxp %>% count(OBS_STATUS, Observation.status)
gdp %>% count(OBS_STATUS, Observation.status)

## row binding -------------------------------------------------------------

#' We can combine in two ways... because the data has identical column names,
#' we can add a new column to each that indicate the other variable, and then
#' ROW BIND, which basically stacks the datasets on top of one another.

lxp <- lxp %>% mutate(var = 'life expectancy')
gdp <- gdp %>% mutate(var = 'GDP per capita')

lxp.gdp <- lxp %>%
  rbind(gdp)

# notice the number of rows in the new table is equal to the number of rows in
# the previous two combined.
lxp.gdp %>% dim()
lxp %>% dim()
gdp %>% dim()

lxp.gdp %>% count(var)
# what is the data structure of the new table?
lxp.gdp

## merging -----------------------------------------------------------------

#' more frequently, the data will have some matching columns, but not ALL
#' matching columns as above.
#'
#' Let's simulate that by renaming the OBS_VALUE column for each.

lxp <- lxp %>%
  pivot_wider( names_from = var ,
               values_from = OBS_VALUE)
gdp <- gdp %>%
  pivot_wider( names_from = var ,
               values_from = OBS_VALUE)

lxp
gdp

#' to merge... there are some mistakes that are easy to make but also relatively
#' easy to avoid!
#'
#' We typically need at least one column where we want to join by common
#' values...
#'
#' In this example, we'll expect at least two: we want to join by COUNTRY and
#' YEAR.
#'
#' Joins can be one-to-one or many-to-many. For example, if we're joining a
#' table that has 1 row/country to one with 5 rows/country, by only the one
#' country column, we'll have a new table that's bigger than the first, with
#' rows from the first table repeated for every match in the second table.
#'
#' The can be good deliberate sometimes but unexpected other times! (as with
#' many things, it's good to make sure everything went as expected even if you
#' get an error!)

# so our first join attempt:
gdp.lxp.tmp <- lxp %>%
  left_join(gdp)

gdp.lxp.tmp
# when we don't specify which columns to join by, it'll try all of the columns in
# common....

# after we tried to join in GDP, we have 1562 NAs for it!!!! What might be going on?
gdp.lxp.tmp$`GDP per capita` %>% summary()

# ...

#' Maybe it'll go better if we don't try to merge by the OBS_STATUS columns!!
#' Notice these are marked slightly differently in the two tables! And we
#' wouldn't want to join based on estimate status anyway.
lxp %>% count(OBS_STATUS, Observation.status)
gdp %>% count(OBS_STATUS, Observation.status)


# so let's specify which columns to join by..
gdp.lxp <- lxp %>%
  left_join(gdp
            ,by = c("REF_AREA", "Reference.area", "TIME_PERIOD")
            )

# lets also do our make.names/tolower
gdp.lxp <- gdp.lxp |>
  rename_with( ~make.names(tolower(.x)) )

# there are still some NAs, but we did start with a different selection of
# countries..
gdp.lxp$gdp.per.capita %>% summary()

# we can look at countries missing from the merged data...
gdp.lxp %>%
  filter(is.na(gdp.per.capita)) %>%
  count(ref_area, reference.area) %>%
  arrange(ref_area)

# ...and those that existed in one of our datasets but not the other
lxp %>%
  filter(! REF_AREA %in%
           gdp$REF_AREA) %>%
  count(REF_AREA, Reference.area) %>%
  arrange(REF_AREA)

# We also know that some years are missing for GDP, which accounts for the
# remaining NAs.


# analyzing together! -----------------------------------------------------

gdp.lxp

gdp.lxp %>%
  filter(time_period == 2021) %>%
  ggplot(
    aes(x = gdp.per.capita
        ,y = life.expectancy)
  ) +
  geom_smooth(method = 'lm') +
  geom_point() +
  plot.theme()

## connected time series scatter plot... -----------------------------------

# minimalist first try.
gdp.lxp %>%
  ggplot(
    aes(x = gdp.per.capita
        ,y = life.expectancy
        ,group = reference.area
        ,label = time_period)
  ) +
  geom_path()

plotly::ggplotly()

### how can we clean this up? ----------------------------------------------------

gdp.lxp %>% count(ref_area, reference.area)

gdp.lxp %>%
  filter(time_period == 2021) %>%
  arrange(desc(gdp.per.capita))

# trim Luxembourg and Ireland (a quick google search yields some articles that
# say Ireland GDP per cap is misleadingly inflated, and Luxembourg is a very
# small, very wealthy country.)
outlier.countries <- gdp.lxp %>%
  filter(time_period == 2021) %>%
  filter(gdp.per.capita > 90e3)

# trim those (what does the ! mean?)
tmp <- gdp.lxp %>%
  filter(! ref_area %in%
           outlier.countries$ref_area)

# also add an identifier to see how a few specific countries are doing

tmp |>
  filter(
    grepl("United States|China|Spain|Korea|Mexico", reference.area)
  ) |>
  count(reference.area)

tmp <- tmp %>%
  mutate(focus.country =
           str_extract(reference.area, "United States|China|Spain|Korea|Mexico"))

tmp %>%
  filter(time_period <= 2021) %>%
  arrange(ref_area, time_period
          ) %>%
  ggplot(
    aes(x = gdp.per.capita
        ,y = life.expectancy
        ,group = reference.area
        ,alpha = focus.country
        ,color = focus.country
        #if_else(is.US, 1, .2)
        ,label = time_period
    )
  ) +
  geom_path(
    #linewidth = 1.1
  ) +
  geom_point(
    data = filter(tmp,
                  time_period == 2021)
  ) +
  scale_alpha_manual(
    values = rep(.8, 5),
    na.value = .2
  ) +
  scale_color_manual(
    values = visaux::jewel.pal()
    ,na.value = "grey35"
  ) +
  plot.theme()

plotly::ggplotly()


# indexed time series is an alternative to this approach ------------------

# tmp |> View()

tmpl <-
  tmp |>
  pivot_longer(
     c("life.expectancy", "gdp.per.capita")
     ,names_to = "var"
     ,values_to = "value"
  ) |>
  filter(!is.na(value)
         ) |>
  arrange(ref_area, time_period) |>
  group_by(ref_area, var) |>
  mutate(index =
           100 * value / head(value, 1)
         ) |>
  ungroup()

tmpl |> glimpse()
# how do we make the plot?



## (answer) ----------------------------------------------------------------


tmpl |>
  ggplot(
    aes(
      x = time_period
      ,y = index
      ,group = reference.area
      ,alpha = focus.country
      ,color = focus.country
  )) +
  geom_path() +
  geom_hline(yintercept = 100
             ,linetype = "dashed") +
  scale_alpha_manual(
    values = rep(.8, 5),
    na.value = .1
  ) +
  scale_color_manual(
    values = visaux::jewel.pal()
    ,na.value = "grey35"
  ) +
  facet_wrap( vars(var)
              ,ncol = 1
              ,scales = "free_y"
             ) +
  plot.theme()

