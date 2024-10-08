

# install.packages("tidyverse")

library(tidyverse)


# load data ---------------------------------------------------------------


# i downloaded this dataset from NYC open data on 9/17/2024
trash.data <-
  read.csv("Recycling_Diversion_and_Capture_Rates_20240917.csv")

trash.data <- trash.data %>% tibble()

trash.data


trash.data %>%
  filter(Fiscal.Year == 2019) %>%
  pull(Diversion.Rate.Total..Total.Recycling...Total.Waste.) %>%
  summary()

trash.data %>% colnames()


trash.data %>% count(Fiscal.Year)

count(trash.data, Fiscal.Year)


1 %>% c(2,3)
c(1, 2,3)



# subsetting/indexing ----------------------------------------------------------------


## base R subsetting ---------------------------------------------------------

trash.data[1:10 , ]

trash.data[20:30 , ]

# select rows where Fiscal year is 2017, and keep only District and Month number
# columns
trash.data[trash.data$Fiscal.Year == 2017 ,
           c("District", "Fiscal.Month.Number") ]


trash.data$Fiscal.Year


## tidyverse subsetting ------------------------------------------------------

trash.data %>%
  filter(Fiscal.Year == 2017) %>%
  select(District, Fiscal.Month.Number)




# some practice -----------------------------------------------------------

trash.data %>% colnames()
trash.data

trash.data %>% count(Zone)

trash.data %>% count(District)

trash.data %>% count(Fiscal.Year)


trash.data$Diversion.Rate.Total..Total.Recycling...Total.Waste. %>% mean()
trash.data$Diversion.Rate.Total..Total.Recycling...Total.Waste. %>% median()

#' what if we want to get the average, by year, or by district, or by both?
#'

trash.data %>%
  group_by(Fiscal.Year) %>%
  summarise(mean(Diversion.Rate.Total..Total.Recycling...Total.Waste.))



# (count is actually a short-cut for group_by %>% summarise(n())
trash.data %>%
  count(Fiscal.Year)

trash.data %>%
  group_by(Fiscal.Year) %>%
  summarise( n() )
