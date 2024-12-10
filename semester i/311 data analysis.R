library(tidyverse)

api.endpt.w.filters <-
  "https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$query=SELECT%0A%20%20%60unique_key%60%2C%0A%20%20%60created_date%60%2C%0A%20%20%60closed_date%60%2C%0A%20%20%60agency%60%2C%0A%20%20%60agency_name%60%2C%0A%20%20%60complaint_type%60%2C%0A%20%20%60descriptor%60%2C%0A%20%20%60location_type%60%2C%0A%20%20%60incident_zip%60%2C%0A%20%20%60facility_type%60%2C%0A%20%20%60status%60%2C%0A%20%20%60due_date%60%2C%0A%20%20%60resolution_description%60%2C%0A%20%20%60resolution_action_updated_date%60%2C%0A%20%20%60community_board%60%2C%0A%20%20%60bbl%60%2C%0A%20%20%60borough%60%2C%0A%20%20%60open_data_channel_type%60%2C%0A%20%20%60park_facility_name%60%2C%0A%20%20%60park_borough%60%2C%0A%20%20%60vehicle_type%60%2C%0A%20%20%60latitude%60%2C%0A%20%20%60longitude%60%2C%0A%20%20%60taxi_company_borough%60%0AWHERE%20%60created_date%60%20%3E%3D%20%222023-01-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0AORDER%20BY%20%60created_date%60%20DESC%20NULL%20FIRST"

# three11 <-
#   RSocrata::read.socrata(
#     #api.endpt.w.filters
#     "https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$query=WHERE created date > 2023-01-01T01:00:00.000"
#   )

three11 <-
  vroom::vroom(
    "~/R/local-data/311 complaints/311_Service_Requests_from_2010_to_Present_20241022.csv"
  )


three11 <- three11 %>% tibble()


# Getting rid of upper case and spaces will make our columns easier to deal with.
three11 <- three11 %>%
  rename_with(
    ~tolower(make.names(.x))
  )

# (alternatively:)
three11 <- three11 %>%
  rename_with(tolower) %>%
  rename_with(make.names)

three11


three11 <-
  three11 %>%
  filter(str_extract(created.date,
                     "\\d{4}") %in% c(2024)
         )

three11$created.date %>% nchar() %>% unique()


# peeks -------------------------------------------------------------------

three11 %>% colnames()
three11 %>% glimpse()

three11 %>% count(status)
three11 %>% count(landmark)
three11 %>% count(open.data.channel.type)

three11 <- three11 %>%
  select(-matches("street|address|landmark|bridge|road.ramp|taxi|location|plane"))


# filter to since 2022 ----------------------------------------------------

library(lubridate)

three11$created.date %>% class()

three11 %>% select(matches("date"))

three11$created.date %>%
  #head() %>%
  as.POSIXct(
    format =
      "%m/%d/%Y %I:%M:%S %p"
    ) %>%
  is.na() %>% sum()


# convert the date-times, which were read in as character strings, into
# date-times!
three11 <- three11 %>%
  mutate(across(matches("date")
         , ~as.POSIXct(
           .x,
           format =
             # this specifies the format the date is represented in, with %m as
             # shorthand for month, %Y as shorthand for Year, etc. But seriously
             # don't worry about this, we'll get to more data cleaning later.
             # And normally lubridate does this for you.
             "%m/%d/%Y %I:%M:%S %p")
  ))


three11 <- three11 %>%
  filter(year(created.date) > 2022)

three11 %>%
  count(year(created.date))

nrow(three11) / 1e6

three11$created.date %>% head() %>% year()

three11 %>% colnames()
three11 %>% glimpse()

three11 %>% count(borough)

three11 %>%
  count(agency, agency.name) %>%
  mutate(perc = n/sum(n)) %>%
  arrange(desc(n))

three11 %>%
  count(borough, agency, agency_name
        ) %>%
  mutate(perc = n/sum(n)
         ) %>%
  arrange(desc(n))


three11 %>% count(complaint_type) %>% arrange(desc(n))

three11 %>%
  filter(complaint_type == "Rodent")

# brainstorm questions and possibilities ----------------------------------

#' time series of when complaints are happening
#'
#' how long it takes to close complaints
#'
#'



# let's see when created times nad closed times are different -------------

head( )


