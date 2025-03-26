library(tidyverse)




# pull data ---------------------------------------------------------------

#' let's use this arms transfer database from the Stockholm International Peace
#' Research Institute
#'
#' https://www.sipri.org/databases/armstransfers
#'
#'

pth <- "local-data/semester ii/sipri-full-trade-register.csv"

sipri <-
  vroom::vroom(pth
               ,skip = 11) |>
  tibble()



sipri

## peeks -------------------------------------------------------------------

sipri
vroom::problems(sipri)

sipri


## cleans and tidies -------------------------------------------------------

sipri <-
  sipri |>
  rename( "order.yr.marker" = 4
          ,number.ordered.marker = 7
          ,number.delivered.marker = 10) |>
  rename_with(make.names) |>
  rename_with(tolower)

sipri

# peek NAs
sipri |>
  filter(
    if_any(c( recipient, supplier, year.of.order)
           ,is.na)
  )

# drop em
sipri <- sipri |>
  filter(!is.na(year.of.order))


# where order tiv does not match weapon tiv
sipri |>
  filter(sipri.tiv.for.total.order !=
           sipri.tiv.of.delivered.weapons) |>
  select(1:3,# :5,
         matches("sipri.tiv"),
         everything()) |>
  arrange(desc(sipri.tiv.for.total.order)) # |> View()



## subsetting data ---------------------------------------------------------


### filter to a year range --------------------------------------------------

sipri$year.of.order |> summary()


sipri.trimmed <- sipri |>
  filter(year.of.order > 2011)



### total providers + recipients --------------------------------------------

total.providers <- sipri.trimmed |>
  group_by(country = supplier) |>
  summarise(across(c(sipri.tiv.for.total.order, sipri.tiv.of.delivered.weapons)
                   ,~sum(.x, na.rm = T))
            ) |>
  arrange(desc(sipri.tiv.for.total.order)) |>
  rename(total.tiv = 2, weapon.tiv = 3)


total.recipients <- sipri.trimmed |>
  group_by(country = recipient) |>
  summarise(across(c(sipri.tiv.for.total.order, sipri.tiv.of.delivered.weapons)
                   ,~sum(.x, na.rm = T))
  ) |>
  arrange(desc(sipri.tiv.for.total.order)) |>
  rename(total.tiv = 2, weapon.tiv = 3)

total.both <-
  total.providers |>
  mutate(direction = "supplied"
         ,.before = everything()) |>
  rbind(
    total.recipients |>
      mutate(direction = "received"
             ,.before = everything())
  )

total.both

# focus countries (when we're trimming)

focus.countries <-
  unique(
    c( head(total.recipients$country, 5)
       ,head(total.providers$country, 5) )
  )

focus.countries

apply.focus.countries <- function(x) {

  x |>
    filter(
      country %in% focus.countries) |>
    mutate(
      country = factor(country, levels = focus.countries)
      )
}

# VISUALS -----------------------------------------------------------------


# more basic --------------------------------------------------------------

tmp <- total.providers |>
  apply.focus.countries()


# ewwww a bar plot!!!
tmp.plot <- tmp |>
  ggplot(aes(
    x = country
    , y = total.tiv / 1e3
    ,fill = total.tiv / 1e3
  )
  ) +
  geom_col() +
  theme_minimal( ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom"
    ,
  ) +
  labs(title = "The US dwarfs all other countries as a supplier of weapons"
       ,subtitle = "Value of all weapon transfers, in standardized values, from 2001-2023."
       ,fill = "Total value (TIV, thousands)"
       ,caption = str_wrap(
         width = 90
         ,"Source: SIPRI Arms Transfers database. Units are trend-indicator value (TIV),
which is based on the known unit production costs of a core set of weapons and is intended to represent the transfer of military resources rather than the financial value of the transfer.")
  )


tmp.plot

## circular plots ----------------------------------------------------------


#' i bet you've heard of a bar plot, but have you ever heard of a... CIRCULAR BAR PLOT??
#'

tmp.plot <- tmp.plot +
  coord_polar(clip = "off") +
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  )

tmp.plot



# what about a radar plot.. -----------------------------------------------

#' well. They are pretty. but have a lot of criticism, see:
#'
#' https://www.data-to-viz.com/caveat/spider.html
#'
#' Probably for these flaws, they are not straightforward to implement. But, as
#' a start:

### using ggradar

# devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

total.both |>
  filter(country %in%
           c(as.character(tmp$supplier), "Saudi Arabia", "India")
         ) |>
  select(1:3) |>
  pivot_wider(names_from = "country"
              ,values_from = "total.tiv") |>
  # mutate(country = factor(country, levels = tmp$supplier)
  #        ) |>
  ggradar::ggradar(
    grid.max = max(total.both$total.tiv)
    ,grid.mid = mean(total.both$total.tiv)
  )



## two substitutes... ------------------------------------------------------

# another polar coordinate column plot, this time with a fill aesthetic too
total.both |>
  apply.focus.countries() |>
  ggplot(
    aes(x = country
        ,fill = direction
        ,y = total.tiv )
  ) +
  geom_col( #position = "identity"
           ,alpha = .8) +
  scale_fill_brewer(
    type = "qual"
  ) +
  coord_polar() +
  theme_minimal() +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom"
    ,
  ) +
  labs(title = "The US dwarfs all other countries as a supplier of weapons"
       ,subtitle = "Value of all weapon transfers, in standardized values, from 2001-2023."
       ,fill = "Total value (TIV, thousands)"
       ,caption = str_wrap(
         width = 90
         ,"Source: SIPRI Arms Transfers database. Units are trend-indicator value (TIV),
which is based on the known unit production costs of a core set of weapons and is intended to represent the transfer of military resources rather than the financial value of the transfer.")
  )

# but many of the radar plot issues apply here too!


# a loliplot ----------------------------------------------

#' a lollipop plot can be good for showing both levels and differences at once.
#'
#' it's not clear it's appropriate for this data, but as a quick example:


tmp <- total.both |>
  apply.focus.countries() |>
  select(-weapon.tiv)

tmp |>
  ggplot(
    aes(y = country
        )
  ) +
  geom_segment(
    data =
      pivot_wider(tmp,
                  names_from = "direction"
                  ,values_from = "total.tiv"
      )
    ,aes(xend = supplied, x = received)
  ) +
  geom_point(
    aes(x = total.tiv
        ,color = direction)
    ,size = 3
  ) +
  scale_color_brewer(
    type = "qual"
  ) +
  theme_minimal()

# #install.packages("ggplot2movies")
# if (require("ggplot2movies")) {
#   movies <- movies
#   movies$rrating <- cut_interval(movies$rating, length = 1)
#   movies$budgetq <- cut_number(movies$budget, 4)
#
#   doh <- ggplot(movies, aes(x = rrating, fill = budgetq))
#
#   # Wind rose
#   doh + geom_bar(width = 1) + coord_polar()
#   # Race track plot
#   doh + geom_bar(width = 0.9, position = "fill") + coord_polar(theta = "y")
# }

# for distributions... ----------------------------------------------------



## ridgelines --------------------------------------------------------------



## violin ------------------------------------------------------------------


## sinaplot ---------------------------------------------------------------

total.recipients

# total transfers to saudi arabia by value and country...
tmp <- sipri.trimmed |>
  filter(

    #grepl("Saudi Arabia|Israel|Palestine|Congo|DRC", recipient)
    recipient %in% c( head(total.recipients$country, 20))
         ) |>
  mutate(recipient =
           case_when(
              grepl("Congo", recipient) ~ "DR Congo"
             ,grepl("Palestine", recipient) ~ "Palestine"
             ,.default = recipient
             )
           ) |>
  mutate(recipient = factor(recipient, levels = head(total.recipients$country, 20))
         ) |>
  arrange(recipient, sipri.tiv.for.total.order)

# add a cumulative sum... and cumulative % sum
tmp <- tmp |>
  mutate( cum.sum = cumsum(sipri.tiv.for.total.order)
         ,ptile = percent_rank(sipri.tiv.for.total.order)
         )

tmp |>
  mutate(
    country.recode =
      factor(supplier,
             levels = #focus.countries
               head(total.providers$country)
             )
  ) |>
  mutate(row = !recipient %in% head(total.recipients$country, 10)
         ) |>
  ggplot(
    aes(
     color = country.recode
     #,fill = country.recode
    ,size = sipri.tiv.for.total.order
    ,x = recipient
    , y =
      sipri.tiv.for.total.order
      #ptile
    ,group = recipient
    ,text = paste(
      year.of.order,
      "\n", number.ordered, number.ordered.marker, weapon.description
      ,"\nFrom:", supplier
    )
    #,weight = sipri.tiv.for.total.order
      )
    ) +
  geom_violin(
    show.legend = F
  ) +
  ggforce::geom_sina(
    position = "identity"
    #,scale = T
    #alpha = .8
  ) +
  scale_color_brewer(
    type = "qual"
    ,palette = "Dark2"
    ,na.value = "grey35"
    #,aesthetics = c("color" , "fill")
  ) +
  scale_alpha_continuous(
    range = c(.1,.7)
  ) +
  scale_size_continuous(
    range = c(1,4)
    ,guide = "none"
  ) +
  facet_wrap(vars(row)
               ,ncol = 1, scales = "free_x"
               ) +
  labs(y = "Total TIV (log scale)"
       ,x = NULL
       ,color = "Supplier Country: "
       ,title = "Distributions of transfers to top 20 recipients of weaponry, 2001-2023."
       ,subtitle =
         str_wrap(width = 110,
         "Each bubble represents a weapon transfer to the below country; bubbles are colored based on the arms-supplying country and sized based on the size of the transfer.")
       ) +
  scale_y_log10() +
  theme_minimal() +
  guides(
    color = guide_legend(nrow = 1
                         ,override.aes = list(size = 4))
  ) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank() ,
        legend.position = "top")

plotly::ggplotly(tooltip = "text")




# other cool ones.. -------------------------------------------------------


## streamgraphs ------------------------------------------------------------

# install.packages("ggstream")
library(ggstream)
# (i'm not sure i see these as better than well-stylized area plots...)

tmp <- sipri |>
  # take a longer time period, which can be more appropriate for these types of plots.
  filter(year.of.order >= 1970) |>
  group_by(recipient, year.of.order
           ) |>
  summarise(tot.tiv = sum(sipri.tiv.for.total.order, na.rm = T)
            ) |>
  ungroup() |>
  filter(recipient %in%
           total.recipients$country[1:8]
         )


total.both


tmp |>
  ggplot(
    aes(x =year.of.order
        ,y = tot.tiv
        ,fill = recipient
        ,label = recipient)
  ) +
  ggstream::geom_stream() +
  ggstream::geom_stream_label(
    #data = tmp |> filter(recipient %in% total.recipients$country[1:10])

  ) +
  scale_fill_brewer(
    type = "qual"
  ) +
  labs( x = "order"
       ,y = "TIV"
       ,fill = NULL
       ) +
  theme_minimal()
  theme(legend.position = "bottom")


plotly::ggplotly()



## so, well-stylized area plot! --------------------------------------------

# combine totals in both directions by year.
sipri$sipri.tiv.for.total.order |> summary()

begin.year <- 1960

tots.by.year <- sipri |>
  filter(year.of.order >= begin.year
         ) |>
  group_by(year.of.order, country = recipient
           ,direction = "received"
           ) |>
  summarise(total.tiv = sum(sipri.tiv.for.total.order)
            ) |>
  ungroup() |>
  rbind({
    sipri |>
      filter(year.of.order >= begin.year
      ) |>
      group_by(year.of.order, country = supplier
               ,direction = "supplied"
      ) |>
      summarise(total.tiv = -1 * sum(sipri.tiv.for.total.order)
      ) |>
      ungroup()
  })

tots.over.period <- tots.by.year |>
  group_by(country) |>  #, direction
  summarise(total.tiv = sum(abs(total.tiv))
            ) |>
  ungroup() |>
  arrange(desc(total.tiv))

tots.over.period$total.tiv |> summary()
tots.over.period |> filter(total.tiv > 400) |> count(country)

#tots.over.period |> View()
tots.over.period$country |> head(20)

tmp <- tots.by.year |>
  mutate(focus.country =
           factor(country, levels =
                    tots.over.period$country[1:8])
         )

tots.over.period$total.tiv |> quantile(seq(0,1,.05))

tmp <- tmp |>
  filter(country %in%
           # filter(tots.over.period
           #        ,total.tiv > 1200)$country
         tots.over.period$country[1:50]
         )

#install.packages("zoo")


tmp |>
  filter(total.tiv != 0
         ) |>
  ggplot(
    aes( x = year.of.order
        ,fill = focus.country
        #,alpha = direction
        ,group = country
        ,y = total.tiv)
  ) +
  geom_area(
    # data = filter(tmp, direction == "received"),
    color = "white"
    ,linewidth = .1
  ) +

  facet_wrap(
    vars(direction)
    ,ncol = 1
    ,scale = "free_y"
  ) +
  scale_fill_brewer(
    type = "qual"
    ,na.value = "grey65"
    ,palette = "Dark2"
  ) +
  # scale_alpha_manual(
  #   values = c(supplied = 1
  #              ,received = .8)
  #   ,guide = "none"
  # ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_blank()
    ) +
  labs(fill = NULL
       ,title = "Value of arms transfers, by receiving country (top) or supplying country (bottom)")


# plotly::ggplotly()


## radar plots -------------------------------------------------------------




# network viz -------------------------------------------------------------



## chord diagrams ----------------------------------------------------------


# https://r-graph-gallery.com/chord-diagram.html

# package options?
# devtools::install_github("mattflor/chorddiag")
#install.packages("ggbio")

recent.flows <- sipri.trimmed |>
  filter(year.of.order %in% 2019:2023) |>
  group_by(supplier, recipient) |>
  summarise(total.tiv = sum(sipri.tiv.for.total.order)
            ) |>
  ungroup()

recent.top.either.direction <-
  recent.flows |>
  pivot_longer(1:2
               ,values_to = "country") |>
  group_by(country) |>
  summarise(total.tiv = sum(total.tiv)) |>
  arrange(desc(total.tiv)) |>
  head(20)

recent.top.either.direction

recent.top.recipients <- recent.flows |>
  group_by(recipient) |>
  summarise(total.tiv = sum(total.tiv)) |>
  arrange(desc(total.tiv)) |>
  head(20)

recent.top.recipients

focus.countries2 <-
  c(# recent.top.either.direction$country[1:7]
    "United States", "Russia", "China",
    recent.top.recipients$recipient[1:4]
    #"Israel", "Saudi Arabia", "Iran", "South Korea"
    ) |>
  unique()

focus.countries2 <- focus.countries2 |>
  factor(levels = c(focus.countries2, "Rest of the World"))

# group countries outside of focus countries to other.
recent.flows <-
  recent.flows |>
  mutate(across(c(supplier, recipient)
                ,~factor(if_else(.x %in% focus.countries2
                          ,.x, "Rest of the World")
                ,levels = levels(focus.countries2))
                )
  ) |>
  group_by(supplier, recipient) |>
  summarise(total.tiv = sum(total.tiv)
  ) |>
  ungroup()

# the function requires a matrix..
recent.flows.w <- recent.flows |>
  arrange(supplier, recipient
          ) |>
  pivot_wider(names_from = recipient
              ,values_from = total.tiv)

setdiff(colnames(recent.flows.w), recent.flows.w$supplier)

recent.flows.w <-
  recent.flows.w |>
  select(1, all_of(levels(focus.countries2))) |>
  arrange(supplier)

fmat <- as.matrix(recent.flows.w[,2:ncol(recent.flows.w)])
row.names(fmat) <- recent.flows.w[[1]]


chorddiag::chorddiag(
   fmat
  ,type =
    #"bipartite"
    "directional"
)



# questions in class ------------------------------------------------------

sipri$year.of.order %>% range()
total.recipients

sipri %>% count()


# let's zero in on india and pakistan (and sri lanka?) -------------------------------------

# sipri %>% View()

tmp <-
  sipri %>%
  filter(
    grepl("India|Pakistan|Sri Lanka", recipient , ignore.case = T)
      #grepl("India|Pakistan|sri lanka", supplier )
            )

tmp <- tmp %>%
  filter(year.of.order >= 1970
         )

tmp %>% count(recipient)

tmp %>% glimpse()
tmp %>% colnames()

tmp2 <-
  tmp %>%
  group_by(recipient, year.of.order
           #,supplier
           ) %>%
  summarise(tot.tiv = sum(sipri.tiv.for.total.order)
            ) %>%
  ungroup()

# what is happening in 2001?
tmp %>%
  filter(year.of.order == 2001
         ) %>%
  arrange(desc(sipri.tiv.for.total.order)
          ) %>% View()


tmp2 %>%
  ggplot(
    aes(
       x = year.of.order
      ,y = tot.tiv
      ,color = recipient
      )
    ) +
  geom_line() +
  geom_vline(xintercept = 2001
             ,linetype = "dashed"
             ,linewidth = .5
             )

plotly::ggplotly()





