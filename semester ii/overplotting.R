library(tidyverse)


# too much data -----------------------------------------------------------

too.much <-
  tibble(x =  rnorm(2e4, mean =1, sd = 25)
         ,y = 1:2e4
         )

too.much <- too.much %>%
  mutate(z = x * y )

too.much
too.much %>% tail()



# binning -----------------------------------------------------------------


too.much %>%
  ggplot(
    aes(x = y
        , y = z)
  ) +
  #geom_point()
  geom_hex(
    #bins = 20 # larger bins
    bins = 200 # smaller bins
  ) +
  scale_fill_viridis_c()



# transparency/size -------------------------------------------------------


too.much %>%
  ggplot(
    aes(x = y
        , y = z)
  ) +
  geom_point(size = .1
             ,alpha = .1)



# probabilistic sampling ----------------------------------------------------------------

too.much %>%
  sample_frac(.5) %>%
  ggplot(
    aes(x = y
        , y = z)
  ) +
  geom_point(size = .4
             ,alpha = .4)


# resources from today ----------------------------------------------------

#' https://bookdown.org/nicohahn/making_maps_with_r5/docs/mapdeck.html#using-mapdeck-to-create-maps
#'
#' https://stackoverflow.com/questions/7714677/scatterplot-with-too-many-points
#'
#' mapgl - https://walker-data.com/mapgl/

