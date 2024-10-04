#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# DESC: Different methods for querying API
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: not well organized - pretty loose code
#-------- should be incoprorated into ablog at some point
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#sec: inputs====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(httr)
library(jsonlite)
library(tidyverse)

#sec: inputs====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#sec: inputs====
#sec: basic_query_workflow======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resp = GET(
  url, query = list(
    "$where" = "sideofstreet IN ('S')"
  ))

content = resp %>% content()

#show one query
content[[1]]

content_wide = content %>%
  enframe() %>%
  unnest_wider(value)

tt_c_wide %>%
  select(name, blockfacename,location) %>%
  hoist(location, "type") %>%
  #method_1
  # hoist(location
  #       ,lat = list('coordinates', 1)
  #       ,lon = list('coordinates', 2))
  hoist(location, "coordinates") %>%
  unnest_wider(coordinates, names_sep = "_")

tt_c_wide %>%
  select(name, blockfacename,location) %>%
  hoist(location, "type") %>%
  #method_1
  # hoist(location
  #       ,lat = list('coordinates', 1)
  #       ,lon = list('coordinates', 2))
  hoist(location, "coordinates")

tt_c_wide %>%
  select(name, blockfacename,location) %>%
  hoist(location, "coordinates") %>%
  #method_2.1
  # mutate(lat = map(coordinates, ~.x[[1]])) %>% unnest("lat")
  #method_2.2
  unnest(cols = "coordinates") %>%
  unnest(cols = "coordinates")


tt_c_wide %>%
  select(name, blockfacename,location) %>%
  #ethod_3
  hoist(location, "coordinates") %>%
  unnest_longer("coordinates")








resp_simp = GET(
  url, query = list(
    "$where" = "sideofstreet IN ('S')"
    ,"$select" = "sideofstreet"
  ))


tt_c_wide
unnest(cols = "location") %>%
  select(name:blockfacename, location)
select(!location) %>%
  data.frame()


response <- content(tt, as = "text", encoding = "UTF-8")

df <- fromJSON(response, flatten = TRUE) %>%
  data.frame()

