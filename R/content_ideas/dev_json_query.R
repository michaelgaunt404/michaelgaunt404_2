
https://dev.socrata.com/docs/queries/query
https://github.com/iqis/fountain

https://soda.demo.socrata.com/resource/4tka-6guv.json?$where=magnitude%20%3E%203.0


add_headers(a = 1, b = 2)

%>%
  paste0(., "?") %>%
  paste0(., "$select=paidparkingsubarea") %>%
  paste0(., "$where=sideofstreetIS'S'") %>%
  jsonlite::read_json()

str(tt) %>% head()
url <- "https://data.seattle.gov/resource/hiyf-7edq.json"
# url <- "https://soda.demo.socrata.com/resource/4tka-6guv.json"
# $where=magnitude > 3.0
tt = GET(url
         ,query = list(
           "$where" = "sideofstreet IN ('S')"
           ,"$select" = "sideofstreet"
         ))


tt = GET(url
         ,query = list(
           "$where" = "sideofstreet IN ('S')"
           ,"$select" = "sideofstreet"
         ))

tt_c = tt %>% content()

tt_c[[1]]

tt_c_wide = tt_c %>%
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





tt_c_wide
unnest(cols = "location") %>%
  select(name:blockfacename, location)
select(!location) %>%
  data.frame()


response <- content(tt, as = "text", encoding = "UTF-8")

df <- fromJSON(response, flatten = TRUE) %>%
  data.frame()

