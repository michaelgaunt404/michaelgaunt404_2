

library(tidyverse)
library(httr)
library(RProtoBuf)
library(jsonlite)

ls("RProtoBuf:DescriptorPool")
# say what is a desctipter pool here

# Read the proto definition
ls("RProtoBuf:DescriptorPool")

proto_file = "https://gtfs.org/documentation/realtime/gtfs-realtime.proto"
download.file(proto_file, destfile = "gtfs-realtime.proto")

readProtoFiles("gtfs-realtime.proto")
ls("RProtoBuf:DescriptorPool")


# Load a GTFS-realtime feed (example)
feed_url <- "http://s3.amazonaws.com/commtrans-realtime-prod/tripupdates.pb"
key = "61762e745cf44d5aa4fff468abe82758"
feed_url = stringr::str_glue("https://api.wmata.com/gtfs/bus-gtfsrt-tripupdates.pb?api_key={key}")
feed_url = base_vp
# feed_url = str_glue("https://api.wmata.com/gtfs/bus-gtfs-static.zip?api_key={key}")
{
  resp_qa = GET(base_vp_qa)
  resp = GET(base_vp)
  feed_data_2 = resp %>%  content("raw")
  feed_data_1 = readBin(url(feed_url, "rb"), "raw", n = 1e9)
  identical(feed_data_1, feed_data_2)
  feed_data_1 %>% .[1:100]
  resp %>% content("raw") %>% .[1:100]
  
  str(feed_data_1)
  str(feed_data_2)
  
  gtfs_feed = P("transit_realtime.FeedMessage")$read(feed_data_2)
  resp$status_code
  print(as.numeric(resp$headers$`content-length`)/1000000)
  gtfs_feed_json
}
# Explore the feed c

gtfs_feed %>% str()
gtfs_feed %>% toJSON() %>% str()
gtfs_feed %>% toJSON() %>% jsonlite::fromJSON() %>% str() 
gtfs_feed$toJSON() %>% jsonlite::fromJSON() %>% str() 
gtfs_feed_json = gtfs_feed$toJSON() %>% jsonlite::fromJSON()


gtfs_feed_json %>% 
  .[['entity']] %>% 
  rename("id_id" = id) %>%
  unnest_wider(tripUpdate) %>%
  unnest_wider(trip) %>%
  unnest_wider(vehicle) %>%
  rename("veh_id" = id) %>%
  # rename_with(~stcr_replace(.x, "Id", "_id")) %>% 
  janitor::clean_names() %>% 
  unnest_longer(stop_time_update) %>%
  .[1,"stop_time_update"] %>% 
    unnest_wider(stop_time_update) 


gtfs_feed_json %>% 
  .[['entity']] %>% 
  rename("id_id" = id) %>%
  unnest_wider(tripUpdate) %>%
  unnest_wider(trip) %>%
  unnest_wider(vehicle) %>%
  rename("veh_id" = id) %>%
  # rename_with(~stcr_replace(.x, "Id", "_id")) %>% 
  janitor::clean_names() %>% 
  unnest_longer(stop_time_update) %>%
  .[1:20,] %>%
  .[1:2,] %>%
  # .[3,] %>% 
  unnest_longer(stop_time_update) %>%
  unnest_wider(arrival) %>%
  
  (stop_time_update, names_sep = "_") %>%
  select(trip_id, timestamp, route_id, direction_id
         ,starts_with("stop_time_update")) %>% 
  # glimpse()
  # mutate(
  #   stop_time_update_stopSequence = map(stop_time_update_stopSequence, unlist) 
  # ) %>% 
  unnest_longer(stop_time_update_stopSequence) %>%
  unnest_longer(stop_time_update_stopId ) 
  unnest_wider(
    stop_time_update_stopId
    ,names_sep = "_") %>% 
  pivot_longer(cols = starts_with("stop_time_update_"))
  unnest_wider(stop_time_update_departure
               ,names_sep = "_") %>%
  # select(!name) %>%
  select(sort(colnames(.)))
  
  
  glimpse()
  
  # enframe() %>%
glimpse()

tripUpdate.trip.

temp = temp_data_c %>%
  .[['entity']] %>%
  enframe() %>%
  unnest_wider(value) %>%
  rename("id_id" = id) %>%
  unnest_wider(vehicle) %>%
  unnest_wider(trip) %>%
  # select(vehicle) %>%
  unnest_wider(vehicle) %>%
  rename("veh_id" = id) %>%
  unnest_wider(position) 
  # Add file name without extension







list(resp
     ,resp_qa) %>% 
  map(~{
    # browser()
    temp_resp = .x
    
    temp_raw = temp_resp %>% 
      content("raw")
    
    temp_gtfs_feed = P("transit_realtime.FeedMessage")$read(temp_raw)
    
    temp_gtfs_feed_json = temp_gtfs_feed$toJSON() %>% 
      jsonlite::fromJSON()
    
    temp_gtfs_feed_json %>% 
      .[['entity']] %>% 
      rename("id_id" = id) %>%  
      head()
    
  })
  

key = "61762e745cf44d5aa4fff468abe82758"
feed_url = stringr::str_glue("https://api.wmata.com/gtfs/bus-gtfsrt-tripupdates.pb?api_key={key}")
base_url = "https://api.wmata.com/gtfs/bus-gtfsrt-tripupdates.pb"


response <- GET(
  url = base_url,
  query = list(api_key = key)
)


function(base_url)
key = "61762e745cf44d5aa4fff468abe82758"
base_url = "http://s3.amazonaws.com/commtrans-realtime-prod/tripupdates.pb"
query_params = NULL
base_url = "https://api.wmata.com/gtfs/bus-gtfsrt-vehiclepositions.pb"
query_params = list(api_key = key)
base_url = "https://api.pugetsound.onebusaway.org/api/gtfs_realtime/vehicle-positions-for-agency/1.pbtext"
query_params = list(key = key)

query_params = list(api_key = key)
query_params = list(key = "your_api_key_here")
query_params = NULL
is.null(query_params)

# Build the full URL
temp_mod_url = modify_url(base_url, query = query_params) 


c(base_url, base_url) %>% map_chr(~modify_url(.x, query = query_params))






index = output_write_location %>% 
  here::here("agency_wamata") %>% 
  list.files(recursive = T, pattern = "jsonResp_updates", full.names = T)




temp_resp = qs::qread(index[1]) 
  


