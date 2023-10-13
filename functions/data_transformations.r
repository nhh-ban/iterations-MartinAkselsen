# Function that transforms stations_metadata into a better format

transform_metadata_to_df <- function(stations_metadata) {
  df <- stations_metadata[[1]] %>%
    map(as_tibble) %>%
    list_rbind() %>%
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>%
    mutate(location = map(location, unlist)) %>%
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>%
    select(-location)
  
  return(df)
}

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)

stations_metadata_df