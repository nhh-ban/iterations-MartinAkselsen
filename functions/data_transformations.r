#
# Functions
#



# Function that transforms stations_metadata into a better format

transform_metadata_to_df <- function(stations_metadata) {
  df <- stations_metadata[[1]] %>%
    map(as_tibble) %>%
    list_rbind() %>%
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>%
    mutate(latestData = with_tz(latestData, "UTC")) %>% 
    mutate(location = map(location, unlist)) %>%
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>%
    select(-location)
  
  return(df)
}


# Time-function

to_iso8601 <- 
function(datetime_var, offset_in_days){
 adjusted_datetime <- datetime_var + days(offset_in_days)
 iso8601_string <- format(with_tz(adjusted_datetime, "UTC"), 
                          "%Y-%m-%dT%H:%M:%SZ")
 return(iso8601_string)
}

# Transform volumes

transform_volumes <- function(data) {
  volumes <- data$trafficData$volume$byHour$edges
  df <- purrr::map_dfr(volumes, ~{
    tibble::tibble(
      from = .x$node$from,
      to = .x$node$to,
      volume = .x$node$total$volumeNumbers$volume
    )
  })
  return(df)
}

