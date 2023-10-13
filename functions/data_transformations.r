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


# GQL-function

vol_qry <- function(id, from, to) {
  query <- sprintf(
    '{
      trafficRegistrationPoint(id: "%s") {
        id
        name
        latestData {
          volumeByHour(from: "%s", to: "%s") {
            edges {
              node {
                from
                to
                volume {
                  car
                  heavyVehicle
                  total
                }
              }
            }
          }
        }
        location {
          coordinates {
            latLon {
              lat
              lon
            }
          }
        }
      }
    }', id, from, to)
  
  return(query)
}

GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

