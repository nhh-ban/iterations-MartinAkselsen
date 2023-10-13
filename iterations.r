library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
  ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  theme_classic()


### 6: Changing the plot
stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  # Randomly samples one station from the filtered data and stores it in `sampled_station`
  sample_n(1) -> sampled_station
# Extracts the name of the sampled station and stores it in `station_name`
station_name <- sampled_station$name

sampled_station %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  # Converts the `from` column to POSIXct format for proper date-time plotting
  mutate(from = as.POSIXct(from, format="%Y-%m-%dT%H:%M:%S")) %>%
  ggplot(aes(x=from, y=volume, group = 1)) + 
  geom_line(aes(color = station_name)) + 
  theme_classic() +
  # Labels the color legend as 'Traffic Station'
  labs(color = 'Traffic Station') + 
  # Formats the x-axis labels as date-time
  scale_x_datetime(labels = scales::date_format("%Y-%m-%d %H:%M")) 





