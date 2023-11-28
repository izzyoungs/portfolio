library(tidyverse)
library(sf)
library(glue)
library(bigrquery)
library(reticulate)
library(RPyGeo)
library(lubridate)
library(data.table)
library(zoo)

# run sql query
for_bq <- glue_sql("WITH network_links  AS (
SELECT stableEdgeId, geometry
FROM `replica-customer.mid_atlantic.mid_atlantic_2023_Q2_network_segments`
),

trips AS (SELECT start_time, end_time, activity_id, network_link_ids as stableEdgeId
FROM `replica-customer.mid_atlantic.mid_atlantic_2023_Q2_thursday_trip`,
UNNEST (network_link_ids) network_link_ids
WHERE destination_bgrp = '110010101002' AND mode = 'WALKING'
)

select t.start_time, t.end_time, t.activity_id, t.stableEdgeId, n.geometry
FROM trips as t
LEFT JOIN network_links as n
  ON t.stableEdgeId = n.stableEdgeId")

# read in table
tb <- bq_project_query("replica-customer", for_bq)

# load table to data frame
df_network <- bq_table_download(tb)

# reduce the geometry for each activity to a single linestring
geometry <- df_network %>%
  filter(!is.na(geometry)) %>%
  st_as_sf(wkt = "geometry") %>%
  st_set_crs(4326) %>%
  group_by(activity_id) %>%
  summarize(geometry = st_union(geometry))

# final dataframe with all relevant columns
df <- df_network %>%
  group_by(activity_id) %>%
  slice(1L) %>%
  select(-stableEdgeId, -geometry) %>%
  left_join(geometry) %>%
  st_as_sf()

# write to a shapefile
st_write(df, "trip_data_spatial/trip_data_2023_q2_thursday_spatial.shp",
         delete_layer = TRUE)

working_dir <- getwd()

# This will throw a warning but you need to connect to the arcgis installation of python
use_python("C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3")

arcpy <- import("arcpy")
  
# generate points along lines every 100 feet for best visualization
arcpy$GeneratePointsAlongLines_management(Input_Features = paste0(working_dir, "/trip_data_spatial/trip_data_2023_q2_thursday_spatial.shp"), 
                                          Output_Feature_Class = paste0(working_dir, "/points_along_lines/trip_data_2023_q2_thursday_points.shp"),
                                          Point_Placement = "DISTANCE",
                                          Distance = '100 feet',
                                          Include_End_Points = "END_POINTS",
                                          Add_Chainage_Fields = "ADD_CHAINAGE")

# Create a function that converts spatial geometry to x and y fields
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# read the new data frame back in
fc <- st_read("points_along_lines/trip_data_2023_q2_thursday_points.shp") %>%
  sfc_as_cols

# get the times
time <- df %>%
  st_drop_geometry() %>%
  select(activity_id, start_time, end_time)

# interpolate time between points of the same activity_id
fc_final <- fc %>%
  st_drop_geometry() %>%
  select(activity_id = actvty_, x, y) %>%
  left_join(time) %>%
  group_by(activity_id) %>%
  mutate(index = row_number(),
         activity_id = paste0("S", activity_id), # Helps when importing to foursquare to not read the col as a numeric
         time = case_when(index == 1 ~ start_time,
                          index == last(index) ~ end_time),
         time = as.ITime(na.approx(time)),
         time = strftime(time, format = "%H:%M")) %>%
  select(-start_time, -end_time, -index)

fc_final %>%
  write_csv("points_for_animated_trips.csv")
