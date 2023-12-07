library(magrittr)
library(dplyr)
library(glue)
library(bigrquery)

for_bq <- glue_sql("WITH network_links AS (
SELECT stableEdgeId, geometry
FROM `replica-customer.mid_atlantic.mid_atlantic_2023_Q2_network_segments`
),

trips AS (SELECT DISTINCT network_link_ids as stableEdgeId
FROM `replica-customer.mid_atlantic.mid_atlantic_2023_Q2_thursday_trip`,
UNNEST (network_link_ids) network_link_ids
WHERE destination_bgrp = '110010101002'
)

select t.stableEdgeId, n.geometry
FROM trips as t
LEFT JOIN network_links as n
  ON t.stableEdgeId = n.stableEdgeId")

# read in table
tb <- bq_project_query("replica-customer", for_bq)

# load table to data frame
df_network <- bq_table_download(tb)

df_network %>%
  count(is.na(geometry))
