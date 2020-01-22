# Generate report on recently created and shared volumes

# Load packages
library(tidyverse)

# Grab activity list from Databrary API

r <- httr::POST("https://nyu.databrary.org/api/activity")
if (httr::status_code(r) == 200) {
  activity_list <- jsonlite::fromJSON(httr::content(r, 'text', encoding = 'UTF-8'))
} else {
  message(paste0( 'Download Failed, HTTP status ', httr::status_code(r)))
}

act_stats <- activity_list$stats
act_vol_info <- activity_list$activity

act_stats_df <- data.frame(date=Sys.time(),
                           institutions=act_stats$authorized[6],
                           investigators=act_stats$authorized[5],
                           affiliates=act_stats$authorized[4],
                           n_volumes=act_stats$volumes,
                           n_shared=act_stats$shared,
                           n_files=act_stats$assets,
                           n_hrs=act_stats$duration/(60*60*1000)) # ms->hrs

shared_vols_df <- act_vol_info$volume %>%
  dplyr::filter(!is.na(id))

# Download more volumes

vs <- httr::GET("https://nyu.databrary.org/api/volume")
if (httr::status_code(vs) == 200) {
  vs.content <- jsonlite::fromJSON(httr::content(vs, 'text', encoding ='UTF-8'))
} else {
  message(paste0( 'Download Failed, HTTP status ', httr::status_code(vs)))
}

# Volumes metadata
vs.content[,c("id", "doi", "creation", "permission")]

# Volume owners
vs.content[,"owners"]
