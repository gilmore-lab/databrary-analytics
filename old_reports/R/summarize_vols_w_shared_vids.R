# 2018-09-11

library(tidyverse)

shared_vols <- read.csv("csv/shared_vols_metadata.csv")
vols_w_vids <- read.csv("csv/shared_videos_2018-09-10.csv")

vols_w_vids_summary <- vols_w_vids %>%
  group_by(., vol.id, session.id) %>%
  summarise(., vid_hrs = sum(duration)/(1000*60*60), n_assets = n())

vid_hrs_in_vol <- vols_w_vids %>%
  group_by(., vol.id) %>%
  summarise(., vid_hrs = sum(as.numeric(duration)/(1000*60*60)))

sess_in_vol <- vols_w_vids %>%
  select(., vol.id, session.id) %>%
  group_by(., vol.id) %>%
  summarize(., n_sessions = n())

assets_in_vol <- vols_w_vids %>%
  group_by(., vol.id) %>%
  summarise(., n_assets = n())

shared_vols_summ <- left_join(shared_vols, sess_in_vol, by = c("id" = "vol.id")) %>%
  left_join(., vid_hrs_in_vol, by = c("id" = "vol.id")) %>%
  left_join(., assets_in_vol, by = c("id" = "vol.id")) %>%
  arrange(., desc(vid_hrs))

write_csv(shared_vols_summ, "csv/shared_vols_w_video_rpt.csv")

