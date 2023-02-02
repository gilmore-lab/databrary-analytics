library(tidyverse)

unique.downloads <- read.csv("csv/uniqueUserCountOfAssetDownloadsByVolume.csv")
# 
users.sessions.downloads <- read.csv("csv/distinctUsersPerVolume.csv")

unique.downloads %>%
  filter(volume == 239) %>%
  select(usrcnt) %>%
  as.numeric()