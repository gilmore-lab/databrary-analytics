extract_vol_owners_name <- function(i, new_vols) {
  new_vols[i,'owners'][[1]][[1]]$name
}

concatenate_owners_names <- function(name_strings) {
  name_str <- paste0(name_strings[1])
  if (length(name_strings) > 1) {
    for (i in 2:length(name_strings)) {
      name_str <- paste0(name_str, '; ', name_strings[i])
    }
  }
  name_str
}

new_volumes %>%
  mutate(name_str = owners[[1]]) %>%
  select(id, name_str)

unnested_vols <- unnest(new_volumes) %>%
  rename(owner_name = name1, owner_id = id1) %>%
  mutate