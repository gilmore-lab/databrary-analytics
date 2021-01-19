# volumes-with-videos-annotations helpers.R

get_assets_in_vol <- function(vol_id, vb = FALSE) {
  require(tidyverse)
  
  if (vb) message(paste0(" Extracting assets from volume ", vol_id))
  vol_data <- databraryapi::list_assets_in_volume(vol_id)
  
  if (is.null(vol_data)) {
    if (vb) paste0(" No available assets.")
    NULL
  } else {
    # some volumes have no assets with duration or size attribute
    if (!('duration' %in% names(vol_data))) {
      vol_data <- dplyr::mutate(vol_data, duration = NA)
    }
    if (!('size' %in% names(vol_data))) {
      vol_data <- dplyr::mutate(vol_data, size = NA)
    }
    vol_data <- vol_data %>%
      dplyr::mutate(vol_id = vol_id) %>%
      dplyr::select(vol_id, size, duration, mimetype, extension, asset_type)
    vol_data    
  }
}

calculate_vol_asset_stats <- function(vol_id, save_file = FALSE,
                                      save_path = 'volumes-with-videos-annotations/csv',
                                      vb = FALSE) {
  require(tidyverse)
  options(dplyr.summarise.inform = FALSE)
  
  if (vb) message(paste0('Retrieving asset data for volume ', vol_id))
  vol_assets <- get_assets_in_vol(vol_id, vb)
  if (is.null(vol_assets)) {
    if (vb) message(paste0(" No data in volume ", vol_id))
    NULL
  } else {
    
    vol_summary <- vol_assets %>%
      # dplyr::mutate(., vol_id = vol_id, mimetype = mimetype, extension = extension) %>%
      dplyr::group_by(., vol_id, mimetype, extension, asset_type) %>%
      dplyr::summarise(., n_files = n(),
                       tot_size = sum(size, na.rm = TRUE),
                       tot_dur = sum(duration, na.rm = TRUE))
    
    if(!('duration' %in% names(vol_summary))) {
      vol_summary <- dplyr::mutate(vol_summary, duration = NA)
    }


    # vol_summary <- vol_summary %>%
    #   dplyr::count(asset_type) %>%
    #   dplyr::mutate(vol_id = vol_id) # %>%
    #   #dplyr::select(vol_id, asset_type, n, tot_size, tot_dur)  
    
    if (save_file) {
      if (vb) message(paste0(" Saving data from volume ", vol_id))
      write_csv(vol_summary, path = paste0(save_path, '/vol_', vol_id, '.csv'))
    }
    vol_summary
  }
}

update_vol_asset_stats <- function(start_vol_id, end_vol_id, save_file = TRUE,
                                   save_path = 'volumes-with-videos-annotations/csv',
                                   vb = FALSE) {
  if (!is.numeric(start_vol_id)) {
    stop('`start_vol_id` must be a number.')
  }
  if (!is.numeric(end_vol_id)) {
    stop('`end_vol_id` must be a number.')
  }
  if (!(start_vol_id <= end_vol_id)) {
    stop('`start_vol_id` must be <= than `end_vol_id`')
  }
  
  if (vb) message(paste0("Updating volume asset statistics for volumes ", start_vol_id, " : ",  end_vol_id))
  purrr::map(c(start_vol_id:end_vol_id), calculate_vol_asset_stats, save_file, save_path, vb)
}

update_all_vol_stats <- function(max_vol_id, 
                                 vols_per_pass = 50,
                                 save_file = TRUE,
                                 save_path = 'volumes-with-videos-annotations/csv',
                                 vb = FALSE) {
  
  # It may be unnecessary, but I do this in separate chunks
  # Some of the larger volumes have a lot of assets, and this 
  # chunking gives the analyst some feedback about what's happening.
  # Also, if a problem arises, it's easier to debug.
  range_start_id <- seq(from = 1, to = max_vol_id, by = vols_per_pass)
  range_end_id <- range_start_id + vols_per_pass
  
  purrr::map2(range_start_id, range_end_id, update_vol_asset_stats, 
              save_file = save_file, save_path = save_path, vb = vb)
}

ms_to_secs <- function(ms) {
  ms/1000
}

secs_to_mins <- function(s) {
  s/60
}

mins_to_hrs <- function(m) {
  m/60
}

ms_to_mins <- function(ms) {
  ms_to_secs(ms) %>% secs_to_mins(.)
}

ms_to_hrs <- function(ms) {
  ms_to_secs(ms) %>% secs_to_mins(.) %>% mins_to_hrs(.)
}
