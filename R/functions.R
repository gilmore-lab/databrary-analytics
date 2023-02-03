# functions.R

select_tags <-
  c(
    'numerical cognition',
    'mathematical ability',
    'number',
    'teaching',
    'teaching clips',
    'abacus',
    'classroom',
    'mathematical equivalence',
    'mental arithmetic',
    'number comprehension',
    'number discrimination',
    'science',
    'stem',
    'statistical learning',
    'school readiness'
  )

auth_to_google <- function(gacct = "rick.o.gilmore") {
  googledrive::drive_auth(email = gacct)
}

load_old_inst_invest_data <-
  function(csv_dir = "csv", csv_fn = "institutions-investigators.csv") {
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    old_stats <-
      readr::read_csv(file.path(csv_dir, csv_fn), show_col_types = FALSE)
    dplyr::mutate(old_stats, date = lubridate::as_datetime(date))
  }

get_inst_invest_datab <- function() {
  suppressPackageStartupMessages(require(tidyverse))
  
  new_stats <- databraryapi::get_db_stats()
  new_stats$date <- lubridate::as_datetime(new_stats$date)
  
  new_stats <- new_stats %>%
    dplyr::select(date, institutions, investigators, affiliates) %>%
    dplyr::mutate(date = lubridate::as_datetime(date))
  
  if (purrr::is_empty(new_stats)) {
    warning("Unable to retrieve new statistics from Databrary.")
    NULL
  } else {
    new_stats
  }
}

update_inst_invest_df <-
  function(csv_dir = "csv", csv_fn = "institutions-investigators.csv") {
    old_df <- load_old_inst_invest_data(csv_dir, csv_fn)
    new_item <- get_inst_invest_datab()
    
    new_df <- old_df
    next_entry <- dim(old_df)[1] + 1
    new_df[next_entry, ] = NA
    new_df[next_entry, ] <- new_item
    new_df
  }

update_inst_invest_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "institutions-investigators.csv") {
    stopifnot(!purrr::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    readr::write_csv(df, file.path(csv_dir, csv_fn))
  }

###############################################################################

load_old_tags_data <-
  function(csv_dir = "csv", csv_fn = "databrary-tags.csv") {
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    old_tags <-
      readr::read_csv(file.path(csv_dir, csv_fn), show_col_types = FALSE)
    old_tags
  }

refresh_volume_tags_df <- function(vol_ids = 1:100) {
  message(
    "Refreshing tags & keywords data for volumes ",
    min(vol_ids),
    ":",
    max(vol_ids),
    ". Please be patient."
  )
  purrr::map_df(.x = vol_ids,
                .f = make_volume_tags_df,
                .progress = TRUE)
}

update_vol_tags_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "databrary-tags.csv") {
    stopifnot(!purrr::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    readr::write_csv(df, file.path(csv_dir, csv_fn))
  }

make_volume_tags_df <- function(vol_id, vb = FALSE) {
  if (vb)
    message(paste0("Gathering tags from volume ", vol_id))
  these_tags <- databraryapi::list_volume_tags(vol_id)
  if (is_empty(these_tags)) {
    df <- data.frame(
      vol_id = vol_id,
      url = paste0("https://nyu.databrary.org/volume/",
                   vol_id),
      tags = NA,
      weight = NA
    )
  } else {
    these_tags <- these_tags %>%
      dplyr::select(., id, weight) %>%
      dplyr::rename(., tags = id)
    df <- these_tags
    df$vol_id = vol_id
    df$url <- paste0("https://nyu.databrary.org/volume/", vol_id)
  }
  dplyr::select(df, vol_id, url, tags, weight)
}

make_stem_tags_df <- function(tags_df,
                              vb = TRUE,
                              save_csv = TRUE) {
  stem_tags <- dplyr::filter(tags_df, tags %in% select_tags)
  stem_tags <- dplyr::arrange(stem_tags, vol_id, tags)
  
  # Unique vol ids to get volume metadata, esp title
  stem_vol_ids <- unique(stem_tags$vol_id)
  
  # Pull titles
  if (vb)
    message("Gathering STEM-related tags from n=",
            length(stem_vol_ids),
            " volumes.")
  stem_vols_df <- purrr::map_df(
    .x = stem_vol_ids,
    .f = databraryapi::list_volume_metadata,
    .progress = TRUE
  )
  
  stem_vols_df <-
    dplyr::left_join(stem_vols_df, stem_tags, multiple = "all")
  
  stem_vols_df <- stem_vols_df %>%
    dplyr::filter(., vol_id != 109) %>% # Empty volume
    dplyr::select(.,-owners,-permission,-doi)
  
  stem_vols_df
}

###############################################################################

get_volume_funding <- function(vol_id, vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  
  if (vb)
    message('Getting funders for volume ', vol_id)
  databraryapi::list_volume_funding(vol_id)
}

refresh_volume_funders_df <- function(vol_ids = 1:1520) {
  message(
    "Refreshing funders data for volumes ",
    min(vol_ids),
    ":",
    max(vol_ids),
    ". Please be patient."
  )
  purrr::map_df(.x = vol_ids,
                .f = get_volume_funding,
                .progress = TRUE)
}

update_volume_funders_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "funders.csv") {
    stopifnot(!purrr::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    if (!file.exists(file.path(csv_dir, csv_fn))) {
      warning("File does not exist: ", file.path(csv_dir, csv_fn))
      warning("Creating new file: ", file.path(csv_dir, csv_fn))
    }
    readr::write_csv(df, file.path(csv_dir, csv_fn))
  }

###############################################################################

get_assets_in_vol <- function(vol_id, vb = FALSE) {
  if ((as.numeric(vol_id) <= 0)) {
    stop('`vol_id` must be > 0.')
  }
  if (!is.logical(vb)) {
    stop('`vb` must be a logical value.')
  }
  
  suppressPackageStartupMessages(require(tidyverse))
  suppressPackageStartupMessages(require(databraryapi))
  
  if (vb)
    message(paste0(" Extracting assets from volume ", vol_id))
  vol_data <- databraryapi::list_assets_in_volume(vol_id)
  
  if (is.null(vol_data)) {
    if (vb)
      message(" No available assets.")
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

calculate_vol_asset_stats <- function(vol_id,
                                      save_file = FALSE,
                                      save_path = 'src/csv',
                                      vb = FALSE) {
  if ((as.numeric(vol_id) <= 0)) {
    stop('`vol_id` must be > 0.')
  }
  if (!is.logical(save_file)) {
    stop('`save_file` must be a logical value.')
  }
  if (!is.character(save_path)) {
    stop('`save_path` must be a character string.')
  }
  if (!dir.exists(save_path)) {
    warning('Directory not found: `', save_path, '`.')
  }
  if (!is.logical(vb)) {
    stop('`vb` must be a logical value.')
  }
  
  suppressPackageStartupMessages(require(tidyverse))
  
  options(dplyr.summarise.inform = FALSE)
  
  if (vb)
    message(paste0('Retrieving asset data for volume ', vol_id))
  vol_assets <- get_assets_in_vol(vol_id, vb)
  if (is.null(vol_assets)) {
    if (vb)
      message(paste0(" No shared data in volume ", vol_id))
    NULL
  } else {
    vol_summary <- vol_assets %>%
      # dplyr::mutate(., vol_id = vol_id, mimetype = mimetype, extension = extension) %>%
      dplyr::group_by(., vol_id, mimetype, extension, asset_type) %>%
      dplyr::summarise(
        .,
        n_files = n(),
        tot_size_gb = bytes_to_gb(sum(size, na.rm = TRUE)),
        tot_dur_hrs = ms_to_hrs(sum(duration, na.rm = TRUE))
      )
    
    if (save_file) {
      out_fn <- paste0(save_path, '/vol_', vol_id, '_assets.csv')
      if (file.exists(out_fn)) {
        if (vb)
          message("File exists: ", out_fn, ". Overwritten.")
      }
      if (vb)
        message(paste0(" Saving data to ", out_fn))
      readr::write_csv(vol_summary, file = out_fn)
    }
    vol_summary
  }
}

update_vol_asset_stats <-
  function(start_vol_id,
           end_vol_id,
           save_file = TRUE,
           save_path = 'src/csv',
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
    
    suppressPackageStartupMessages(require(purrr))
    
    message(
        paste0(
          "Updating volume asset statistics for volumes ",
          start_vol_id,
          " : ",
          end_vol_id, ". Please be patient."
        )
      )
    purrr::map(
      c(start_vol_id:end_vol_id),
      calculate_vol_asset_stats,
      save_file,
      save_path,
      vb,
      .progress = TRUE
    )
  }

update_all_vol_stats <- function(max_volume_id,
                                 vols_per_pass = 50,
                                 save_file = TRUE,
                                 save_path = 'csv',
                                 vb = TRUE) {
  if (!is.numeric(max_volume_id)) {
    stop('`max_volume_id` must be a number.')
  }
  if (max_volume_id <= 0) {
    stop('`max_volume_id` must be > 0')
  }
  if (!is.numeric(vols_per_pass)) {
    stop('`vols_per_pass` must be a number.')
  }
  if (vols_per_pass <= 0) {
    stop('`vols_per_pass` must be > 0')
  }
  if (!is.logical(save_file)) {
    stop('`save_file` must be a logical value.')
  }
  if (!is.character(save_path)) {
    stop('`save_path` must be a character string.')
  }
  if (!dir.exists(save_path)) {
    stop(paste0('Directory not found: `', save_path, '`.'))
  }
  if (!is.logical(vb)) {
    stop('`vb` must be a logical value.')
  }
  
  suppressPackageStartupMessages(require(purrr))
  
  # It may be unnecessary, but I do this in separate chunks
  # Some of the larger volumes have a lot of assets, and this
  # chunking gives the analyst some feedback about what's happening.
  # Also, if a problem arises, it's easier to debug.
  range_start_id <-
    seq(from = 1, to = max_volume_id, by = vols_per_pass)
  range_end_id <- range_start_id + vols_per_pass
  
  purrr::map2(
    range_start_id,
    range_end_id,
    update_vol_asset_stats,
    save_file = save_file,
    save_path = save_path,
    vb = vb
  )
}

make_volume_assets_stats_df <- function(csv_dir = "src/csv") {
  stopifnot(dir.exists(csv_dir))
  csv_fns <- list.files(csv_dir, '_assets\\.csv', full.names = TRUE)
  df <- purrr::map_df(csv_fns, readr::read_csv, show_col_types = FALSE)
  dplyr::arrange(df, vol_id)
}

bytes_to_gb <- function(b) {
  b / (1.024e9)
}

ms_to_secs <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms / 1000
}

secs_to_mins <- function(s) {
  if (!is.numeric(s)) {
    stop('`s` must be a number.')
  }
  s / 60
}

mins_to_hrs <- function(m) {
  if (!is.numeric(m)) {
    stop('`m` must be a number.')
  }
  m / 60
}

ms_to_mins <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms_to_secs(ms) %>% secs_to_mins(.)
}

ms_to_hrs <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms_to_secs(ms) %>% secs_to_mins(.) %>% mins_to_hrs(.)
}
