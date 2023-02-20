# R/volume_asset_functions.R

#-------------------------------------------------------------------------------
get_assets_in_vol <- function(vol_id, vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.logical(vb))
  
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

#-------------------------------------------------------------------------------
calculate_vol_asset_stats <- function(vol_id,
                                      save_file = FALSE,
                                      save_path = 'src/csv',
                                      vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.logical(save_file))
  stopifnot(is.character(save_path))
  stopifnot(dir.exists(save_path))
  stopifnot(is.logical(vb))
  
  if (vb)
    message(paste0('Retrieving asset data for volume ', vol_id))
  vol_assets <- get_assets_in_vol(vol_id, vb)
  if (is.null(vol_assets)) {
    if (vb)
      message(paste0(" No shared data in volume ", vol_id))
    NULL
  } else {
    vol_summary <- vol_assets %>%
      dplyr::group_by(., vol_id, mimetype, extension, asset_type) %>%
      dplyr::summarise(
        .,
        n_files = n(),
        tot_size_gb = bytes_to_gb(sum(size, na.rm = TRUE)),
        tot_dur_hrs = ms_to_hrs(sum(duration, na.rm = TRUE))
      )
    
    if (save_file) {
      out_fn <-
        file.path(save_path, paste0(stringr::str_pad(vol_id, 5, pad = "0"), '-assets.csv'))
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

#-------------------------------------------------------------------------------
update_vol_asset_stats <-
  function(start_vol_id,
           end_vol_id,
           save_file = TRUE,
           save_path = 'src/csv',
           vb = FALSE) {
    stopifnot(is.numeric(start_vol_id))
    stopifnot(start_vol_id > 0)
    stopifnot(is.numeric(end_vol_id))
    stopifnot(end_vol_id > 0)
    stopifnot(end_vol_id > start_vol_id)
    stopifnot(is.logical(save_file))
    stopifnot(is.character(save_path))
    stopifnot(dir.exists(save_path))
    stopifnot(is.logical(vb))
    
    message(
      paste0(
        "Updating volume asset statistics for volumes ",
        start_vol_id,
        ":",
        end_vol_id,
        "."
      )
    )
    message("Please be patient.")
    purrr::map(
      c(start_vol_id:end_vol_id),
      calculate_vol_asset_stats,
      save_file,
      save_path,
      vb,
      .progress = "Vol assets:"
    )
  }

#-------------------------------------------------------------------------------
#
update_all_vol_stats <- function(max_volume_id,
                                 vols_per_pass = 50,
                                 save_file = TRUE,
                                 save_path = 'src/csv',
                                 vb = FALSE,
                                 db_login = Sys.getenv("DATABRARY_LOGIN")) {
  stopifnot(is.numeric(max_volume_id))
  stopifnot(max_volume_id > 0)
  stopifnot(is.numeric(vols_per_pass))
  stopifnot(vols_per_pass > 0)
  stopifnot(is.logical(save_file))
  stopifnot(is.character(save_path))
  stopifnot(dir.exists(save_path))
  stopifnot(is.logical(vb))
  stopifnot(is.character(db_login))
  
  options(dplyr.summarise.inform = FALSE)
  
  databraryapi::login_db(db_login)
  
  # It may be unnecessary, but I do this in separate chunks
  # Some of the larger volumes have a lot of assets, and this
  # chunking gives the analyst some feedback about what's happening.
  # Also, if a problem arises, it's easier to debug.
  range_start_id <-
    seq(from = 1, to = max_volume_id, by = vols_per_pass)
  range_end_id <- range_start_id + vols_per_pass
  
  purrr::walk(
    range_start_id,
    range_end_id,
    update_vol_asset_stats,
    save_file = save_file,
    save_path = save_path,
    vb = vb
  )
  
  databraryapi::logout_db()
}

#-------------------------------------------------------------------------------
generate_volume_asset_csv_list <- function(csv_dir = "src/csv") {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  list.files(csv_dir, '-assets\\.csv', full.names = TRUE)
}

#-------------------------------------------------------------------------------
make_volume_assets_stats_df <-
  function(csv_fns = "src/csv", vb = FALSE) {
    stopifnot(is.character(csv_fns))
    if (vb)
      message("Making df from CSVs.")
    df <-
      purrr::map_df(csv_fns,
                    readr::read_csv,
                    show_col_types = FALSE,
                    .progress = "Asset CSVs:")
    dplyr::arrange(df, vol_id)
  }

#-------------------------------------------------------------------------------
bytes_to_gb <- function(b) {
  b / (1.024e9)
}

#-------------------------------------------------------------------------------
ms_to_secs <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms / 1000
}

#-------------------------------------------------------------------------------
secs_to_mins <- function(s) {
  if (!is.numeric(s)) {
    stop('`s` must be a number.')
  }
  s / 60
}

#-------------------------------------------------------------------------------
mins_to_hrs <- function(m) {
  if (!is.numeric(m)) {
    stop('`m` must be a number.')
  }
  m / 60
}

#-------------------------------------------------------------------------------
ms_to_mins <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms_to_secs(ms) %>% secs_to_mins(.)
}

#-------------------------------------------------------------------------------
ms_to_hrs <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms_to_secs(ms) %>% secs_to_mins(.) %>% mins_to_hrs(.)
}

storage_gb_by_vol <-
  function(df,
           deposit_fee_figshare_gb = 2.34,
           deposit_fee_dryad_gb = 5) {
    df_new <- df |>
      dplyr::group_by(vol_id) |>
      dplyr::summarise(n_tot_files = sum(n_files),
                       tot_gb = sum(tot_size_gb)) |>
      dplyr::mutate(
        figshare_fee = tot_gb * deposit_fee_figshare_gb,
        dryad_fee = tot_gb * deposit_fee_dryad_gb
      )
    df_new
  }
