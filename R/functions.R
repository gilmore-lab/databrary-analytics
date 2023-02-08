# functions.R

# select_tags <-
#   c(
#     'numerical cognition',
#     'mathematical ability',
#     'number',
#     'teaching',
#     'teaching clips',
#     'abacus',
#     'classroom',
#     'mathematical equivalence',
#     'mental arithmetic',
#     'number comprehension',
#     'number discrimination',
#     'science',
#     'stem',
#     'statistical learning',
#     'school readiness'
#   )

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
  
  if (rlang::is_empty(new_stats)) {
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
    stopifnot(!rlang::is_empty(df))
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
                .progress = "Volume tags:")
}

update_vol_tags_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "databrary-tags.csv") {
    stopifnot(!rlang::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    readr::write_csv(df, file.path(csv_dir, csv_fn))
  }

make_volume_tags_df <- function(vol_id, vb = FALSE) {
  if (vb)
    message(paste0("Gathering tags from volume ", vol_id))
  these_tags <- databraryapi::list_volume_tags(vol_id)
  if (rlang::is_empty(these_tags)) {
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
                              vb = FALSE,
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
    .progress = "STEM tags:"
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
                .progress = "Volume funders:")
}

update_volume_funders_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "funders.csv") {
    stopifnot(!rlang::is_empty(df))
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
        end_vol_id,
        ". Please be patient."
      )
    )
    purrr::map(
      c(start_vol_id:end_vol_id),
      calculate_vol_asset_stats,
      save_file,
      save_path,
      vb,
      .progress = "Volume assets:"
    )
  }

update_all_vol_stats <- function(max_volume_id,
                                 vols_per_pass = 50,
                                 save_file = TRUE,
                                 save_path = 'csv',
                                 vb = FALSE) {
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
  df <-
    purrr::map_df(csv_fns, readr::read_csv, show_col_types = FALSE)
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

################################################################################

make_all_session_df <- function(csv_dir = "src/csv") {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  fl <- list.files(csv_dir, "[0-9]{4}\\-sess\\-materials\\.csv", full.names = TRUE)
  purrr::map_df(fl, make_cleaned_session_df, csv_dir, .progress = "Import sessions:")
}


################################################################################
vol_csv_avail <- function(vol_id) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  r <- httr::GET(this_url,
                 httr::authenticate(Sys.getenv("DATABRARY_LOGIN"), 
                                    keyring::key_get(service = "databrary", 
                                                     username = Sys.getenv("DATABRARY_LOGIN"))))
  
  if (httr::status_code(r) == 404) {
    FALSE
  } else {
    TRUE
  }
}

################################################################################
get_volume_demog <- function(vol_id = 4, vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  if (vb)
    message(paste0("Gathering demog data from ", this_url))
  
  r <- try(httr::GET(this_url,
                 httr::authenticate(Sys.getenv("DATABRARY_LOGIN"), 
                                    keyring::key_get(service = "databrary", 
                                                     username = Sys.getenv("DATABRARY_LOGIN")))), silent = TRUE)
  
  if (inherits(r, 'try-error')) {
    if (vb) message(" Error accessing volume ", vol_id)
    return(NULL)
  }
  
  if (httr::status_code(r) == 404) {
    if (vb) message(" 404 error from ", this_url)
    return(NULL)
  }

  if (is.null(r)) {
      if (vb) message(" NULL response from ", this_url)
      return(NULL)
  }
    
  c <- httr::content(r, show_col_types = FALSE, type = "text/csv", 
                     encoding = "utf-8")
  
  if (is.null(c)) {
    if (vb) message("  No CSV data returned from ", this_url)
    return(NULL)
  }
  
  if (vb) message("Converting response to data frame.")
  if (is.data.frame(c)) {
    if (vb) 
      message(paste0("Imported data frame. Cleaning up."))
    r_df <- dplyr::mutate(c, vol_id = vol_id)
    names(r_df) <- stringr::str_replace_all(names(r_df), 
                                        pattern = "[\\-|\\.| ]", replacement = "_")
    r_df <-
      dplyr::filter(r_df, !(stringr::str_detect(session_date, 'materials')))
    r_df <- dplyr::mutate(
      r_df,
      session_id = as.character(session_id))
    # TODO: Devise a more elegant way to clean-up/normalize the spreadsheet data
    if ("participant_ID" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, participant_ID = as.character(participant_ID))
    if ("participant1_ID" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, participant1_ID = as.character(participant1_ID))
    if ("participant2_ID" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, participant2_ID = as.character(participant2_ID))
    if ("participant3_ID" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, participant3_ID = as.character(participant3_ID))
    if ("participant4_ID" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, participant4_ID = as.character(participant4_ID))
    if ("participant4_ID" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, participant5_ID = as.character(participant5_ID))
    if ("session_date" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, session_date = as.character(session_date))
    if ("participant_birthdate" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, participant_birthdate = as.character(participant_birthdate))
    if ("session_release" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, session_release = as.character(session_release))
    if ("session_name" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, session_name = as.character(session_name))
    if ("group_name" %in% names(r_df))
      r_df <- dplyr::mutate(r_df, group_name = as.character(group_name))
    return(r_df)
  }
  else {
    if (vb) 
      message("Can't coerce to data frame. Skipping.\n")
    return(NULL)
  }
  r_df
}

################################################################################
get_volume_ss_save_csv <- function(vol_id = 4, dir = "src/csv", vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  if (vb)
    message(paste0("Gathering demog data from ", this_url))
  
  r <- try(httr::GET(this_url,
                     httr::authenticate(Sys.getenv("DATABRARY_LOGIN"), 
                                        keyring::key_get(service = "databrary", 
                                                         username = Sys.getenv("DATABRARY_LOGIN")))), silent = TRUE)
  
  if (inherits(r, 'try-error')) {
    if (vb) message(" Error accessing volume ", vol_id)
    return(NULL)
  }
  
  if (httr::status_code(r) == 404) {
    if (vb) message(" 404 error from ", this_url)
    return(NULL)
  }
  
  if (is.null(r)) {
    if (vb) message(" NULL response from ", this_url)
    return(NULL)
  }
  
  c <- httr::content(r, as = 'text')
  
  if (is.null(c)) {
    if (vb) message(" No text data returned from ", this_url)
    return(NULL)
  }
  
  df <- readr::read_csv(c, col_types = "cccccccccccccccccccccccccccccc")
  if (is.data.frame(df)) {
    if (vb) message(paste0(" Imported data frame."))
    fn <- paste0(stringr::str_pad(vol_id, 4, pad = "0"), "-sess-materials.csv")
    out_fn <- file.path(dir, fn)
    if (vb) message(" Saving ", out_fn)
    readr::write_csv(df, out_fn)
  } else {
    if (vb) message("Failed to convert to data frame.")
    NULL
  }
}

################################################################################
get_volume_birthdate <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryapi::download_session_csv(vol_id), silent = TRUE)
  if (vb)
    message(paste0(
      "....Gathering participant_birthdate data from volume ",
      vol_id
    ))
  
  if ("participant_birthdate" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_birthdate)
  } else {
    if (vb)
      message(".....participant_birthdate not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_birthdate = NA
    )
  }
}

################################################################################
get_volume_session_date <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryapi::download_session_csv(vol_id), silent = TRUE)
  if (vb)
    message(paste0("....Gathering session_date data from volume ",
                   vol_id))
  
  if ("session_date" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_date)
  } else {
    message(".....session_date not found for volume ", vol_id)
    data.frame(vol_id = vol_id,
               session_id = NA,
               session_date = NA)
  }
}

################################################################################
get_volume_gender <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryapi::download_session_csv(vol_id), silent = TRUE)
  if (vb)
    message(paste0("....Gathering participant_gender data from volume ", vol_id))
  
  if ("participant_gender" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_gender)
  } else {
    if (vb)
      message(".....participant_gender not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_gender = NA
    )
  }
}

################################################################################
get_volume_race <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryapi::download_session_csv(vol_id), silent = TRUE)
  if (vb)
    message(paste0("....Gathering participant_race data from volume ", vol_id))
  
  if ("participant_race" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_race)
  } else {
    if (vb)
      message(".....participant_race not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_race = NA
    )
  }
}

################################################################################
get_volume_ethnicity <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryapi::download_session_csv(vol_id), silent = TRUE)
  if (vb)
    message(paste0(
      "....Gathering participant_ethnicity data from volume ",
      vol_id
    ))
  
  if ("participant_ethnicity" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_ethnicity)
  } else {
    if (vb)
      message(".....participant_ethnicity not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_ethnicity = NA
    )
  }
}

################################################################################
get_volumes_demo <- function(min_vol_id = 1,
                             max_vol_id = 10,
                             vb = FALSE) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(is.numeric(max_vol_id))
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  
  vols_range <- min_vol_id:max_vol_id
  
  db_status <- databraryapi::login_db(Sys.getenv("DATABRARY_LOGIN"))
  if (!db_status) {
    message("Unable to login to Databrary. Only public data will be gathered")
  }
  
  if(vb) message(paste0(
    "Getting demographic data for volumes ",
    min_vol_id,
    ":",
    max_vol_id,
    "\n"
  ))
  
  ml <-
    purrr::map_df(vols_range, get_volume_demog, vb, .progress = "Volume demog:")
  
  databraryapi::logout_db()
  
  purrr::list_rbind(ml)
}

################################################################################
get_volume_demo_save_csv_mult <- function(min_vol_id = 1,
                             max_vol_id = 10,
                             csv_dir = 'src/csv',
                             vb = FALSE) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(is.numeric(max_vol_id))
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  vols_range <- min_vol_id:max_vol_id
  
  db_status <- databraryapi::login_db(Sys.getenv("DATABRARY_LOGIN"))
  if (!db_status) {
    message("Unable to login to Databrary. Only public data will be gathered")
  }
  
  if(vb) message(paste0(
    "Getting demographic data for volumes ",
    min_vol_id,
    ":",
    max_vol_id,
    "\n"
  ))
  
  purrr::map(vols_range, get_volume_ss_save_csv, csv_dir, vb, .progress = "Volume ss:")
  
  databraryapi::logout_db()
}

################################################################################
save_volumes_demo <- function(df, min_vol_id, max_vol_id,
                              dir = "participant-demographics/csv") {
  message(paste0(
    "Saving demographic data for volumes ",
    min_vol_id,
    "-",
    max_vol_id,
    "\n"
  ))
  fn <-
    paste0(
      dir,
      "/",
      stringr::str_pad(min_vol_id, 4, pad = "0"),
      "-",
      stringr::str_pad(max_vol_id, 4, pad = "0"),
      "-demog.csv"
    )
  fn
  readr::write_csv(df, fn)
}

# Won't work because participants could have different demographic characteristics
n_particip_in_session <- function(df) {
  p_indices <- stringr::str_match(names(v2), "participant([0-9]+)")[,2]
  p_indices <- p_indices[!is.na(p_indices)]
  max(as.numeric(p_indices))
}

################################################################################
#` 
pivot_ss_longer <- function(df) {
  require(tidyverse)
  df %>%
    tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-ID'), names_to = NULL, values_to = "participant-id") %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-birthdate'), names_to = NULL, values_to = "participant-birthdate") %>%
    # dplyr::distinct() %>%
    # tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-race'), names_to = NULL, values_to = "participant-race") %>%
    # dplyr::distinct() %>%
    # tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-gender'), names_to = NULL, values_to = "participant-gender") %>%
    # dplyr::distinct() %>%
    # tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-ethnicity'), names_to = NULL, values_to = "participant-ethnicity") %>%
    dplyr::distinct()
}

################################################################################
# Downloads new demographic data from Databrary and saves it as a new CSV
#
# For efficiency reasons, the code takes 10 volumes at a time and saves them
# in individual CSVs labeled with the volume numbers.
get_save_volumes_demo <- function(min_vol_id = 1,
                                  max_vol_id = 10,
                                  dir = "src/csv",
                                  vb = FALSE) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(is.numeric(max_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  stopifnot(is.character(dir))
  stopifnot(dir.exists(dir))
  
  suppressPackageStartupMessages(require(tidyverse))
  
  df <- get_volumes_demo(min_vol_id, max_vol_id, vb)
  save_volumes_demo(df, min_vol_id, max_vol_id, dir)
}

regenerate_vol_demo_csvs <- function(new_vol_rg_min = 1261,
                                     new_vol_rg_max = 1270,
                                     csv_dir = "participant-demographics/csv",
                                     vb = FALSE) {
  require(purrr)
  
  if (!is.numeric(new_vol_rg_min)) {
    stop('`new_vol_rg_min` must be a number.')
  }
  if (new_vol_rg_min < 1) {
    stop('`new_vol_rg_min` must > 0.')
  }
  if (!is.numeric(new_vol_rg_max)) {
    stop('`new_vol_rg_max` must be a number.')
  }
  if (new_vol_rg_max < 1) {
    stop('`new_vol_rg_max` must > 0.')
  }
  if (new_vol_rg_min > new_vol_rg_max) {
    stop('`new_vol_rg_min` must < `new_vol_rg_max.')
  }
  if (!is_character(csv_dir)) {
    stop('`csv_dir` must be a character string.')
  }
  if (!dir.exists(csv_dir)) {
    stop('Directory not found: `', csv_dir, '`.')
  }
  
  lo <- seq(from = new_vol_rg_min, to = new_vol_rg_max, by = 10)
  hi <- seq(from = new_vol_rg_min + 9, to = new_vol_rg_max, by = 10)
  
  # Rick really loves functional programming
  purrr::map2(.x = lo,
              .y = hi,
              get_save_volumes_demo,
              csv_dir,
              vb)
}

load_demog_csvs <- function(dir = "participant-demographics/csv") {
  if (!is.character(dir)) {
    stop("'dir' must be a string")
  }
  if (!dir.exists(dir)) {
    stop("'dir' does not exist.")
  }
  
  fl <- list.files(dir, "-demog\\.csv$", full.names = TRUE)
  if (is.null(fl)) {
    stop(paste0("No csv files in ", dir, "."))
  }
  purrr::map_dfr(fl,
                 readr::read_csv,
                 col_types = 'c',
                 show_col_types = FALSE)
}

get_volumes_owners <- function(min_vol_id = 1,
                               max_vol_id = 10,
                               vb = FALSE) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(is.numeric(max_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  
  vols_range <- min_vol_id:max_vol_id
  if (vb)
    message("Gathering owners from volumes ", min_vol_id, ":", max_vol_id)
  purrr::map_dfr(
    .x = vols_range,
    .f = databraryapi::list_volume_owners,
    .progress = "Volume owners:"
  )
}

get_volume_first_owner <- function(vol_id) {
  df <- databraryapi::list_volume_owners(vol_id)
  if (rlang::is_empty(df)) {
    NULL
  } else {
    df[1,]
  }
}

get_volumes_first_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           vb = FALSE) {
    stopifnot(is.numeric(min_vol_id))
    stopifnot(is.numeric(max_vol_id))
    stopifnot(min_vol_id > 0)
    stopifnot(max_vol_id > 0)
    stopifnot(min_vol_id < max_vol_id)
    
    vols_range <- min_vol_id:max_vol_id
    if (vb)
      message("Gathering first owners from volumes ",
              min_vol_id,
              ":",
              max_vol_id)
    purrr::map_dfr(.x = vols_range,
                   .f = get_volume_first_owner,
                   .progress = "Volume 1st owners:")
  }

save_volumes_owners <- function(df,
                                min_vol_id,
                                max_vol_id,
                                dir = "src/csv",
                                fn_suffix = "-owners.csv",
                                vb = FALSE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.numeric(min_vol_id))
  stopifnot(is.numeric(max_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  stopifnot(is.character(dir))
  stopifnot(dir.exists(dir))
  
  fn <-
    paste0(
      dir,
      "/",
      stringr::str_pad(min_vol_id, 4, pad = "0"),
      "-",
      stringr::str_pad(max_vol_id, 4, pad = "0"),
      fn_suffix
    )
  if (vb)
    message(paste0("Saving volume owner data to ", fn, "."))
  readr::write_csv(df, fn)
}

get_save_volumes_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           dir = "src/csv",
           vb = FALSE) {
    stopifnot(is.numeric(min_vol_id))
    stopifnot(is.numeric(max_vol_id))
    stopifnot(min_vol_id > 0)
    stopifnot(max_vol_id > 0)
    stopifnot(min_vol_id < max_vol_id)
    stopifnot(is.character(dir))
    stopifnot(dir.exists(dir))
    
    if (vb)
      message(paste0("Getting owner data for volumes ", min_vol_id, ":", max_vol_id))
    df <- get_volumes_owners(min_vol_id, max_vol_id, vb)
    if (vb)
      message(paste0("Saving owner data for volumes ", min_vol_id, "-", max_vol_id))
    save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-owners.csv", vb)
  }

get_save_volumes_first_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           dir = "src/csv",
           vb = FALSE) {
    stopifnot(is.numeric(min_vol_id))
    stopifnot(is.numeric(max_vol_id))
    stopifnot(min_vol_id > 0)
    stopifnot(max_vol_id > 0)
    stopifnot(min_vol_id < max_vol_id)
    stopifnot(is.character(dir))
    stopifnot(dir.exists(dir))
    
    if (vb)
      message(paste0(
        "Getting first owner data for volumes ",
        min_vol_id,
        ":",
        max_vol_id
      ))
    
    df <- get_volumes_first_owners(min_vol_id, max_vol_id)
    
    if (vb)
      message(paste0(
        "Saving first owner data for volumes ",
        min_vol_id,
        ":",
        max_vol_id
      ))
    
    save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-first-owners.csv", vb)
  }

get_all_owners_save_csvs <-
  function(max_vol_id = 1520,
           dir = "src/csv",
           vb = FALSE) {
    stopifnot(is.numeric(max_vol_id))
    stopifnot(max_vol_id > 0)
    stopifnot(is.character(dir))
    stopifnot(dir.exists(dir))
    
    # TODO: Fix this hack
    get_save_volumes_owners(1, 500)
    get_save_volumes_owners(501, 1000)
    get_save_volumes_owners(1001, 1275) # skip 1276 & 1277 because no owners
    get_save_volumes_owners(1278, 1500)
    get_save_volumes_owners(1501, max_vol_id)
  }

load_owner_csvs <- function(dir = "src/csv",
                            fn_suffix = "-owners") {
  if (!is.character(dir)) {
    stop("'dir' must be a string")
  }
  if (!dir.exists(dir)) {
    stop("'dir' does not exist.")
  }
  
  fl <-
    list.files(dir, paste0(fn_suffix, "\\.csv$"), full.names = TRUE)
  if (is.null(fl)) {
    stop(paste0("No csv files in ", dir, "."))
  }
  purrr::map_dfr(fl,
                 readr::read_csv,
                 col_types = 'c',
                 show_col_types = FALSE)
}

render_participant_demog_report <- function(db_login) {
  if (!is.character(db_login)) {
    stop('`db_login` must be a character string.')
  }
  
  rmarkdown::render(
    "participant-demographics/participant-demog-report.Rmd",
    params = list(db_login = db_login)
  )
  databraryapi::logout_db()
  if (file.exists("participant-demographics/.databrary.RData")) {
    file.remove("participant-demographics/.databrary.RData")
  }
}

my_gender <- function(v, a) {
  if (a == 'warning') {
    warning('some warning')
    return_value <- get_volume_gender(v)
  } else if (a == 'error') {
    warning('Error detected, ignored.')
    return_value = NULL
  } else {
    return_value <- get_volume_gender(v)
  }
}

################################################################################
get_volume_ss <- function(vol_id = 1,
                          omit_materials = FALSE,
                          vb = FALSE) {
  message(paste0("Gathering spreadsheet data from volume ", vol_id))
  
  if (!vol_csv_avail(vol_id)) {
    if (vb)
      message("No CSV available for volume ", vol_id)
    return(NULL)
  }
  
  v_ss <-
    try(databraryapi::download_session_csv(vol_id), silent = TRUE)
  
  if ('try-error' %in% class(v_ss)) {
    message(".Error loading CSV for volume ", vol_id)
    df <- NULL
    return(df)
  } else if (is.null(v_ss)) {
    message(".NULL CSV for volume ", vol_id)
    df <- NULL
    return(df)
  } else {
    df <- v_ss
    if (omit_materials) {
      df <-
        dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
    }
    df$vol_id <- vol_id
    return(df)
  }
}

get_volume_ss_httr <- function(vol_id = 1,
                               omit_materials = FALSE,
                               vb = FALSE) {
  
  
}

################################################################################
get_volume_ss_vroom <- function(vol_id = 4,
                                omit_materials = FALSE,
                                vb = FALSE) {
  if (vb)
    message(paste0("Gathering spreadsheet data from volume ", vol_id))
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  if (!vol_csv_avail(vol_id)) {
    if (vb)
      message("No CSV available for volume ", vol_id)
    return(NULL)
  }
  
  v_ss <-
    try(vroom::vroom(this_url, delim = ",", col_types = "cccccccccccccccccccc",
                     show_col_types = FALSE),
        silent = TRUE)
  if (inherits(v_ss, 'try-error')) {
    if (vb)
      message(" Error loading CSV from ", this_url)
    df <- NULL
    return(df)
  } else if (is.null(v_ss)) {
    if (vb)
      message(" NULL CSV for volume ", vol_id)
    df <- NULL
    return(df)
  } else {
    df <- v_ss
    names(df) <- stringr::str_replace_all(names(df), '[-\\. ]', '_')
    if (omit_materials) {
      df <-
        dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
    }
    df$vol_id <- as.character(vol_id)
    return(df)
  }
}

get_save_volume_ss <- function(vol_id = 4,
                               dir = "src/csv",
                               omit_materials = FALSE,
                               vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.character(dir))
  stopifnot(is.logical(omit_materials))
  stopifnot(is.logical(vb))
  
  # df <- get_volume_ss(vol_id, omit_materials, vb)
  df <- get_volume_ss_vroom(vol_id, omit_materials, vb)
  
  if (!is.null(df)) {
    if (omit_materials) {
      fn <-
        paste0(dir,
               "/",
               stringr::str_pad(vol_id, 4, pad = "0"),
               "-sess.csv")
    } else {
      fn <-
        paste0(dir,
               "/",
               stringr::str_pad(vol_id, 4, pad = "0"),
               "-sess-materials.csv")
    }
    
    readr::write_csv(df, fn)
    if (vb)
      message(" Saved ", fn)
    
  } else {
    if (vb)
      message(" No data available for volume: ", vol_id)
  }
}

get_save_multiple_volume_ss <-
  function(vol_id_min,
           vol_id_max,
           csv_dir = "src/csv",
           omit_materials = FALSE,
           vb = FALSE) {

    stopifnot(is.numeric(vol_id_min))
    stopifnot(is.numeric(vol_id_max))
    stopifnot(vol_id_min > 0)
    stopifnot(vol_id_max > 0)
    stopifnot(vol_id_min < vol_id_max)
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    db_status <- databraryapi::login_db(Sys.getenv("DATABRARY_LOGIN"))
    if (!db_status) {
      message("Unable to login to Databrary. Only public data will be gathered")
    }
    
    purrr::map(
      c(vol_id_min:vol_id_max),
      get_save_volume_ss,
      csv_dir,
      omit_materials,
      vb,
      .progress = "Volume ss:"
    )
  }

# Returns a tibble
extract_sessions_from_vol_csv <-
  function(csv_fn = "src/csv/0002-sess-materials.csv", vb = FALSE) {
    stopifnot(is.character(csv_fn))
    stopifnot(file.exists(csv_fn))
    
    df <- read.csv(csv_fn, colClasses = "character")
    
    if (dim(df)[1] == 0) {
      if (vb) message("CSV empty: ", csv_fn)
      NULL
    } else {
      dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
    }
  }

# Returns a tibble
count_sessions_materials_folders <-
  function(csv_fn = "participant-demographics/csv/0002-sess-materials.csv") {
    stopifnot(is.character(csv_fn))
    stopifnot(file.exists(csv_fn))
    
    df <- readr::read_csv(csv_fn, show_col_types = FALSE)
    
    if (dim(df)[1] == 0) {
      message("CSV empty: ", csv_fn)
      NULL
    } else {
      df_sess <-
        dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
      n_sess <- dim(df_sess)[1]
      
      df_materials <-
        dplyr::filter(df, (stringr::str_detect(session_date, 'materials')))
      n_materials <- dim(df_materials)[1]
      
      vol_id <- stringr::str_extract(csv_fn, "[0-9]{4}")
      
      tibble::tibble(vol_id, n_sess, n_materials)
    }
    
  }

detect_n_participants <- function(df) {
  sum(stringr::str_detect(names(df), "participant[1-9]?_ID"))
}

generate_sessions_materials_df <-
  function(csv_folder = "participant-demographics/csv") {
    stopifnot(is.character(csv_folder))
    stopifnot(file.exists(csv_folder))
    
    fl <-
      list.files(csv_folder,
                 "[0-9]{4}\\-sess\\-materials\\.csv",
                 full.names = TRUE)
    if (length(fl) <= 0) {
      message("No session/materials CSV files found: ", csv_folder)
      NULL
    } else {
      purrr::map_df(fl, count_sessions_materials_folders)
    }
  }

extract_participant_vars <- function(df) {
  stopifnot(is.data.frame(df))
  
  stringr::str_match(names(df), "participant[1-9]?_([A-Za-z]+)")[, 2]
}

extract_participant_identifiers <- function(df) {
  stopifnot(is.data.frame(df))
  
  x <- unique(stringr::str_extract(names(df), "participant[1-9]?"))
  x[!is.na(x)]
}

# If there are multiple participants, augmented data frame
alter_sess_df_w_mult_part <- function(df, vb = FALSE) {
  if (is.null(df))
    return(NULL)
  stopifnot(is.data.frame(df))
  
  n_particip <- detect_n_participants(df)
  if (n_particip > 0) {
    purrr::map_df(1:n_particip, select_particip_sess_by_number, df)
  } else {
    if (vb) message(" No participant data:...skipping")
    NULL
  }
}

# Helper function to extract data frame for participant1, participant2, etc.
select_particip_sess_by_number <- function(i, df) {
  stopifnot(is.numeric(i))
  stopifnot(i > 0)
  stopifnot(is.data.frame(df))
  
  vars_not_part_specific <-
    !(stringr::str_detect(names(df), "participant"))
  if (i == 1) {
    part_specific_vars <-
      stringr::str_detect(names(df), "participant[1]?_")
  } else {
    part_specific_vars <-
      stringr::str_detect(names(df), paste0("participant[", i, "]{1}"))
  }
  select_these <- vars_not_part_specific | part_specific_vars
  out_df <- df[, select_these]
  stripped_names <-
    stringr::str_replace(names(out_df),
                         paste0("participant[", i, "]{1}"),
                         "participant")
  names(out_df) <- stripped_names
  out_df
}

make_cleaned_session_df <- function(csv_fn, vb = FALSE) {
  stopifnot(is.character(csv_fn))
  stopifnot(file.exists(csv_fn))
  
  if (vb) message("Extracting session info: ", csv_fn)
  df <- extract_sessions_from_vol_csv(csv_fn, vb)
  alter_sess_df_w_mult_part(df, vb)
}

download_csv_vroom <- function(vol_id = 4, vb = FALSE) {
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  x <-
    try(vroom::vroom(this_url, col_types = "cccccccccccccccccccc",  delim = ","), silent = TRUE)
  if (inherits(x, 'try-error'))  {
    if (vb)
      message("Error in downloading spreadsheet from  ", this_url)
    return(NULL)
  } else {
    names(x) <- stringr::str_replace_all(names(x), '[-\\. ]', '_')
    x$vol_id <- as.character(vol_id)
    x
  }
}

################################################################################
## New demog functions as of 2023-02-08

extract_particip_info <- function(df) {
  select(df, contains('participant'))
}

remove_materials <- function(df) {
  filter(df, !str_detect(`session-date`, '[Mm]aterials'))
}

extract_unique_participant_info <- function(df, vb = FALSE) {
  if(vb) message("Extracting participant info.")
  df %>%
    remove_materials() %>%
    extract_particip_info() %>%
    distinct()
}

extract_single_participant <- function(i, df) {
  d <- select(df, contains(paste0('participant', i)))
  rename_with(d, ~ gsub("[0-9]+", "", .x))
}

extract_participant_ids <- function(df) {
  these_cols <- colnames(df)
  str_extract(these_cols, '([0-9]+)') |> unique()
}

convert_vol_ss_to_particip_df <- function(df, vol_id, vb = FALSE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(vol_id))
  
  unique_p_info <- extract_unique_participant_info(df, vb)
  if (dim(unique_p_info)[2] == 0) {
    if (vb) message("No participant data.")
    return(NULL)
  }
  if (vb) message("Making participant df.")
  p_ids <- extract_participant_ids(unique_p_info)
  if (length(p_ids) > 1) {
    df <- purrr::map(p_ids, extract_single_participant, unique_p_info) |>
      list_rbind()
  } else {
    df <- unique_p_info
  }
  df$vol_id <- vol_id
  df
}

convert_vol_ss_csv_to_particip_df <- function(csv_fn,
                                              vb = FALSE) {
  stopifnot(is.character(csv_fn))
  
  vol_id <- unique(str_extract(basename(csv_fn), '([0-9]{4})'))
  
  df <- readr::read_csv(csv_fn, col_types = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
                        show_col_types = FALSE)
  convert_vol_ss_to_particip_df(df, vol_id, vb)
}

create_aggregate_demog_df <- function(csv_dir = 'src/csv', vb = FALSE) {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  fl <- list.files(csv_dir, "[0-9]+\\-sess\\-materials.csv", full.names = TRUE)
  df <- purrr::map(fl, convert_vol_ss_csv_to_particip_df, vb, .progress = "Particip df") %>%
    list_rbind()
  
  rename_with(df, ~ gsub("[- ]", "_", .x))
}

create_complete_demog_df <- function(fl, vb = FALSE) {
  stopifnot(is.character(fl))
  df <- purrr::map(fl, convert_vol_ss_csv_to_particip_df, vb, .progress = "Particip df") %>%
    list_rbind()
  
  rename_with(df, ~ gsub("[- ]", "_", .x))
}

