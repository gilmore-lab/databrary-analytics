# institution_investigator_functions.R

################################################################################
# Institutions and Authorized Investigators (AIs)
#
# These functions process data about institutions and authorized investigators

#-------------------------------------------------------------------------------
update_invest_csv <- function(all_inst_df,
                              csv_dir = "src/csv",
                              vb = FALSE) {
  # Filter
  # if (vb) message("Getting updated institutional info")
  # all_inst_df <- get_all_inst(csv_dir, save_csv = TRUE, vb)
  
  if (vb)
    message("Filtering for active institutions with AIs.")
  inst_ids <-
    dplyr::filter(all_inst_df, daa == TRUE, n_auth_invest > 0) |>
    dplyr::select(inst_id)
  ids <- as.integer(unlist(inst_ids))
  
  if (vb)
    message("There are n=",
            dim(ids)[1],
            " institutions with AIs. Retrieving AI info.")
  
  ais_l <-
    purrr::map(ids, get_ais_from_inst, vb, .progress = "AIs from insts:")
  
  if (vb)
    message("Making data frame.")
  ais_df <- purrr::list_rbind(ais_l)
  
  fn <- file.path(csv_dir, "all-ais.csv")
  if (vb)
    message("Writing CSV: ", fn)
  readr::write_csv(ais_df, fn)
}

#-------------------------------------------------------------------------------
get_all_inst <- function(csv_dir = "src/csv",
                         save_csv = TRUE,
                         vb = FALSE) {
  inst_fl <- list.files(csv_dir, "inst\\-[0-9]+", full.names = TRUE)
  
  if (vb)
    message("Loading institution CSVs from ", csv_dir)
  inst_l <-
    purrr::map(inst_fl, readr::read_csv, show_col_types = FALSE)
  
  if (vb)
    message("Making dataframe.")
  inst_df <- purrr::list_rbind(inst_l)
  
  if (save_csv) {
    fn <- file.path(csv_dir, "all-institutions.csv")
    if (vb)
      message("Writing ", fn)
    readr::write_csv(inst_df, fn)
  }
  inst_df
}

#-------------------------------------------------------------------------------
get_ais_from_inst <- function(inst_id = 8, vb = FALSE) {
  if (vb)
    message("Getting AIs from institution ", inst_id)
  inst_df <- databraryapi::list_party(inst_id)
  
  if (!is.null(dim(inst_df$children))) {
    ais_df <- as.data.frame(inst_df$children$party)
    ais_df <- dplyr::rename(ais_df,
                            ai_id = id,
                            ai_last = sortname,
                            ai_first = prename)
    
    df <- tibble::tibble(ais_df)
    df <- dplyr::mutate(
      df,
      inst_id = inst_df$id,
      inst_name = inst_df$sortname,
      inst_db_url = paste0("https://nyu.databrary.org/party/", inst_df$id),
      ai_db_url = paste0("https://nyu.databrary.org/party/", ai_id)
    )
    df$n_affils <- count_affiliates_for_ais(df$ai_id)
    
    df <- dplyr::arrange(df, desc(n_affils), ai_last, ai_first)
    df
  } else {
    NULL
  }
}

#-------------------------------------------------------------------------------
count_affiliates_for_ai <- function(ai_id) {
  affils <- databraryapi::list_affiliates(ai_id)
  if (is.null(affils)) {
    x <- 0
  } else {
    x <- dim(affils)[1]
  }
  x
}

#-------------------------------------------------------------------------------
count_affiliates_for_ais <- function(ai_ids) {
  purrr::map_dbl(ai_ids, count_affiliates_for_ai)
}

#-------------------------------------------------------------------------------
update_inst_csv <- function(csv_fn = "src/csv/institutions.csv",
                            max_id = 10868,
                            save_new = TRUE,
                            update_geo = FALSE,
                            vb = FALSE) {
  if (file.exists(csv_fn)) {
    if (vb)
      message("Reading from saved file.")
    old_inst <- readr::read_csv(csv_fn, show_col_types = FALSE)
    max_old_inst_id <- unique(max(old_inst$inst_id))
    if (max_old_inst_id < max_id) {
      if (vb)
        message(" Retrieving data from `party_id` ",
                max_old_inst_id + 1,
                ":",
                max_id)
      new_inst <-
        purrr::map_df((max_old_inst_id + 1):max_id,
                      get_inst_info,
                      update_geo,
                      .progress = "Geo coords:")
      if (vb)
        message(" ", paste0(dim(new_inst)[1], " new institutions added."))
      df <- rbind(old_inst, new_inst)
      if (save_new) {
        if (vb)
          message(paste0(" Writing new file: ", csv_fn))
        write_csv(df, csv_fn)
      }
    } else {
      if (vb)
        message(" No update needed.")
      df <- old_inst
    }
  } else {
    if (vb)
      message("No file found. Recreating from inst_id 1:", max_id)
    df <-
      purrr::map_df(1:max_id,
                    get_inst_info,
                    update_geo = update_geo,
                    vb,
                    .progress = "Inst :")
    if (save_new) {
      if (vb)
        message(paste0(" Writing new file: ", csv_fn))
      write_csv(df, csv_fn)
    }
  }
  df
}

#-------------------------------------------------------------------------------
get_inst_info <-
  function(inst_id = 8,
           update_geo = FALSE,
           vb = FALSE) {
    if (!is.numeric(inst_id)) {
      warning('`inst_id` must be a number.')
      inst_id <- as.numeric(inst_id)
    }
    
    suppressPackageStartupMessages(require(databraryapi))
    
    if (!db_credentials_valid()) {
      message(
        "Not logged in to Databrary. Running `databraryapi::login_db()` with default credentials."
      )
      databraryapi::login_db(Sys.getenv("DATABRARY_LOGIN"))
      return(NULL)
    }
    
    if (inst_id > 0) {
      if (databraryapi::is_institution(inst_id)) {
        if (vb)
          message("Getting information for institution ", inst_id)
        
        inst_df <- databraryapi::list_party(inst_id)
        df <- data.frame(
          inst_id = inst_df$id,
          inst_name = inst_df$sortname,
          inst_url = ifelse('url' %in% names(inst_df), inst_df$url, NA),
          databrary_url = paste0("https://nyu.databrary.org/party/", inst_id)
        )
        if (!is.null(dim(inst_df$children))) {
          df$n_auth_invest <- dim(inst_df$children)[1]
        } else {
          df$n_auth_invest = 0
        }
        if (!is.null(dim(inst_df$parents))) {
          df$daa <- TRUE
        } else {
          df$daa <- FALSE
        }
        # Get lat and lon
        if (update_geo == TRUE) {
          if (vb) message(" Updating lat/lon coords.")
          df <- update_inst_lat_lon(df, vb)
        } else {
          df$lat = NA
          df$lon = NA
        }
        df
      } else {
        if (vb)
          message("Party ", inst_id, " is not an institution. Skipping.")
        NULL
      }
    } else {
      if (vb)
        message("`inst_id` must be a positive number.")
    }
  }

#-------------------------------------------------------------------------------
get_inst_info_save_csv <-
  function(party_id = 8,
           update_geo = FALSE,
           csv_dir = "src/csv",
           vb = FALSE,
           non_insts = c(2, 9, 10, 12, 15)) {
    stopifnot(is.numeric(party_id))
    stopifnot(party_id > 0)
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    if (party_id %in% non_insts) {
      if (vb)
        message("Party ", party_id, " is not a physical institution. Skipping.")
      return(NULL)
    }
    this_inst <- get_inst_info(party_id, update_geo, vb)
    
    if (!is.null(this_inst)) {
      fn <-
        file.path(csv_dir, paste0("inst-", stringr::str_pad(party_id, 5, pad = "0"), ".csv"))
      if (vb)
        message(" Saving ", fn)
      readr::write_csv(this_inst, fn)
    }
  }


#-------------------------------------------------------------------------------
get_save_many_inst_csvs <-
  function(min_id = 1,
           max_id = 100,
           update_geo = FALSE,
           csv_dir = 'src/csv',
           vb = FALSE) {
    stopifnot(is.numeric(min_id))
    stopifnot(min_id > 0)
    stopifnot(is.numeric(max_id))
    stopifnot(max_id > 0)
    stopifnot(max_id >= min_id)
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    if (!ggmap::has_google_key()) {
      if (vb) message("No Google maps key found. No geo info will be updated.")
      update_geo = FALSE
    }
    
    purrr::walk(min_id:max_id,
                get_inst_info_save_csv,
                update_geo,
                csv_dir,
                vb,
                .progress = "Inst CSVs:")
  }

#-------------------------------------------------------------------------------
load_inst_df_from_csvs <- function(csv_fl, vb = FALSE) {
  stopifnot(is.character(csv_fl))
  
  if (vb)
    message("Creating data frame from institution CSVs.")
  purrr::map(csv_fl, readr::read_csv, show_col_types = FALSE) |> purrr::list_rbind()
}

#-------------------------------------------------------------------------------
update_inst_lat_lon <- function(inst_df, vb = FALSE) {
  stopifnot(is.data.frame(inst_df))
  
  if (vb) message(" Calling ggmap::geocode() with '", inst_df$inst_name, "'.")
  ll <- ggmap::geocode(as.character(inst_df$inst_name), override_limit = TRUE)
  
  inst_df$lat = NA
  inst_df$lon = NA
  
  if (!is.na(ll$lat))
    inst_df$lat <- ll$lat
  if (!is.na(ll$lon))
    inst_df$lon <- ll$lon
  
  inst_df
}

#-------------------------------------------------------------------------------
extract_inst_csv_id <- function(csv_dir = "src/csv", vb = FALSE) {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  inst_fl <- list.files(csv_dir, "^inst\\-[0-9]{5}")
  as.numeric(stringr::str_extract(inst_fl, "[0-9]{5}"))
}
