# make_institutional_party_df
# 
# Creates a data frame of an institution that has authorized investigators
require(here)
require(databraryapi)
require(tidyverse)

make_institutional_party_df <- function(inst_id) {
  if (!is.numeric(inst_id)) {
    stop('`inst_id` must be a number.')
  }
  if (inst_id <= 0) {
    warning('`inst_id` must be >= 0')
    return(NULL)
  }
  
  require(databraryapi)
  require(purrr)
  
  # if (!db_credentials_valid()){
  #   message("Not logged in to Databrary. Run `databraryapi::login_db()`.")
  #   return(NULL)
  # }
  
  if (databraryapi::is_institution(inst_id)) {
    message("Retrieving investigators at institution ", inst_id, '.')
    these_affiliates <- databraryapi::list_affiliates(inst_id)
    if (purrr::is_empty(these_affiliates) | is.null(these_affiliates)) {
      df <- data.frame(inst_id = inst_id,
                       affiliation = NA,
                       inst_url = paste0("https://nyu.databrary.org/party/", inst_id),
                       party_id = NA,
                       sortname = NA,
                       prename = NA,
                       party_url = NA,
                       url = NA,
                       email = NA)    
    } else {
      df <- these_affiliates
      df$inst_id <- inst_id
      df <- dplyr::rename(df, party_id = id)
      df <- dplyr::mutate(df, inst_url = paste0("https://nyu.databrary.org/party/", inst_id),
                          party_url = paste0("https://nyu.databrary.org/party/", party_id))
      df <- dplyr::select(df, 
                          inst_id, affiliation, inst_url, party_id, sortname, prename, party_url, email)
    } 
    df
  } else {
    NULL
  }
}

get_inst_info <- function(inst_id = 8, update_geo = FALSE) {
  if (!is.numeric(inst_id)) {
    warning('`inst_id` must be a number.')
    inst_id <- as.numeric(inst_id)
  }

  require(databraryapi)
  
  if (!db_credentials_valid()){
    warning("Not logged in to Databrary. Run `databraryapi::login_db()`.")
    return(NULL)
  }
  
  if (inst_id > 0) {
    if (databraryapi::is_institution(inst_id)) {
      message("Getting information for institution ", inst_id)
      
      inst_df <- list_party(inst_id)
      df <- data.frame(inst_id = inst_df$id,
                       inst_name = inst_df$sortname,
                       inst_url = ifelse('url' %in% names(inst_df), inst_df$url, NA),
                       databrary_url = paste0("https://nyu.databrary.org/party/", inst_id))
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
        df <- update_inst_lat_lon(df)        
      } else {
        df$lat = NA
        df$lon = NA
      }
      df
    } else {
      NULL
    }
  } else {
    message("`inst_id` must be a positive number.")
  }
}

write_inst_csv <- function(min_inst_id = 1, max_inst_id = 25, 
                           csv_fn = paste0(here::here(), 
                                                    "/institutions-investigators/csv/institutions.csv")) {
  if (!is.numeric(min_inst_id)) {
    stop('`min_inst_id` must be a number.')
  }
  if (!is.numeric(max_inst_id)) {
    stop('`min_inst_id` must be a number.')
  }
  if (!is.character(csv_fn)) {
    stop('`csv_fn` must be a character string.')
  }
  if (!dir.exists(csv_fn)) {
    stop(paste0('Directory not found: `', csv_fn, '`.' ))
  }
  
  if (min_inst_id <= max_inst_id) {
    message("Retrieving data from inst_id ", min_inst_id, ":", max_inst_id)
    df <- purrr::map_df(min_inst_id:max_inst_id, get_inst_info)
    if (!is.null(df)) {
      message(paste0("Writing new file: ", csv_fn))
      write_csv(df, csv_fn)      
    } else {
      message('Unable to generate data frame.')
      NULL
    }
  } else {
    message("`min_inst_id` must be less than `max_inst_id")
  }
}

get_max_institutional_id <- function(csv_fn = paste0(here::here(), 
                                                     "/institutions-investigators/csv/institutions-investigators.csv")) {
  if (file.exists(csv_fn)) {
    message("Reading from saved file.")
    inst_invest <- readr::read_csv(csv_fn, show_col_types = FALSE)
    unique(max(inst_invest$inst_id))
  } else {
    message(paste0("File not found: ", csv_fn))
  }
}

update_inst_csv <- function(csv_fn = paste0(here::here(), 
                                                   "/institutions-investigators/csv/institutions.csv"),
                                   max_id = 9000,
                                   save_new = TRUE,
                            update_geo = FALSE) {

  if (file.exists(csv_fn)) {
    message("Reading from saved file.")
    old_inst <- readr::read_csv(csv_fn, show_col_types = FALSE)
    max_old_inst_id <- unique(max(old_inst$inst_id))
    if (max_old_inst_id < max_id) {
      message("Retrieving data from `party_id` ", max_old_inst_id + 1, ":", max_id)
      new_inst <- purrr::map_df((max_old_inst_id + 1):max_id, get_inst_info, update_geo)
      message(paste0(dim(new_inst)[1], " new institutions added."))
      df <- rbind(old_inst, new_inst)
      if (save_new) {
        message(paste0("Writing new file: ", csv_fn))
        write_csv(df, csv_fn)
      }
    } else {
      message("No update needed.")
      df <- old_inst
    }
  } else {
    message("No file found. Recreating from inst_id 1:", max_id)
    df <- purrr::map_df(1:max_id, get_inst_info, update_geo = update_geo)
    if (save_new) {
      message(paste0("Writing new file: ", csv_fn))
      write_csv(df, csv_fn)
    }
  }
  df
}

resort_inst_fields <- function(csv_fn = paste0(here::here(), 
                                               "/institutions-investigators/csv/institutions-investigators.csv")) {
  if (file.exists(csv_fn)) {
    inst_df <- readr::read_csv(csv_fn, show_col_types = FALSE)
    inst_df <- dplyr::select(inst_df, party_id, sortname, prename, affiliation, email,
                             inst_id, inst_url, party_url)
    write_csv(inst_df, csv_fn)
  } else {
    message(paste0("File not found: ", csv_fn))    
  }
}

get_institution_party_df <- function(min_inst_id = 1, max_inst_id = 20) {
  if (min_inst_id <= max_inst_id) {
    purrr::map_df(min_inst_id:max_inst_id, make_institutional_party_df)
  } else {
    message("`min_inst_party` must be less than `max_inst_id`")
    NULL
  }
} 


# Authorized investigator report

get_auth_inst_list <- function(update_inst = FALSE,
                               csv_fn = paste0(here::here(), 
                                               "/institutions-investigators/csv/institutions.csv")) {
  if (update_inst) update_inst_csv()
  if (file.exists(csv_fn)) {
    inst_df <- readr::read_csv(csv_fn, show_col_types = FALSE)
    unique(inst_df$inst_id)    
  } else {
    warning(paste("Institutions file not found: ", csv_fn))
    NULL
  }
}

get_ais_from_inst <- function(inst_id=8) {
  message("Getting AIs from institution ", inst_id)
  inst_df <- databraryapi::list_party(inst_id)
  
  if (!is.null(dim(inst_df$children))) {
    ais_df <- as.data.frame(inst_df$children$party)
    ais_df <- dplyr::rename(ais_df, ai_id = id,
                            ai_last = sortname,
                            ai_first = prename)
    
    df <- tibble::tibble(ais_df)
    df <- dplyr::mutate(df, inst_id = inst_df$id,
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

count_affiliates_for_ai <- function(ai_id) {
  affils <- databraryapi::list_affiliates(ai_id)
  if (is.null(affils)) {
    x <- 0
  } else {
    x <- dim(affils)[1]
  }
  x
}

count_affiliates_for_ais <- function(ai_ids) {
 purrr::map_dbl(ai_ids, count_affiliates_for_ai) 
}

# update_invest_csv
#
# Updates the csv/investigators.csv file.
#
# if update_inst is TRUE, updates the institutional database first then uses the updated
# institutions list to generate the authorized investigators data frame.
# if save_new is FALSE, no file is saved.
update_invest_csv <- function(csv_fn = paste0(here::here(), 
                                            "/institutions-investigators/csv/investigators.csv"),
                            save_new = TRUE,
                            update_inst = FALSE,
                            return_df = FALSE) {
  
  message("Updating list of authorizing institutions.")
  inst_ids <- get_auth_inst_list(update_inst = update_inst)
  
  message("Retrieving authorized investigators from institutions.")
  df <- purrr::map_df(inst_ids, get_ais_from_inst)
  
  if (file.exists(csv_fn)) {
    message(paste0("File exists: ", csv_fn))
    if (save_new == TRUE) {
      message(paste0("Overwriting."))
      write_csv(df, csv_fn)
    } else {
      message("No file saved.")
    }
  } else {
    message(paste0("Creating new file: ", csf_fn))
  }
  if (return_df) {
    df    
  }
}

write_invest_csv <- function(min_id = 1, max_id = 25, 
                           csv_fn = paste0(here::here(), 
                                           "/institutions-investigators/csv/investigators.csv"),
                           overwrite = TRUE) {
  if (min_id <= max__id) {
    message("Retrieving data from id ", min_id, ":", max__id)
    df <- purrr::map_df(min_id:max__id, get_inst_info)
    
    if (file.exists(csv_fn)) {
      message(paste0("File exists: ", csv_fn))
      if (overwrite==TRUE) {
        message(paste0("Writing new file: ", csv_fn))
        write_csv(df, csv_fn)
      } else {
        message("Skipping. No file saved.")
        df
      }
    }
  } else {
    message("`min_inst_id` must be less than `max_inst_id")
  }
}

# Map-related functions

lookup_lat_lon <- function(df_row, df, 
                           force_update = FALSE) {
  require(ggmap)
  
  if (!is.numeric(df_row)) {
    stop('`df_row` must be a number.')
  }
  if (df_row < 0) {
    stop('`df_row must be > 0.')
  }
  if (!is.data.frame(df)) {
    stop('`df` must be a data frame.')
  }
  if (df_row > dim(df)[1]) {
    stop('Row index `', df_row, '` is greater than number of rows in `df`.')
  }
  
  this_site <- df[df_row,]
  
  if (is.na(this_site$lat) || force_update) {
    # Skip odd institution names
    if (!(this_site$inst_name == "Staff" || 
          this_site$inst_name == "Muppets University")) {
      ll <- ggmap::geocode(df$inst_name[df_row])
      
      this_site$lat <- ll$lat
      this_site$lon <- ll$lon
    }    
  }
  this_site
}

update_inst_lat_lon <- function(df) {
  if (!is.data.frame(df)) {
    stop('`df` must be a data frame.')
  }
  ll <- ggmap::geocode(as.character(df$inst_name))
  df$lat<- ll$lat
  df$lon<- ll$lon
  
  df
}

update_all_lat_lons <- function(df, force_update = FALSE) {
  row_indices <- 1:dim(df)[1]
  
  purrr::map_df(row_indices, lookup_lat_lon, df, force_update)
}

export_inst_to_json <- function(df) {
  if (!is.data.frame(df)) {
    stop('`df` must be a data frame.')
  }
  require(tidyverse)
  trim_df <- df %>%
    dplyr::filter(., daa == TRUE) %>%
    dplyr::filter(., inst_name != "Staff") %>%
    dplyr::select(., inst_name, lat, lon)
  
  jsonlite::toJSON(trim_df)
}

clean_inst_json_for_leaflet <- function(json, 
                                        write_out = TRUE, 
                                        file_path = file.path(here::here(),
                                                              'institutions-investigators/js')) {
  json <- stringr::str_remove_all(json, '\"inst_name\"\\:')
  json <- stringr::str_remove_all(json, '\"lat\"\\:')
  json <- stringr::str_remove_all(json, '\"lon\"\\:')
  json <- stringr::str_replace_all(json, ',\\{', ',[')
  json <- stringr::str_replace_all(json, '\\},', '],')
  json <- stringr::str_replace_all(json, '\\}\\]', ']}')
  json <- stringr::str_replace_all(json, '\\[\\{', '{[')
  
  if (write_out) {
    if (!dir.exists(file_path)) {
      message('Output directory not found; creating `', file_path, '`.')
      dir.create(file_path)
    }
    writeLines(json, file.path(file_path, 'institutions.json'))
    message(paste0('Saved json to `', file.path(file_path, 'institutions.json'), '`.'))
  } else {
    json
  }
}

create_markers_array_js <- function(json_fn = 'institutions.json', 
                                    write_out = TRUE, 
                                    file_path = file.path(here::here(),
                                                          'institutions-investigators/js'),
                                    js_fn = 'institutions.js') {
  if (!is.character(json_fn)) {
    stop('`json_fn` must be a character string.')
  }
  if (!file.exists(file.path(file_path, json_fn))) {
    stop('Institution file not found: `', file.path(file_path, json_fn), '`.')
  }
  if (!is.logical(write_out)) {
    stop('`write_out` must be a logical value.')
  }
  if (!is.character(file_path)) {
    stop('`file_path` must be a character string.')
  }
  if (!is.character(js_fn)) {
    stop('`js_fn` must be a character string.')
  }
  
  this_con <- file(file.path(file_path, json_fn))
  inst_file <- readLines(this_con)
  inst_file <- stringr::str_replace(inst_file, '\\{\\[', 'var markers = [[')
  inst_file <- stringr::str_replace(inst_file, '\\]\\}', ']];')
  
  if (write_out) {
    if (!dir.exists(file_path)) {
      message('Output directory not found; creating `', file_path, '`.')
      dir.create(file_path)
    }
    writeLines(inst_file, file.path(file_path, js_fn))
    message(paste0('Saved to `', file.path(file_path, js_fn), '`.'))
  } else {
    inst_file
  }
  close(this_con)
}

export_cleaned_inst_json_from_csv <-
  function(csv_fn = file.path(here::here(),
                              'institutions-investigators/csv/institutions.csv')) {
    if (!is.character(csv_fn)) {
      stop(paste0('`csv_fn` must be character.'))
    }
    if (!file.exists(csv_fn)) {
      stop(paste0('File not found: `', csv_fn, '`.'))
    }
    
    df <- readr::read_csv(csv_fn, show_col_types = FALSE)
    message('Exporting json.')
    json <- export_inst_to_json(df)
    message('Cleaning json.')
    clean_inst_json_for_leaflet(json)
    message('Creating `institutions.js` file.')
    create_markers_array_js()
  }

# Rendering Rmarkdown reports

render_institutions_investigators_report <- function(db_login) {
  rmarkdown::render("institutions-investigators/institutions-investigators.Rmd",
                    params = list(db_login = db_login, 
                    max_party_id = 9000))
  clean_up()
}

render_institutions_report <- function(max_party_id = 8150) {
  if (!is.numeric(max_party_id)) {
    stop('`max_party_id` must be numeric.')
  }
  if (max_party_id < 1) {
    stop('`max_party_id` must be >= 1.')
  }
  
  if (!db_credentials_valid()) databraryapi::login_db()
  
  rmarkdown::render("institutions-investigators/institutions.Rmd",
                    params = list(max_party_id = max_party_id))
  clean_up()
}

render_investigators_report <- function(update_invest_csv = TRUE) {
  if (!db_credentials_valid()) databraryapi::login_db()
  
  rmarkdown::render("institutions-investigators/investigators.Rmd",
                    params = list(update_invest_csv = update_invest_csv))
  clean_up()
}

clean_up <- function() {
  #databraryapi::logout_db()
  if (file.exists("institutions-investigators/.databrary.RData")) unlink("institutions-investigators/.databrary.RData")
}

db_credentials_valid <- function() {
  if (file.exists('.databrary.RData')) {
    TRUE
  } else {
    FALSE
  }
}

render_by_institution_report <- function(party_id) {
  if (!db_credentials_valid()) databraryapi::login_db()
  
  this_inst <- get_inst_info(params$party_id)
  
  rmarkdown::render("institutions-investigators/by-institution-report.Rmd")
  
  clean_up()
}

get_volume_data <- function(vol_id = 1, skip_vols = c(1276, 1277)) {
  require(databraryapi)
  require(tidyverse)
  
  if (vol_id %in% skip_vols) return(NULL)
  
  vol_data <- databraryapi::download_containers_records(vol_id)
  if (is.null(vol_data)) {
    message(paste0("No data in volume ", vol_id))
    NULL
  } else {
    message(paste0("Gathering data from volume ", vol_id))
    data.frame(
      vol_id = vol_data$id,
      vol_name = vol_data$name,
      sharing_level = vol_data$publicaccess,
      owner_ids = vol_data$owners,
      sessions_shared = ifelse(
        is.null(vol_data$publicsharefull),
        FALSE,
        vol_data$publicsharefull
      ),
      n_sessions = dim(vol_data$containers)[1] - 1,
      created_date = lubridate::as_datetime(vol_data$creation)
    )
  }
}

get_ai_vols <- function(party_id=6) {
  message("Getting data for party ", party_id)
  ai_df <- databraryapi::list_party(party_id)
  if (!is.null(ai_df)) {
    new_df <- ai_df$access$volume
    # new_df$party_id <- party_id
    # new_df$last_name <- ai_df$sortname
    # new_df$first_name <- ai_df$prename
    # new_df$affiliation <- ai_df$affiliation
    # new_df$person_id <- ai_df$id
    # new_df %>%
    #   dplyr::rename(., vol_id = id, vol_name = name, vol_desc = body, vol_creation = creation, vol_owners = owners,
    #                 ) %>%
    #   dplyr::select(., person_id, last_name, first_name, affiliation, vol_id, vol_name, vol_desc, vol_creation,
    #                 vol_owners)
    new_df
  } else {
   NULL 
  }
}

trim_ai_vols <- function(df) {
  # Extract first owner from data.frame nested in df
  first_owners <- purrr::map_df(lapply(df$owners, '['), function(x) {x[1,]})
  first_owners <- first_owners %>%
    dplyr::rename(., first_owner_name = name,
                  first_owner_id = id)
  
  new_df <- df %>%
    dplyr::select(., id, name, creation, permission) %>%
    dplyr::rename(., vol_id = id, vol_name = name)
  
  cbind(new_df, first_owners)
}

get_clean_ai_vols <- function(party_id=6) {
  if (is.na(party_id)) {
    message("Invalid party_id `", party_id, '`.')
    return(NULL)
  } else {
    message("Getting volume data for party `", party_id, '`.')
    
    ai_df <- databraryapi::list_party(party_id)
    if (is.null(dim(ai_df$access))) {
      message(" No volumes found for party `", party_id, '`.')
      return(NULL)
    } else {
      raw_vols_df <- trim_ai_vols(ai_df$access$volume)
      n_vols <- dim(raw_vols_df)[1]
      
      df <- tibble(person_id = rep(ai_df$id, n_vols),
                   last_name = rep(ai_df$sortname, n_vols),
                   first_name = rep(ai_df$prename, n_vols),
                   affiliation = rep(ai_df$affiliation, n_vols),
                   vol_id = raw_vols_df$vol_id,
                   vol_name = raw_vols_df$vol_name,
                   vol_creation = raw_vols_df$creation,
                   vol_permission = raw_vols_df$permission,
                   vol_1st_owner_name = raw_vols_df$first_owner_name,
                   vol_1st_owner_id = raw_vols_df$first_owner_id)
      df
    }
  }
}

#---------------------------------------------------------------------------------------
# Saves a CSV of the data frame providing data about an Authorized Investigator's volumes
save_ai_vols_csv <- function(df, csv_path = "csv") {
  fn <- file.path(csv_path, paste0(stringi::stri_pad(df$person_id[1], 4, pad="0"), "-",
                                   tolower(df$last_name[1]), "-", 
                                   tolower(df$first_name[1]), "-vols-", 
                                           Sys.Date(), ".csv"))
  message(" Saving file: `", fn, '`.')
  readr::write_csv(df, fn)
}

#---------------------------------------------------------------------------------------
# Creates, cleans, and saves as a CSV a data frame providing data about an Authorized 
# Investigator's volumes
get_clean_save_ai_vols_csv <- function(party_id, csv_path = "csv") {
  ai_vols <- get_clean_ai_vols(party_id)
  if(!is.null(ai_vols)) {
    save_ai_vols_csv(ai_vols, csv_path)    
  }
}

#---------------------------------------------------------------------------------------
# Generates data frames and CSVs for all of an institution's investigators
get_clean_save_inst_ais_csvs <- function(inst_id = 8, csv_path = "csv") {
  these_ais <- make_institutional_party_df(inst_id)
  
  if (is.null(these_ais)) {
    warning("Unable to retrieve investigator data for institution ", inst_id, '.')
    return(NULL)
  }
  purrr::map(these_ais$party_id, get_clean_save_ai_vols_csv, csv_path)
}
  
#---------------------------------------------------------------------------------------
# Generates a data frame containing summary statistics about a volume's assets
calculate_vol_asset_stats <- function(vol_id) {
  if ((as.numeric(vol_id) <= 0)) {
    stop('`vol_id` must be > 0.')
  }

  require(tidyverse)
  
  options(dplyr.summarise.inform = FALSE)
  
  message(paste0('Retrieving asset data for volume ', vol_id))
  vol_assets <- get_assets_in_vol(vol_id)
  if (is.null(vol_assets)) {
    message(paste0(" No shared data in volume ", vol_id))
    NULL
  } else {
    
    vol_summary <- vol_assets %>%
      # dplyr::mutate(., vol_id = vol_id, mimetype = mimetype, extension = extension) %>%
      dplyr::group_by(., vol_id, mimetype, extension, asset_type) %>%
      dplyr::summarise(., n_files = n(),
                       tot_size = sum(size, na.rm = TRUE),
                       tot_dur_hrs = ms_to_hrs(sum(duration, na.rm = TRUE)))
    
    # if(!('duration' %in% names(vol_summary))) {
    #   vol_summary <- dplyr::mutate(vol_summary, duration = NA)
    # }
    vol_summary
  }
}

#---------------------------------------------------------------------------------------
# Generates a data frame containing information about a volume's assets
get_assets_in_vol <- function(vol_id, vb = FALSE) {
  if ((as.numeric(vol_id) <= 0)) {
    stop('`vol_id` must be > 0.')
  }
  if (!is.logical(vb)) {
    stop('`vb` must be a logical value.')
  }
  
  require(tidyverse)
  require(databraryapi)
  
  message(paste0(" Extracting assets from volume ", vol_id))
  vol_data <- databraryapi::list_assets_in_volume(vol_id)
  
  if (is.null(vol_data)) {
    message("   No available assets.")
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

#---------------------------------------------------------------------------------------
# Generates a data frame containing information about an authorized investigator's
# volumes and assets
get_ai_vols_assets <- function(party_id) {
  if (!is.numeric(party_id)) {
    warning('`party_id` must be numeric. Converting.')
    party_id <- as.numeric(party_id)
  }
  if (party_id < 1) {
    warning('`party_id` must be > 0')
    return(NULL)
  }
  
  require(dplyr)
  
  ais_vols <- get_clean_ai_vols(party_id)
  if (is.null(ais_vols)) {
    warning("No volumes found for party `", party_id, '`.')
    return(NULL)
  }
  
  ais_vols_stats <- purrr::map_df(ais_vols$vol_id, get_assets_in_vol)
  if (is.null(ais_vols_stats)) {
    warning('Unable to retrieve volume-level data for party `', party_id, '`.')
    return(NULL)
  }
  
  dplyr::left_join(ais_vols, ais_vols_stats, by='vol_id')
}

#---------------------------------------------------------------------------------------
# Open a CSV with volume data, generate asset-level statistics, return an augmented
# data frame
merge_asset_stats_for_ai_vols <- function(vol_csv_fn) {
  this_vol_df <- readr::read_csv(vol_csv_fn, show_col_types = FALSE)
  asset_stats_df <- purrr::map_df(this_vol_df$vol_id, calculate_vol_asset_stats)
  if (dim(asset_stats_df)[1] == 0) {
    warning("No asset data available for volumes in `", vol_csv_fn, '`.')
    return(NULL)
  } else {
    df <- dplyr::left_join(this_vol_df, asset_stats_df, by = 'vol_id')
    df %>%
      dplyr::arrange(., vol_id, asset_type)    
  }
}

#---------------------------------------------------------------------------------------
# Export CSV with volume and asset-level statistics for a given researcher
export_vol_asset_csv <- function(vol_csv_fn, csv_path = "csv") {
  df <- merge_asset_stats_for_ai_vols(vol_csv_fn)
  if (is.null(df)) {
    warning("  No asset data to be merged for `", vol_csv_fn, '`.')
    return(NULL)
  } else {
    out_fn <- file.path(csv_path, paste0(stringi::stri_pad(df$person_id[1], 4, pad="0"), "-",
                                         tolower(df$last_name[1]), "-", 
                                         tolower(df$first_name[1]), "-assets-", 
                                         Sys.Date(), ".csv"))
    
    message("  Saving file: `", out_fn, '`.')
    readr::write_csv(df, out_fn)    
  }
}


get_unique_ai_assets <- function(ai_index, inst_df, csv_dir = "institutions-investigators/csv") {
  this_party_id <- inst_df$party_id[ai_index]
  this_party_sortname <- inst_df$sortname[ai_index]
  this_party_prename <- inst_df$prename[ai_index]
  
  this_search_str <- paste0(tolower(this_party_sortname), "\\-", tolower(this_party_prename), "\\-vols")
  this_vol_fns <- list.files(csv_dir, this_search_str,
                             full.names = TRUE)
  
  if (length(this_vol_fns) > 1) {
    this_vol_fn <- this_vol_fns[length(this_vol_fns)]
  } else {
    message("  No unique volume-level CSV found for party `", this_party_id, '`.')
    return(NULL)
  }
  message("Computing asset-by-volume statistics for party `", this_party_id, '`.')
  merge_asset_stats_for_ai_vols(this_vol_fn)
}

#---------------------------------------------------------------------------------------

bytes_to_gb <- function(b) {
  b/(1.024e9)
}

bytes_to_mb <- function(m) {
  m/(1.024e6)
}

ms_to_secs <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms/1000
}

secs_to_mins <- function(s) {
  if (!is.numeric(s)) {
    stop('`s` must be a number.')
  }
  s/60
}

mins_to_hrs <- function(m) {
  if (!is.numeric(m)) {
    stop('`m` must be a number.')
  }  
  m/60
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
