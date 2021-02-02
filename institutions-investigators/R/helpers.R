# make_institutional_party_df
# 
# Creates a data frame of an institution that has authorized investigators
make_institutional_party_df <- function(inst_id) {
  if (databraryapi::is_institution(inst_id)) {
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
                          inst_id, affiliation, inst_url, party_id, sortname, prename, party_url,
                          url, email)
    } 
    df
  } else {
    NULL
  }
}

get_inst_info <- function(inst_id = 8) {
  require(databraryapi)
  if (inst_id > 0) {
    if (databraryapi::is_institution(inst_id)) {
      inst_df <- list_party(inst_id)
      df <- data.frame(inst_id = inst_df$id,
                       inst_name = inst_df$sortname,
                       inst_url = ifelse('url' %in% names(inst_df), inst_df$url, NA))
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
      df <- update_inst_lat_lon(df)
      
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
  if (min_inst_id <= max_inst_id) {
    message("Retrieving data from inst_id ", min_inst_id, ":", max_inst_id)
    df <- purrr::map_df(min_inst_id:max_inst_id, get_inst_info)
    message(paste0("Writing new file: ", csv_fn))
    write_csv(df, csv_fn)
  } else {
    message("`min_inst_id` must be less than `max_inst_id")
  }
}

get_max_institutional_id <- function(csv_fn = paste0(here::here(), 
                                                     "/institutions-investigators/csv/institutions-investigators.csv")) {
  if (file.exists(csv_fn)) {
    message("Reading from saved file.")
    inst_invest <- read_csv(csv_fn)
    unique(max(inst_invest$inst_id))
  } else {
    message(paste0("File not found: ", csv_fn))
  }
}

update_inst_csv <- function(csv_fn = paste0(here::here(), 
                                                   "/institutions-investigators/csv/institutions.csv"),
                                   max_id = 9000,
                                   save_new = TRUE) {

  if (file.exists(csv_fn)) {
    message("Reading from saved file.")
    old_inst <- read_csv(csv_fn)
    max_old_inst_id <- unique(max(old_inst$inst_id))
    if (max_old_inst_id < max_id) {
      message("Retrieving data from `party_id` ", max_old_inst_id + 1, ":", max_id)
      new_inst <- purrr::map_df((max_old_inst_id + 1):max_id, get_inst_info)
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
    df <- get_inst_info(1, max_id)
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
    inst_df <- read_csv(csv_fn)
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
    inst_df <- read_csv(csv_fn)
    unique(inst_df$inst_id)    
  } else {
    warning(paste("Institutions file not found: ", csv_fn))
    NULL
  }
}

get_ais_from_inst <- function(inst_id=8) {
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
# institutions list to generate the authorize investigators data frame.
# if save_new is FALSE, no file is saved.
update_invest_csv <- function(csv_fn = paste0(here::here(), 
                                            "/institutions-investigators/csv/investigators.csv"),
                            save_new = TRUE,
                            update_inst = FALSE,
                            return_df = FALSE) {
  
  inst_ids <- get_auth_inst_list(update_inst = update_inst)
  
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

export_cleaned_inst_json_from_csv <- function(csv_fn = 'institutions-investigators/csv/institutions.csv') {
  
  if (!is.character(csv_fn)) {
    stop(paste0('`csv_fn` must be character.'))
  }
  if (!file.exists(csv_fn)) {
    stop(paste0('File not found: `', csv_fn, '`.'))
  }
  
  df <- readr::read_csv(csv_fn)
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

render_institutions_report <- function(db_login, max_party_id = 9000) {
  if (!is.character(db_login)) {
    stop('`db_login` must be a character string.')
  }
  if (!is.numeric(max_party_id)) {
    stop('`max_party_id` must be numeric.')
  }
  if (max_party_id < 1) {
    stop('`max_party_id` must be >= 1.')
  }
  
  rmarkdown::render("institutions-investigators/institutions.Rmd",
                    params = list(db_login = db_login, 
                                  max_party_id = max_party_id))
  clean_up()
}

render_investigators_report <- function(db_login, update_invest_csv = 'True') {
  rmarkdown::render("institutions-investigators/investigators.Rmd",
                    params = list(db_login = db_login, 
                                  update_invest_csv = update_invest_csv))
  clean_up()
}

clean_up <- function() {
  databraryapi::logout_db()
  if (file.exists("institutions-investigators/.databrary.RData")) unlink("institutions-investigators/.databrary.RData")
}