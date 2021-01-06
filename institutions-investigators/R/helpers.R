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

# Rendering Rmarkdown reports

render_institutions_investigators_report <- function(db_login) {
  rmarkdown::render("institutions-investigators/institutions-investigators.Rmd",
                    params = list(db_login = db_login, 
                    max_party_id = 9000))
  clean_up()
}

render_institutions_report <- function(db_login, max_party_id = 9000) {
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