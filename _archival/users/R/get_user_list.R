get_user_email_list <- function(user_ids,
                                inflate_id_n = 25,
                                gather_all = FALSE,
                                vb = FALSE) {
  if (!is.numeric(user_ids)) {
    stop("'user_ids' must be numeric.")
  }
  if (!is.logical(gather_all)) {
    stop("'gather_all' must be a logical value")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be a logical value")
  }
  
  # Calculate maximum party_id value from databraryapi::get_db_stats()
  curr_stats <- databraryapi::get_db_stats()
  est_max_party_id <- (curr_stats$investigators + curr_stats$affiliates + curr_stats$institutions) + inflate_id_n
  
  if (gather_all) {
    if (vb) message(paste0("Gathering data from n=", est_max_party_id, " party IDs"))
    people <- databraryapi::list_people(1:est_max_party_id)
  } else {
    if (vb) message(paste0("Gathering data from n=", length(user_ids), " party IDs."))
    people <- databraryapi::list_people(user_ids)
  }
  # Drop NAs
  dplyr::filter(people, !is.na(sortname))
}

get_save_user_email_list <- function(user_ids = 6, fn = "users/csv/user_emails.csv",
                                     inflate_id_n = 50,
                                     gather_all = FALSE,
                                     vb = FALSE) {
  
  if (!is.numeric(user_ids)) {
    stop("'user_ids' must be numeric.")
  }
  if (!is.character(fn)) {
    stop("'fn' must be a string.")
  }
  if (!is.logical(gather_all)) {
    stop("'gather_all' must be a logical value")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be a logical value")
  }
  
  people <- get_user_email_list(user_ids, inflate_id_n, gather_all, vb)
  
  if (!purrr::is_empty(people)) {
    if (file.exists(fn)) {
      message(paste0("File '", fn, "' exists. Overwriting."))
      readr::write_csv(people, path = fn)
    } else {
      message(paste0("Creating new '", fn, "'."))
      readr::write_csv(people, path = fn)
    }
  } else {
    message("No data returned. No file saved.")
    NULL
  }
}

get_user_data <- function(user_id) {
  if (!is.numeric(user_id)) {
    stop("'user_id' must be numeric.")
  }
  if (user_id < 1) {
    stop("'user_id' must be > 1.")
  }
  
  url <- paste0("https://nyu.databrary.org/api/party/", user_id, "?parents")
  r <- httr::GET(url)
  if (r$status_code == 200) {
    if (r$headers$`content-type` == 'application/json') {
      this_content <- httr::content(r, 'text', encoding = 'UTF-8')
      u <- jsonlite::fromJSON(this_content)
      data.frame(user_id = u$id,
                 last_name = u$sortname,
                 first_name = u$prename,
                 affiliation = u$affiliation,
                 url = u$url,
                 email = u$email,
                 permission = u$permission)
    }
  }
}

get_user_sponsors <- function(user_id, inst_only = TRUE) {
  if (!is.numeric(user_id)) {
    stop("'user_id' must be numeric.")
  }
  if (user_id < 1) {
    stop("'user_id' must be > 1.")
  }
  
  if (databraryapi::is_person(user_id)) {
    url <- paste0("https://nyu.databrary.org/api/party/", user_id, "?parents")
    r <- httr::GET(url)
    if (r$status_code == 200) {
      if (r$headers$`content-type` == 'application/json') {
        this_content <- httr::content(r, 'text', encoding = 'UTF-8')
        u <- jsonlite::fromJSON(this_content)
        if (!is.null(u)) {
          df <- data.frame(u$parents$party)
          df <- dplyr::mutate(df, user_id = u$id,
                              user_last = u$sortname,
                              user_first = u$prename)
          if ("id" %in% names(df)) {
            df <- dplyr::rename(df,
                                sp_id = id,
                                sp_last = sortname,
                                sp_is_institution = institution,
                                sp_url = url)
            df <- dplyr::select(df, user_id, user_last, user_first,
                                sp_id, sp_last, sp_is_institution, sp_url)
            if (inst_only) {
              dplyr::filter(df, sp_is_institution == TRUE)
            } else {
              df
            }          
          }
        }
      }
    }
  }
}

get_user_parents <- function(user_id, inst_only = FALSE) {
  if (!is.numeric(user_id)) {
    stop("'user_id' must be numeric.")
  }
  if (user_id < 1) {
    stop("'user_id' must be > 1.")
  }
  
  if (databraryapi::is_person(user_id)) {
    url <- paste0("https://nyu.databrary.org/api/party/", user_id, "?parents")
    r <- httr::GET(url)
    if (r$status_code == 200) {
      if (r$headers$`content-type` == 'application/json') {
        this_content <- httr::content(r, 'text', encoding = 'UTF-8')
        u <- jsonlite::fromJSON(this_content)
        if (!is.null(u)) {
          df <- data.frame(u$parents$party)
          if (dim(df)[1] > 0) {
            df <- dplyr::mutate(df, user_id = u$id,
                                user_last = u$sortname,
                                user_first = u$prename)
            df <- dplyr::rename(df,
                                sponsor_id = id,
                                sponsor_last = sortname)
            if ("prename" %in% names(df)) {
              df <- dplyr::rename(df, sponsor_first = prename)
            } else {
              df$sponsor_first = NA
            }
            if ("institution" %in% names(df)) {
              df <- dplyr::mutate(df, sponsor_is_institution = !is.na(institution))
            } else {
              df$sponsor_is_institution <- FALSE
            }
            df <- dplyr::select(df, user_id, user_last, user_first,
                                sponsor_id, sponsor_last, sponsor_first, sponsor_is_institution)
            if (inst_only) {
              dplyr::filter(df, sponsor_is_institution == TRUE)
            } else {
              df
            }
          }
        }
      }
    }
  }
}


get_save_user_sponsors_list <- function(user_ids = 6, 
                                        fn = "users/csv/user_sponsors.csv",
                                        inflate_id_n = 50,
                                        gather_all = FALSE,
                                        inst_only = FALSE,
                                        vb = FALSE) {
  
  if (!is.numeric(user_ids)) {
    stop("'user_ids' must be numeric.")
  }
  if (!is.character(fn)) {
    stop("'fn' must be a string.")
  }
  if (!is.numeric(inflate_id_n)) {
    stop("'inflate_id_n' must be numeric.")
  }
  if (!is.logical(gather_all)) {
    stop("'gather_all' must be a logical value")
  }
  if (!is.logical(inst_only)) {
    stop("'inst_only' must be a logical value")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be a logical value")
  }
  
  # Calculate maximum party_id value from databraryapi::get_db_stats()
  curr_stats <- databraryapi::get_db_stats()
  est_max_party_id <- (curr_stats$investigators + curr_stats$affiliates + curr_stats$institutions) + inflate_id_n
  
  if (gather_all) {
    if (vb) message(paste0("Gathering data from n=", est_max_party_id, " party IDs"))
    #people <- databraryapi::list_people(1:est_max_party_id)
    people <- purrr::map_df(1:est_max_party_id, get_user_parents, inst_only)
  } else {
    if (vb) message(paste0("Gathering data from n=", length(user_ids), " party IDs."))
    people <- purrr::map_df(user_ids, get_user_parents, inst_only)
  }
  
  if (!purrr::is_empty(people)) {
    if (file.exists(fn)) {
      message(paste0("File '", fn, "' exists. Overwriting."))
      readr::write_csv(people, path = fn)
    } else {
      message(paste0("Creating new '", fn, "'."))
      readr::write_csv(people, path = fn)
    }
  } else {
    message("No data returned. No file saved.")
    NULL
  }
}
