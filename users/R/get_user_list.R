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
  est_max_party_id <- (curr_stats$investigators + curr_stats$affiliates) + inflate_id_n
  
  if (gather_all) {
    if (vb) message(paste0("Gathering data from n=", est_max_party_id, " party IDs"))
    people <- databraryapi::list_people(1:est_max_party_id)
  } else {
    if (vb) message(paste0("Gathering data from n=", length(user_ids), " party IDs."))
    people <- databraryapi::list_people(user_ids)
  }
  # Drop NAs
  dplyr::filter(people, is.na(sortname))
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