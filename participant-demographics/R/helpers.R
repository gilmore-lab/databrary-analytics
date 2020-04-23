# helper functions for participant-demog-report

# Temporary until we update the databraryapi package
download_session_csv <- function(vol_id = 1, to_df = TRUE,
                                 return_response = FALSE, vb = FALSE) {
  
  # Error handling
  if (length(vol_id) > 1) {
    stop("vol_id must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("vol_id must be an integer > 0.")
  }
  
  if (vb) message(paste0("Downloading spreadsheet from volume ", vol_id))
  r <- httr::content(httr::GET(paste0("https://nyu.databrary.org/volume/",
                                      vol_id, "/csv")), 'text', encoding='UTF-8')
  
  if (is.null(r) | !stringr::str_detect(r, "session-id")) {
    if (vb) message(paste0("No CSV data returned from volume ", vol_id))
    NULL
  } else if (to_df == TRUE) {
    if (vb) message(paste0("Converting response to data frame."))
    r_df <- read.csv(text = r, stringsAsFactors = FALSE)
    if (class(r_df)=="data.frame") {
      if (vb) message(paste0("Imported data frame. Cleaning up."))
      r_df <- dplyr::mutate(r_df, vol_id = vol_id)
      r_df <- dplyr::rename(r_df,
                            session_id = session.id,
                            session_name = session.name,
                            session_date = session.date,
                            session_release = session.release)
      r_df
    } else {
      if (vb) message("Can't coerce to data frame. Skipping.\n")
      NULL
    }
  } else {
    if (vb) message(paste0("Returning raw data from volume ", vol_id))
    r
  }
}

get_volume_demog <- function(vol_id) {
  v_ss <- download_session_csv(vol_id)
  if ("participant.birthdate" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, participant.birthdate, participant.race, 
                  participant.ethnicity, participant.gender, participant.birthdate)
  } else {
    NULL
  }
}

get_volume_birthdate <- function(vol_id) {
  v_ss <- download_session_csv(vol_id)
  if ("participant.birthdate" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant.birthdate)
  } else {
    data.frame(vol_id = vol_id, session_id = NA,  participant.birthdate = NA)
  }
}

get_volume_gender <- function(vol_id) {
  v_ss <- download_session_csv(vol_id)
  if ("participant.gender" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant.gender)
  } else {
    data.frame(vol_id = vol_id, session_id = NA,  participant.gender = NA)
  }
}

get_volume_race <- function(vol_id) {
  v_ss <- download_session_csv(vol_id)
  if ("participant.race" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant.race)
  } else {
    data.frame(vol_id = vol_id, session_id = NA, participant.race = NA)
  }
}

get_volume_ethnicity <- function(vol_id) {
  v_ss <- download_session_csv(vol_id)
  if ("participant.race" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant.ethnicity)
  } else {
    data.frame(vol_id = vol_id, session_id = NA, participant.ethnicity = NA)
  }
}

get_volumes_demo <- function(min_vol_id = 1, max_vol_id = 10) {
  vols_range <- min_vol_id:max_vol_id
  message(paste0("Getting demographic data for volumes ", min_vol_id, "-", max_vol_id, "\n"))
  message("...Gathering birthdates")
  bdts <- purrr::map_dfr(vols_range, get_volume_birthdate)
  message("...Gathering race")
  races <- purrr::map_dfr(vols_range, get_volume_race)
  message("...Gathering ethnicity")
  ethn <- purrr::map_dfr(vols_range, get_volume_ethnicity)
  message("...Gathering gender")
  gend <- purrr::map_dfr(vols_range, get_volume_gender)
  
  m <- dplyr::left_join(bdts, races, by = c("vol_id", "session_id"))
  m <- dplyr::left_join(m, ethn, by = c("vol_id", "session_id"))
  m <- dplyr::left_join(m, gend, by = c("vol_id", "session_id"))
  m
}

save_volumes_demo <- function(df, min_vol_id, max_vol_id, 
                              dir = "participant-demographics/csv") {
  message(paste0("Saving demographic data for volumes ", min_vol_id, "-", max_vol_id, "\n"))
  fn <- paste0(dir, "/", stringr::str_pad(min_vol_id, 4, pad = "0"), "-", 
               stringr::str_pad(max_vol_id, 4, pad = "0"), "-demog.csv")
  fn
  readr::write_csv(df, fn)
}

get_save_volumes_demo <- function(min_vol_id = 1, max_vol_id = 10) {
  df <- get_volumes_demo(min_vol_id, max_vol_id)
  save_volumes_demo(df, min_vol_id, max_vol_id)
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
  purrr::map_dfr(fl, readr::read_csv)
}

get_volumes_owners <- function(min_vol_id = 1, max_vol_id = 10) {
  vols_range <- min_vol_id:max_vol_id
  message(".Gathering volume owners")
  purrr::map_dfr(.x = vols_range, .f = databraryapi::list_volume_owners)
}

get_volume_first_owner <- function(vol_id) {
  df <- databraryapi::list_volume_owners(vol_id)
  if (purrr::is_empty(df)) {
    NULL
  } else {
    df[1,]
  }
}

get_volumes_first_owners <- function(min_vol_id = 1, max_vol_id = 10) {
  vols_range <- min_vol_id:max_vol_id
  message(".Gathering volume first owners")
  purrr::map_dfr(.x = vols_range, .f = get_volume_first_owner)
}

save_volumes_owners <- function(df, min_vol_id, max_vol_id, 
                                dir = "participant-demographics/csv",
                                fn_suffix = "-owners.csv") {
  fn <- paste0(dir, "/", stringr::str_pad(min_vol_id, 4, pad = "0"), "-", 
               stringr::str_pad(max_vol_id, 4, pad = "0"), fn_suffix)
  message(paste0(".Saving volume owner data to ", fn, "."))
  readr::write_csv(df, fn)
}

get_save_volumes_owners <- function(min_vol_id = 1, max_vol_id = 10, 
                                    dir = "participant-demographics/csv") {
  message(paste0("Getting owner data for volumes ", min_vol_id, "-", max_vol_id))
  df <- get_volumes_owners(min_vol_id, max_vol_id)
  message(paste0("Saving owner data for volumes ", min_vol_id, "-", max_vol_id))
  save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-owners.csv")
}

get_save_volumes_first_owners <- function(min_vol_id = 1, max_vol_id = 10,
                                          dir = "participant-demographics/csv") {
  message(paste0("Getting first owner data for volumes ", min_vol_id, "-", max_vol_id))
  df <- get_volumes_first_owners(min_vol_id, max_vol_id)
  message(paste0("Saving first owner data for volumes ", min_vol_id, "-", max_vol_id))
  save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-first-owners.csv")
}

load_owner_csvs <- function(dir = "participant-demographics/csv", 
                            fn_suffix = "-owners") {
  if (!is.character(dir)) {
    stop("'dir' must be a string")
  }
  if (!dir.exists(dir)) {
    stop("'dir' does not exist.")
  }
  
  fl <- list.files(dir, paste0(fn_suffix, "\\.csv$"), full.names = TRUE)
  if (is.null(fl)) {
    stop(paste0("No csv files in ", dir, "."))
  }
  purrr::map_dfr(fl, readr::read_csv)
}

render_participant_demog_report <- function(db_login) {
  rmarkdown::render("participant-demographics/participant-demog-report.Rmd",
                    params = list(db_login = db_login))
  databraryapi::logout_db()
  if(file.exists("participant-demographics/.databrary.RData")) {
    file.remove("participant-demographics/.databrary.RData")
  }
}