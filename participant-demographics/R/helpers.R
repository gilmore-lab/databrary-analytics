# helper functions for participant-demog-report

# Temporary until we update the databraryapi package
download_session_csv_ <- function(vol_id = 1,
                                  to_df = TRUE,
                                  return_response = FALSE,
                                  vb = FALSE) {
  # Error handling
  if (length(vol_id) > 1) {
    stop("vol_id must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("vol_id must be an integer > 0.")
  }
  
  if (vb)
    message(paste0("Downloading spreadsheet from volume ", vol_id))
  
  url <-  paste0("https://nyu.databrary.org/volume/",
                 as.character(vol_id),
                 "/csv")
  # r <-
  #   httr::content(httr::GET(url)
  #   ), 'text', encoding = 'UTF-8')
  
  # Use curl library instead of httr and try() to handle 404 errors
  r <- NULL
  try(r <- read_lines(curl::curl(url)), silent = TRUE)
  
  if (is.null(r)) {
    if (vb)
      message(paste0("No CSV data returned from volume ", vol_id))
    NULL
  } else if (!stringr::str_detect(r[1], 'session[\\-\\.]id')) {
    if (vb)
      message(paste0("No CSV data returned from volume ", vol_id))
    NULL
  } else if (to_df == TRUE) {
    if (vb)
      message(paste0("Converting response to data frame."))
    r_df <- read.csv(text = r, stringsAsFactors = FALSE)
    if (class(r_df) == "data.frame") {
      if (vb)
        message(paste0("Imported data frame. Cleaning up."))
      r_df <- dplyr::mutate(r_df, vol_id = vol_id)
      r_df <- dplyr::rename(
        r_df,
        session_id = session.id,
        session_name = session.name,
        session_date = session.date,
        session_release = session.release
      )
      r_df
    } else {
      if (vb)
        message("Can't coerce to data frame. Skipping.\n")
      NULL
    }
  } else {
    if (vb)
      message(paste0("Returning raw data from volume ", vol_id))
    r
  }
}

get_volume_demog <- function(vol_id, vb = FALSE) {
  message(paste0("....Gathering demog data from volume ", vol_id))
  v_ss <-
    try(databraryapi::download_session_csv(vol_id), silent = TRUE)
  
  if ('try-error' %in% class(v_ss)) {
    message(".....Error loading CSV for volume ", vol_id)
    df <- tibble(vol_id = NA,
                 age_days = NA,
                 participant_gender = NA,
                 participant_race = NA,
                 participant_ethnicity = NA)
    return(df)
  } else if (is.null(v_ss)) {
    message(".....NULL CSV for volume ", vol_id)
    df <- tibble(vol_id = NA,
                 age_days = NA,
                 participant_gender = NA,
                 participant_race = NA,
                 participant_ethnicity = NA)
    return(df)
  }

  df <- v_ss  
  df <- dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
  
  df$vol_id <- vol_id
  
  # Given that some spreadsheets contain multiple participants, we might have to handle cleaning separately from exporting.
  
  # has_participant_birthdate <- stringr::str_detect(names(v_ss), "participant_birthdate")
  # has_session_date <- stringr::str_detect(names(v_ss), "session_date")
  # 
  # if (has_participant_birthdate & has_session_date) {
  #   df <-
  #     dplyr::mutate(df,
  #                   age_days = as.character(as.Date(session_date) - as.Date(participant_birthdate)))
  #   df <-
  #     dplyr::select(df,-c(has_participant_birthdate, has_session_date))
  # } else {
  #   df$age_days = NA
  # }
  # 
  # if (!("participant_race" %in% names(df))) {
  #   df$participant_race = NA
  # }
  # 
  # if (!("participant_ethnicity" %in% names(df))) {
  #   df$participant_ethnicity = NA
  # }
  # 
  # if (!("participant_gender" %in% names(df))) {
  #   df$participant_gender = NA
  # }
  # 
  # df <-
  #   dplyr::select(df,
  #                 vol_id,
  #                 age_days,
  #                 participant_gender,
  #                 participant_race,
  #                 participant_ethnicity)
  
  df
}

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

get_volumes_demo <- function(min_vol_id = 1,
                             max_vol_id = 10,
                             vb = FALSE) {
  vols_range <- min_vol_id:max_vol_id
  
  message(paste0(
    "Getting demographic data for volumes ",
    min_vol_id,
    "-",
    max_vol_id,
    "\n"
  ))
  
  
  # message("...Gathering session dates")
  # bdts <- purrr::map_dfr(vols_range, get_volume_session_date)
  #
  # message("...Gathering birthdates")
  # bdts <- purrr::map_dfr(vols_range, get_volume_birthdate, vb)
  #
  # message("\n...Gathering race")
  # races <- purrr::map_dfr(vols_range, get_volume_race, vb)
  #
  # message("\n...Gathering ethnicity")
  # ethn <- purrr::map_dfr(vols_range, get_volume_ethnicity, vb)
  #
  # message("\n...Gathering gender")
  # gend <- purrr::map_dfr(vols_range, get_volume_gender, vb)
  # m <- dplyr::left_join(bdts, races, by = c("vol_id", "session_id"))
  # m <- dplyr::left_join(m, ethn, by = c("vol_id", "session_id"))
  # m <- dplyr::left_join(m, gend, by = c("vol_id", "session_id"))
  
  m <- purrr::map_df(vols_range, get_volume_demog, vb)
  
  m
}

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

get_save_volumes_demo <- function(min_vol_id = 1,
                                  max_vol_id = 10,
                                  dir = "participant-demographics/csv",
                                  vb = FALSE) {
  df <- get_volumes_demo(min_vol_id, max_vol_id, vb)
  save_volumes_demo(df, min_vol_id, max_vol_id, dir)
}

# Downloads new demographic data from Databrary and saves it as a new CSV
#
# For efficiency reasons, the code takes 10 volumes at a time and saves them
# in individual CSVs labeled with the volume numbers.
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
                               max_vol_id = 10) {
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

get_volumes_first_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10) {
    vols_range <- min_vol_id:max_vol_id
    message(".Gathering volume first owners")
    purrr::map_dfr(.x = vols_range, .f = get_volume_first_owner)
  }

save_volumes_owners <- function(df,
                                min_vol_id,
                                max_vol_id,
                                dir = "participant-demographics/csv",
                                fn_suffix = "-owners.csv") {
  fn <-
    paste0(
      dir,
      "/",
      stringr::str_pad(min_vol_id, 4, pad = "0"),
      "-",
      stringr::str_pad(max_vol_id, 4, pad = "0"),
      fn_suffix
    )
  message(paste0(".Saving volume owner data to ", fn, "."))
  readr::write_csv(df, fn)
}

get_save_volumes_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           dir = "participant-demographics/csv") {
    message(paste0("Getting owner data for volumes ", min_vol_id, "-", max_vol_id))
    df <- get_volumes_owners(min_vol_id, max_vol_id)
    message(paste0("Saving owner data for volumes ", min_vol_id, "-", max_vol_id))
    save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-owners.csv")
  }

get_save_volumes_first_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           dir = "participant-demographics/csv") {
    message(paste0(
      "Getting first owner data for volumes ",
      min_vol_id,
      "-",
      max_vol_id
    ))
    df <- get_volumes_first_owners(min_vol_id, max_vol_id)
    message(paste0(
      "Saving first owner data for volumes ",
      min_vol_id,
      "-",
      max_vol_id
    ))
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

get_volume_ss <- function(vol_id = 1, 
                          omit_materials = FALSE,
                          vb = FALSE) {
  message(paste0("Gathering spreadsheet data from volume ", vol_id))
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
      df <- dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
    }
    df$vol_id <- vol_id
    return(df)    
  }
}

get_save_volume_ss <- function(vol_id = 1,
                               dir = "participant-demographics/csv",
                               omit_materials = FALSE,
                               vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.character(dir))
  stopifnot(is.logical(omit_materials))
  stopifnot(is.logical(vb))
  
  df <- get_volume_ss(vol_id, omit_materials, vb)
  
  if (!is.null(df)) {
    if (omit_materials) {
      fn <-
        paste0(
          dir,
          "/",
          stringr::str_pad(vol_id, 4, pad = "0"),
          "-sess.csv"
        )
    } else {
      fn <-
        paste0(
          dir,
          "/",
          stringr::str_pad(vol_id, 4, pad = "0"),
          "-sess-materials.csv"
        )
    }
    
    readr::write_csv(df, fn)
    message(".Saved ", fn)
    
  } else {
    message(".No data available for volume: ", vol_id)
  }
}

# Returns a tibble
extract_sessions_from_vol_csv <- function(csv_fn = "participant-demographics/csv/0002-sess-materials.csv") {
  stopifnot(is.character(csv_fn))
  stopifnot(file.exists(csv_fn))
  
  #df <- readr::read_csv(csv_fn, show_col_types = FALSE)
  df <- read.csv(csv_fn, colClasses = "character")
  
  if (dim(df)[1] == 0) {
    message("CSV empty: ", csv_fn)
    NULL
  } else {
    dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
  }
}

# Returns a tibble 
count_sessions_materials_folders <- function(csv_fn = "participant-demographics/csv/0002-sess-materials.csv") {
  stopifnot(is.character(csv_fn))
  stopifnot(file.exists(csv_fn))
  
  df <- readr::read_csv(csv_fn, show_col_types = FALSE)
  
  if (dim(df)[1] == 0) {
    message("CSV empty: ", csv_fn)
    NULL
  } else {
    df_sess <- dplyr::filter(df, !(stringr::str_detect(session_date, 'materials')))
    n_sess <- dim(df_sess)[1]
    
    df_materials <- dplyr::filter(df, (stringr::str_detect(session_date, 'materials')))
    n_materials <- dim(df_materials)[1]
    
    vol_id <- stringr::str_extract(csv_fn, "[0-9]{4}")
    
    tibble::tibble(vol_id, n_sess, n_materials)
  }
  
}

detect_n_participants <- function(df) {
  sum(stringr::str_detect(names(df), "participant[1-9]?_ID"))
}

generate_sessions_materials_df <- function(csv_folder = "participant-demographics/csv") {
  stopifnot(is.character(csv_folder))
  stopifnot(file.exists(csv_folder))
  
  fl <- list.files(csv_folder, "[0-9]{4}\\-sess\\-materials\\.csv", full.names = TRUE)
  if (length(fl) <= 0) {
    message("No session/materials CSV files found: ", csv_folder)
    NULL
  } else {
    purrr::map_df(fl, count_sessions_materials_folders)
  }
}

extract_participant_vars <- function(df) {
  stopifnot(is.data.frame(df))
  
  stringr::str_match(names(df), "participant[1-9]?_([A-Za-z]+)")[,2]
}

extract_participant_identifiers <- function(df) {
  stopifnot(is.data.frame(df))
  
  x <- unique(stringr::str_extract(names(df), "participant[1-9]?"))
  x[!is.na(x)]
}

# If there are multiple participants, augmented data frame
alter_sess_df_w_mult_part <- function(df) {
  if (is.null(df)) return(NULL)
  stopifnot(is.data.frame(df))
  
  n_particip <- detect_n_participants(df)
  if (n_particip > 0) {
    purrr::map_df(1:n_particip, select_particip_sess_by_number, df)    
  } else {
    message("No participant data...skipping")
    NULL
  }
}

# Helper function to extract data frame for participant1, participant2, etc.
select_particip_sess_by_number <- function(i, df) {
  stopifnot(is.numeric(i))
  stopifnot(i > 0)
  stopifnot(is.data.frame(df))
  
  vars_not_part_specific <- !(stringr::str_detect(names(df), "participant"))
  if (i == 1) {
    part_specific_vars <- stringr::str_detect(names(df), "participant[1]?_")
  } else {
    part_specific_vars <- stringr::str_detect(names(df), paste0("participant[", i, "]{1}"))
  }
  select_these <- vars_not_part_specific | part_specific_vars
  out_df <- df[, select_these]
  stripped_names <- stringr::str_replace(names(out_df), paste0("participant[", i, "]{1}"), "participant")
  names(out_df) <- stripped_names
  out_df
}

make_cleaned_session_df <- function(csv_fn) {
  stopifnot(is.character(csv_fn))
  stopifnot(file.exists(csv_fn))
  
  message("Extracting session info: ", csv_fn)
  df <- extract_sessions_from_vol_csv(csv_fn)
  alter_sess_df_w_mult_part(df)
}