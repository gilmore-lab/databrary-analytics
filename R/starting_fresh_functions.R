# R/starting_fresh_functions.R
# Generate all new supporting data files

source("R/institution_investigator_functions.R")
source("R/volume_asset_functions.R")
source("R/functions.R")

#' New analytics project setup
#' 
#' This function is used to set-up the CSV folders and files and the
#' .gitignore files that are needed to generate the Databrary analytics report.
#' 
#' @returns Errors and warnings from component functions.
#' @param my_db_login Your Databrary login, usually an email address.
#' @param vb A logical value indicating whether verbose output will be returned.
#' TRUE by default.
#'  
setup_anew <- function(my_db_login, vb = TRUE) {
  
  stopifnot(is.character(my_db_login))
  stopifnot(is.logical(vb))
  
  if (vb) message("\nChecking R package dependencies")
  check_dependencies()
  
  if (vb) message("\nChecking credentials for external services...")
  check_all_credentials(my_db_login)
  
  if (vb) message("\nChecking data directories and .gitignore files...")
  prepare_to_update_csvs()
  
  if (vb) message("\nGenerating new institution-level CSVs...")
  generate_fresh_inst_csvs(delete_old = TRUE)
  
  if (vb) message("\nGenerating session-level asset statistics...")
  generate_fresh_asset_stats_csvs(delete_old = TRUE)
  
  if (vb) message("\nGenerating volume demographics CSVs...")
  generate_fresh_volume_demo_csvs(delete_old = TRUE)
  
  if (vb) message("\nGenerating volume owner CSVs...")
  generate_fresh_owners_csvs()
}

#------------------------------------------------------------------------------
check_dependencies <- function() {
  if (!require(renv)) {
    message("Package `renv` not installed. Installing...")
    install.packages('renv')
  }
  message("Installing required package dependencies via `renv`...")
  renv::restore()
}

#------------------------------------------------------------------------------
check_all_credentials <- function(my_db_login) {
  
  stopifnot(is.character(my_db_login))
  
  db_status <- databraryr::login_db(my_db_login)
  if (db_status == FALSE) {
    message("Invalid Databrary credentials.")
    message("Run `databraryr::config_pwd()` to add login credentials.")
  } else {
    message("Credentials verified with Databrary.")
    databrary_login_renviron <- Sys.getenv("DATABRARY_LOGIN")
    if (databrary_login_renviron != my_db_login) {
      message("Databrary login not found in ~/.Renviron.")
      message(" Edit ~/.Renviron with `usethis::edit_r_environ('user')`")
      message(" Add DATABRARY_LOGIN='YOUR_LOGIN@YOUR_INST' to ~/.Renviron.")
    } else {
      message("Databrary login found in ~/.Renviron.")
    }
  }
  
  google_maps_api_key_status <- Sys.getenv("GGMAP_GOOGLE_API_KEY")
  if (google_maps_api_key_status == "") {
    message("No Google Maps API key in ~/.Renviron.")
    message("  Edit ~/.Renviron with `usethis::edit_r_environ('user')`")
    message("  Add GGMAP_GOOGLE_API_KEY=<YOUR_GOOGLE_API_KEY> to ~/.Renviron")
    message("  Restart R session and try `check_all_credentials()` again.")
  } else {
    message("Google maps credentials verified.")
  }
}

#------------------------------------------------------------------------------
generate_fresh_volume_demo_csvs <- function(delete_old = FALSE) {
  if (delete_old) unlink("src/csv/*demog.csv")
  
  max_ids <- update_max_vol_party_ids()
  get_volume_demo_save_csv_mult(1, max_ids$MAX_VOL_ID)
}

#------------------------------------------------------------------------------
generate_fresh_owners_csvs <- function(delete_old = FALSE) {
  if (delete_old) unlink("src/csv/*owners.csv")
  max_ids <- update_max_vol_party_ids()
  get_all_owners_save_csvs(max_ids$MAX_VOL_ID)
}

#------------------------------------------------------------------------------
generate_fresh_inst_csvs <- function(delete_old = FALSE) {
  if (delete_old) unlink("src/csv/institutions.csv")
  
  max_ids <- update_max_vol_party_ids()
  update_inst_csv(max_id = max_ids$MAX_PARTY_ID, update_geo = TRUE)
}

#------------------------------------------------------------------------------
generate_fresh_asset_stats_csvs <- function(delete_old = FALSE) {
  if (delete_old) unlink("src/csv/*assets.csv")
  max_ids <- update_max_vol_party_ids()
  
  update_all_vol_stats(max_volume_id = max_ids$MAX_VOL_ID, vb = TRUE)
}

#------------------------------------------------------------------------------
check_directory_create <-
  function(this_dir,
           create_new = TRUE,
           full_path = TRUE,
           vb = TRUE) {
    if (!dir.exists(this_dir)) {
      if (create_new) {
        if (vb)
          message("Directory not found. Creating: '", this_dir, "'.")
        dir.create(this_dir, recursive = full_path)
      } else {
        if (vb) message("Directory not found: '", this_dir, "'.")
        FALSE
      }
    } else {
      if (vb)
        message("Directory found: '", this_dir, "'.")
      TRUE
    }
  }

#------------------------------------------------------------------------------
create_required_directories <- function(required_dirs = c("src/archive", "src/csv")) {
  purrr::map(required_dirs, check_directory_create) |>
    purrr::list_c()
}

#------------------------------------------------------------------------------
add_gitignore_to_csv_dir <- function(csv_dir) {
  stopifnot(dir.exists(csv_dir))
  
  fileConn <- file(file.path(csv_dir, '.gitignore'))
  writeLines(c('*sess-materials.csv',
               '*assets.csv',
               '*owners.csv',
               'inst*',
               '*.csv'), fileConn)
  close(fileConn)
  message("Created .gitignore in ", csv_dir)
}

#------------------------------------------------------------------------------
prepare_to_update_csvs <- function() {
  create_required_directories()
  add_gitignore_to_csv_dir('src/csv')
}

#------------------------------------------------------------------------------
update_csv_folders_data_rpt <- function() {
  prepare_to_update_csvs()
  
  message("Updating supporting CSV files. Please be patient.")
  targets::tar_make()
  
  bookdown::render_book('src')
}
