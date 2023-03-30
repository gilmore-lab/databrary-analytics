# R/starting_fresh_functions.R
# Generate all new supporting data files

source("R/institution_investigator_functions.R")
source("R/volume_asset_functions.R")

check_all_credentials <- function(my_db_login) {
  
  db_status <- databraryapi::login_db(my_db_login)
  if (db_status == FALSE) {
    message("Invalid Databrary credentials.")
    message("Run `databraryapi::config_pwd()` to add login credentials.")
  } else {
    message("Credentials verified with Databrary.")
    databrary_login_renviron <- Sys.getenv("DATABRARY_LOGIN")
    if (databrary_login_renviron != my_db_login) {
      message("Databrary login not found in ~/.Renviron.")
      message("  Edit ~/.Renviron with `usethis::edit_r_environ('user')`")
      message("  Add DATABRARY_LOGIN='YOUR_LOGIN@YOUR_INST' to ~/.Renviron.")
    } else {
      message("Databrary login found in ~/.Renviron.")
    }
  }
  
  google_maps_api_key_status <- Sys.getenv("GGMAP_GOOGLE_API_KEY")
  if (google_maps_api_key_status == "") {
    message("No Google Maps API key in ~/.Renviron.")
  } else {
    message("Google maps credentials verified.")
  }
}

#max_ids <- update_max_vol_party_ids()

generate_fresh_inst_csvs <- function() {
  unlink("src/csv/institutions.csv")
  
  max_ids <- update_max_vol_party_ids()
  update_inst_csv(max_id = max_ids$MAX_PARTY_ID, update_geo = TRUE)
}

generate_fresh_asset_stats_csvs <- function() {
  max_ids <- update_max_vol_party_ids()
  
  update_all_vol_stats(max_volume_id = max_ids$MAX_VOL_ID, vb = TRUE)
}