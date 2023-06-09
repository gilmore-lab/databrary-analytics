# R/check_readiness_functions.R

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
  
  require(targets)
  
  message("Updating supporting CSV files. Please be patient.")
  tar_make()
  
  bookdown::render_book('src')
}

#------------------------------------------------------------------------------
# check_db_creds <- function(env_var = "DATABRARY_LOGIN") {
#   stopifnot(is.character(env_var))
#   
#   db_login <- Sys.getenv(env_var)
#   if (db_login == "") {
#     message(env_var, " not found in user .Renviron.")
#     message("Recommend saving ",  env_var, " in user or project .Renviron. via `usethis::edit_r_environment()`.")
#     message("Add DATABRARY_LOGIN='", env_var, "' to .Renviron.")
#   } else {
#     message(env_var, " found in `.Renviron`.")
#   }
# }