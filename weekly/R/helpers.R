# helper functions for databrary_weekly_report

install_required_pkgs <- function() {
  cran_packages <- c('ggplot2', 'tidyverse', 'reshape2', 'cowplot',
                     'googledrive', 'plyr', 'googlesheets', 'ezknitr',
                     'reshape2', 'kableExtra', 'googledrive', 'openssl', 'purrr')
  not_installed <- cran_packages[!(cran_packages %in% installed.packages()[,"Package"])]
  if(length(not_installed)) {
    message('Installing packages required for this document.')
    install.packages(not_installed)
  }
  
  github_packages <- 'PLAY-behaviorome/databraryapi'
  not_installed <- github_packages[!(github_packages %in% installed.packages()[,"Package"])]
  if(length(not_installed)) devtools::install_github(not_installed)
}

update_weekly_report <- function(db_account="yourname@email.com") {
  if (purrr::is_empty(db_account)) {
    stop("You must provide your Databrary login.")
  }
  if (!is.character(db_account)) {
    stop("'db_account' must be a string.")
  }
  
  rmarkdown::render("weekly/databrary_weekly_report.Rmd", output_format = "html_document", 
                    params = list(db_account = db_account, 
                                  update_gs = FALSE,
                                  update_stats = TRUE,
                                  update_csv = TRUE))
  copy_to_archive()  
}

render_weekly_report <- function(db_account="yourname@email.com") {
  if (purrr::is_empty(db_account)) {
    stop("You must provide your Databrary login.")
  }
  if (!is.character(db_account)) {
    stop("'db_account' must be a string.")
  }

  rmarkdown::render("weekly/databrary_weekly_report.Rmd", output_format = "html_document", 
                    params = list(db_account = db_account, 
                                  update_gs = FALSE,
                                  update_stats = FALSE,
                                  update_csv = FALSE))
}

copy_to_archive <- function() {
  if (file.exists("weekly/databrary_weekly_report.html")) {
    current_fn <- "weekly/databrary_weekly_report.html"
    new_fn <- paste0("weekly/archive/", Sys.Date(), '_', "databrary_weekly_report.html")
    if (file.copy(current_fn, new_fn)) {
      message("Report copied to 'weekly/archive'.")
    }
  } else {
    message("No file copied.")
  }
}