# helpers for tags-keywords-report

install_required_pkgs <- function() {
  cran_packages <- c('wordcloud', 'plyr')
  not_installed <- cran_packages[!(cran_packages %in% installed.packages()[,"Package"])]
  if (length(not_installed)) {
    message('Installing packages required for this document.')
    install.packages(not_installed)
  }
  
  github_packages <- 'PLAY-behaviorome/databraryapi'
  not_installed <- github_packages[!(github_packages %in% installed.packages()[,"Package"])]
  if (length(not_installed)) devtools::install_github(not_installed)
}

make_volume_tags_df <- function(vol_id) {
  message(paste0("Gathering tags from volume ", vol_id))
  these_tags <- databraryapi::list_volume_tags(vol_id)
  if (is_empty(these_tags)) {
    df <- data.frame(vol_id = vol_id, 
                     url = paste0("https://nyu.databrary.org/volume/",
                                  vol_id),
                     tags = NA, weight = NA)    
  } else {
    these_tags <- these_tags %>%
      dplyr::select(., id, weight) %>%
      dplyr::rename(., tags = id)
    df <- these_tags
    df$vol_id = vol_id
    df$url <- paste0("https://nyu.databrary.org/volume/", vol_id)
  }
  dplyr::select(df, vol_id, url, tags, weight) 
}

render_tags_keywords_report <- function(db_login) {
  rmarkdown::render("tags-keywords/tags-keywords-report.Rmd", 
                    params = list(db_login = db_login, 
                                  read_saved = TRUE))
  clean_up()
}

update_tags_keywords_report <- function(db_login) {
  start_time <- Sys.time()
  rmarkdown::render("tags-keywords/tags-keywords-report.Rmd", 
                    params = list(db_login = db_login, 
                                  read_saved = FALSE))
  clean_up()
  end_time <- Sys.time()
  message(paste0("Generating the report took ", end_time - start_time))
}

clean_up <- function() {
  databraryapi::logout_db()
  if (file.exists("tags-keywords/.databrary.RData")) unlink("tags-keywords/.databrary.RData")
}