make_institutional_party_df <- function(inst_id) {
  if (databraryapi::is_institution(inst_id)) {
    these_affiliates <- databraryapi::get_affiliates(inst_id)
    if (is_empty(these_affiliates) | is.null(these_affiliates)) {
      df <- data.frame(inst_id = inst_id,
                       affiliation = NA,
                       inst_url = paste0("https://nyu.databrary.org/party/", inst_id),
                       party_id = NA,
                       sortname = NA,
                       prename = NA,
                       party_url = NA)    
    } else {
      df <- these_affiliates
      df$inst_id <- inst_id
      df <- dplyr::rename(df, party_id = id)
      df <- dplyr::mutate(df, inst_url = paste0("https://nyu.databrary.org/party/", inst_id),
                          party_url = paste0("https://nyu.databrary.org/party/", party_id))
    } 
    df
  } else {
    NULL
  }
}

get_institution_party_df <- function(max_inst_id = 20) {
  purrr::map_dfr(1:max_inst_id, make_institutional_party_df)
} 

render_institutions_investigators_report <- function(db_login) {
  rmarkdown::render("institutions-investigators/institutions-investigators.Rmd",
                    params = list(db_login = db_login, 
                    max_party_id = 1600))
  clean_up()
}

clean_up <- function() {
  databraryapi::logout_db()
  if (file.exists("institutions-investigators/.databrary.RData")) unlink("institutions-investigators/.databrary.RData")
}