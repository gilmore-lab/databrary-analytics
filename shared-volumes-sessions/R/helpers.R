update_shared_volumes_sessions <-
  function(db_login = "email@provider.com") {
    if (purrr::is_empty(db_login)) {
      stop("You must provide your Databrary login.")
    }
    if (!is.character(db_login)) {
      stop("'db_account' must be a string.")
    }
    
    rmarkdown::render(
      "shared-volumes-sessons/shared-volumes-sessions.Rmd",
      output_format = "html_document",
      params = list(db_login = db_account,
                    use_saved_file = FALSE)
    )
    
    databraryapi::logout_db()
  }

render_shared_volumes_sessions <-
  function(db_login = "email@provider.com") {
    if (purrr::is_empty(db_login)) {
      stop("You must provide your Databrary login.")
    }
    if (!is.character(db_login)) {
      stop("'db_login' must be a string.")
    }
    
    rmarkdown::render(
      "shared-volumes-sessions/shared-volumes-sessions.Rmd",
      output_format = "html_document",
      params = list(db_login = db_login,
                    use_saved_file = TRUE)
    )
    
    databraryapi::logout_db()
  }
