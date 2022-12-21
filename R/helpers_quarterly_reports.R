copy_to_archive <- function(fpath, fn) {
  if (file.exists(file.path(fpath, fn))) {
    current_fn <- file.path(fpath, fn)
    new_fn <- file.path(fpath, "archive", paste0(Sys.Date(), '_', fn))
    if (file.copy(current_fn, new_fn)) {
      message("Report copied to '", paste0(fpath, "/archive'"))
    }
  } else {
    message("No file copied.")
  }
}

make_next_ten <- function(n) {
  n_mod_10 <- (n %% 10)
  if (n_mod_10 == 0) {
    n
  } else {
    n + (10 - n_mod_10)
  }
}

load_default_report_parameters <- function() {
  # Parameters
  update_asset_data <-
    TRUE # This is very time consuming, but should be done periodically, probably quarterly
  update_demog_data <-
    TRUE # This is very time consuming, but should be done periodically, probably quarterly
  update_shared_volumes <- TRUE
  update_volume_tags <- TRUE
  
  max_party_id <- 12000
  max_volume_id <-
    1590 # Must end in zero for now, so see next function
  max_party_id <- make_next_ten(max_party_id)
  max_volume_id <- make_next_ten(max_volume_id)
  list(max_party_id = max_party_id, max_volume_id = max_volume_id)
}

make_funders_report <- function(use_saved_file = TRUE,
                                max_volume_id = 1520) {
  if (!is.logical(use_saved_file))
    stop("`use_saved_file` must be a a logical value.")
  if (max_volume_id <= 0)
    stop("`max_volume_id` must be > 0.")
  
  start_time <- Sys.time()
  
  message("----- Generating funders report -----")
  copy_to_archive("funders", "funder-report.html")
  rmarkdown::render(
    input = "funders/funder-report.Rmd",
    params = list(use_saved_file = use_saved_file,
                  max_volume_id = max_volume_id)
  )
  
  Sys.time() - start_time
}

make_institutions_report <- function(max_party_id = 12000, 
                                     db_login = Sys.getenv("DATABRARY_LOGIN")) {
  if (max_party_id <= 0)
    stop("`max_party_id` must be > 0.")
  if (length(max_party_id) > 1)
    stop("`max_party_id` must have length == 1.")
  
  start_time <- Sys.time()
  
  message("----- Generating institutions report -----")
  source("institutions-investigators/R/helpers.R")
  copy_to_archive("institutions-investigators", "institutions.html")
  rmarkdown::render("institutions-investigators/institutions.Rmd",
                    params = list(db_login = db_login, 
                                  max_party_id = max_party_id))
  
  Sys.time() - start_time
}

make_investigators_report <- function(max_party_id = 12000, db_login = Sys.getenv("DATABRARY_LOGIN")) {
  start_time <- Sys.time()
  
  message("----- Generating investigators report -----")
  copy_to_archive("institutions-investigators", "investigators.html")
  rmarkdown::render("institutions-investigators/investigators.Rmd",
                    params = list(db_login = db_login, 
                                  max_party_id = max_party_id))
  
  
  Sys.time() - start_time
}

make_shared_volumes_sessions_report <-
  function(update = FALSE,
           max_volume_id = 1599,
           db_login = Sys.getenv("DATABRARY_LOGIN")) {
    
    if (!is.logical(update))
      stop("`update` must be a logical value.")
    if (max_volume_id <= 0)
      stop("`max_volume_id` must be > 0.")
    
    start_time <- Sys.time()
    
    message("----- Generating volumes and sessions report -----")
    copy_to_archive("shared-volumes-sessions", "shared-volumes-sessions.html")
    
    if (update) {
      rmarkdown::render(
        "shared-volumes-sessions/shared-volumes-sessions.Rmd",
        params = list(save_file = TRUE,
                      use_saved_file = FALSE,
                      max_volume_id = max_volume_id,
                      db_login = db_login)
      )
    } else {
      rmarkdown::render("shared-volumes-sessions/shared-volumes-sessions.Rmd",
                        params = list(use_saved_file = TRUE,
                                      max_volume_id = max_volume_id,
                                      db_login = db_login))
    }
    
    Sys.time() - start_time
  }

make_volume_assets_report <- function(update = FALSE,
                                      max_volume_id = 1599,
                                      db_login = Sys.getenv("DATABRARY_LOGIN")) {
  if (!is.logical(update))
    stop("`update` must be a logical value.")
  if (max_volume_id <= 0)
    stop("`max_volume_id` must be > 0.")
  
  start_time <- Sys.time()
  
  message("----- Generating volume assets report -----")
  copy_to_archive("volume-assets", "assets-stats.html")
  if (update) {
    message("-- Regenerating asset data from all volumes --")
    rmarkdown::render(
      "volume-assets/assets-stats.Rmd",
      params = list(use_saved_csvs = FALSE,
                    max_volume_id = max_volume_id,
                    db_login = db_login)
    )
  } else {
    message("-- Using previously saved volume asset data --")
    rmarkdown::render("volume-assets/assets-stats.Rmd",
                      params = list(use_saved_csvs = TRUE,
                                    db_login = db_login))
  }
  
  Sys.time() - start_time
}

make_volume_demographics_report <- function(update = FALSE, 
                                            max_volume_id = 1540,
                                            db_login = Sys.getenv("DATABRARY_LOGIN")) {
  if (!is.logical(update))
    stop("`update` must be a logical value.")
  if (max_volume_id <= 0)
    stop("`max_volume_id` must be > 0.")
  
  start_time <- Sys.time()
  
  message("----- Generating volume-level demographics report -----")
  copy_to_archive("participant-demographics",
                  "participant-demog-report.html")
  if (update) {
    message("-- Regenerating demog data from all volumes --")
    
    # Delete "old" demog files
    old_demo_fn <-
      list.files("participant-demographics/csv", "demog", full.names = TRUE)
    sapply(old_demo_fn, unlink)
    
    # Generate report
    rmarkdown::render(
      "participant-demographics/participant-demog-report.Rmd",
      params = list(
        new_vol_rg_min = 1,
        new_vol_rg_max = max_volume_id,
        update_demo_csvs = TRUE,
        db_login = db_login
      )
    )
  } else {
    message("-- Using previously saved demog data --")
    rmarkdown::render(
      "participant-demographics/participant-demog-report.Rmd",
      params = list(update_demo_csvs = FALSE)
    )
    
  }
  
  Sys.time() - start_time
}

make_tags_keywords_report <- function(update = FALSE,
                                      max_volume_id = 1520) {
  if (!is.logical(update))
    stop("`update` must be a logical value.")
  if (max_volume_id <= 0)
    stop("`max_volume_id` must be > 0.")
  
  message("----- Generating tags and keywords report -----")
  source("tags-keywords/R/helpers.R")
  copy_to_archive("tags-keywords", "tags-keywords-report.html")
  
  start_time <- Sys.time()
  
  if (update) {
    rmarkdown::render(
      "tags-keywords/tags-keywords-report.Rmd",
      params = list(read_saved = FALSE, max_volume_id = max_volume_id)
    )
    
  } else {
    rmarkdown::render(
      "tags-keywords/tags-keywords-report.Rmd",
      params = list(read_saved = TRUE, max_volume_id = max_volume_id)
    )
  }
  
  Sys.time() - start_time
}

test_reports <- function(max_volume_id = 5) {
  make_tags_keywords_report(update=FALSE,
                            max_volume_id = max_volume_id)
  make_volume_demographics_report(update=FALSE,
                                  max_volume_id = max_volume_id)
  make_volume_assets_report(update=FALSE,
                            max_volume_id = max_volume_id)
  make_shared_volumes_sessions_report(update=FALSE,
                                      max_volume_id = max_volume_id)
  make_investigators_report()
  make_institutions_report(max_party_id = 25)
  make_funders_report(max_volume_id = max_volume_id)
}

clean_up <- function() {
  databraryapi::logout_db()
  if (file.exists(".databrary.RData")) unlink(".databrary.RData")
  if (file.exists("funders/.databrary.RData")) unlink("funders/.databrary.RData")
  if (file.exists("institutions-investigators/.databrary.RData")) unlink("institutions-investigators/.databrary.RData")
  if (file.exists("shared-volumes-sessions/.databrary.RData")) unlink("shared-volumes-sessions/.databrary.RData")
  if (file.exists("volumes-with-videos-annotations/.databrary.RData")) unlink("volumes-with-videos-annotations/.databrary.RData")
  if (file.exists("tags-keywords/.databrary.RData")) unlink("tags-keywords/.databrary.RData")
}

update_tags_report <- function(max_volume_id = 1590) {
  make_tags_keywords_report(update = TRUE,
                            max_volume_id = max_volume_id)
}

update_funders_report <- function(max_volume_id = 1590) {
  make_funders_report(use_saved_file = FALSE,
                                  max_volume_id = max_volume_id) 
}

update_demog_report <- function(max_volume_id = 1590) {
  make_volume_demographics_report(update = TRUE,
                                  max_volume_id = max_volume_id)
}

update_assets_report <- function(max_volume_id = 1590) {
  make_volume_assets_report(update = TRUE, max_volume_id = max_volume_id)
}

update_sessions_report <- function(max_volume_id = 1590) {
  make_shared_volumes_sessions_report(update = TRUE,
                                      max_volume_id)
}

update_institutions_report <- function(max_party_id = 10684) {
  make_institutions_report(max_party_id)
}

update_investigators_report <- function(max_party_id = 10684) {
  make_investigators_report(max_party_id)
}

update_all_reports <- function() {
  p <- load_default_report_parameters()
  
  # volume-level reports
  update_tags_report(p$max_volume_id)
  update_funders_report(p$max_volume_id)
  update_demog_report(p$max_volume_id)
  update_assets_report(p$max_volume_id)
  update_sessions_report(p$max_volume_id)
  
  # party-level reports
  update_institutions_report(p$max_party_id)
  update_investigators_report(p$max_party_id)
  
  # summary reports
  rmarkdown::render("state-of-the-repo/index.Rmd")
  
  # Clean-up
  clean_up()
}

