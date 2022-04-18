make_funders_report <- function(use_saved_file = "False",
                                max_vol_id = 1450) {
  
  if (!is.character(use_saved_file)) stop("`use_saved_file` must be a character string.")
  if (max_vol_id <= 0) stop("`max_vol_id` must be > 0.")
  
  message("----- Generating funders report -----")
  copy_to_archive("funders", "funder-report.html")
  rmarkdown::render(input = "funders/funder-report.Rmd",
                    params = list(use_saved_file = "False", 
                                  max_vol_id = max_volume_id ))  
}

make_institutions_report <- function(max_party_id = 9750) {
  
  if (max_party_id <= 0) stop("`max_party_id` must be > 0.")
  if (length(max_party_id > 1)) stop("`max_party_id` must have length == 1.")
  
  message("----- Generating institutions report -----")
  source("institutions-investigators/R/helpers.R")
  copy_to_archive("institutions-investigators", "institutions.html")
  render_institutions_report(max_party_id = max_party_id)
}

make_investigators_report <- function() {
  message("----- Generating investigators report -----")
  copy_to_archive("institutions-investigators", "investigators.html")
  render_investigators_report()
}

make_shared_volumes_sessions_report <- function(save_file = TRUE,
                                                 use_saved_file = FALSE,
                                                update_shared_volumes = TRUE) {
  
  if (!is.logical(save_file)) stop("`save_file` must be a logical value.")
  if (!is.logical(use_saved_file)) stop("`use_saved_file` must be a logical value.")
  
  message("----- Generating volumes and sessions report -----")
  copy_to_archive("shared-volumes-sessions", "shared-volumes-sessions.html")
  
  if (update_shared_volumes) {
    rmarkdown::render("shared-volumes-sessions/shared-volumes-sessions.Rmd",
                      params = list(save_file = save_file,
                                    use_saved_file = use_saved_file))  
  } else {
    rmarkdown::render("shared-volumes-sessions/shared-volumes-sessions.Rmd")  
  }
}

make_volume_assets_report <- function(use_saved_csvs=FALSE,
                                      max_vol_id = 1450,
                                      update_asset_data = FALSE) {
  
  if (!is.logical(us_saved_csvs)) stop("`use_saved_csvs` must be a logical value.")
  if (max_vol_id <= 0) stop("`max_vol_id` must be > 0.")
  
  message("----- Generating volume assets report -----")
  copy_to_archive("volumes-with-videos-annotations", "assets-stats.html")
  if (update_asset_data) {
    message("-- Regenerating asset data from all volumes --")
    rmarkdown::render("volumes-with-videos-annotations/assets-stats.Rmd", 
                      params=list(use_saved_csvs=FALSE,
                                  max_vol_id = max_volume_id))
  } else {
    message("-- Using previously saved volume asset data --")
    rmarkdown::render("volumes-with-videos-annotations/assets-stats.Rmd", 
                      params=list(use_saved_csvs=use_saved_csvs))
  }
}

make_volume_demographics_report() {
  
}