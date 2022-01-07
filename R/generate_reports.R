# Generate reports

## Requirements
require(databraryapi)

## Helper function

copy_to_archive <- function(fpath, fn) {
  if (file.exists(paste0(fpath, "/", fn))) {
    current_fn <- paste0(fpath, "/", fn)
    new_fn <- paste0(fpath, "/archive/", Sys.Date(), '_', fn)
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

## Log in to Databrary

login <- databraryapi::login_db()
max_party_id <- 8700
max_volume_id <- 1773 # Must end in zero for now, so see next function
max_party_id <- make_next_ten(max_party_id)

update_asset_data <- FALSE # This is very time consuming, but should be done periodically, probably quarterly
update_demog_data <- FALSE # This is very time consuming, but should be done periodically, probably quarterly

## Funders report

### Default is to update on each run

message("----- Generating funders report -----")
copy_to_archive("funders", "funder-report.html")
rmarkdown::render(input = "funders/funder-report.Rmd",
                  output_file = "funder-report.html",
                  params = list(use_saved_file = "False"))

## Institutions

message("----- Generating institutions report -----")
source("institutions-investigators/R/helpers.R")
copy_to_archive("institutions-investigators", "institutions.html")
render_institutions_report(max_party_id = max_party_id)

## Investigators

message("----- Generating investigators report -----")
copy_to_archive("institutions-investigators", "investigators.html")
render_investigators_report()

## Shared volumes and sessions

message("----- Generating volumes and sessions report -----")
copy_to_archive("shared-volumes-sessions", "shared-volumes-sessions.html")
rmarkdown::render("shared-volumes-sessions/shared-volumes-sessions.Rmd")

## Shared assets

message("----- Generating volume assets report -----")
copy_to_archive("volumes-with-videos-annotations", "assets-stats.html")
if (update_asset_data) {
  message("-- Regenerating asset data from all volumes --")
  rmarkdown::render("volumes-with-videos-annotations/assets-stats.Rmd", 
                    params=list(use_saved_csvs=FALSE))
} else {
  message("-- Using previously saved volume asset data --")
  rmarkdown::render("volumes-with-videos-annotations/assets-stats.Rmd", 
                    params=list(use_saved_csvs=TRUE))
  
}

## Volume-level demographics

message("----- Generating volume-level demographics report -----")
copy_to_archive("participant-demographics", "participant-demog-report.html")
if (update_demog_data) {
  message("-- Regenerating demog data from all volumes --")
  
  # Delete "old" demog files
  old_demo_fn <- list.files("participant-demographics/csv", "demog", full.names = TRUE)
  sapply(old_demo_fn, unlink)
  
  # Generate report
  rmarkdown::render("participant-demographics/participant-demog-report.Rmd",
                    params=list(new_vol_rg_min = 1,
                                new_vol_rg_max = max_volume_id,
                                update_demo_csvs=TRUE))
} else {
  message("-- Using previously saved demog data --")
  rmarkdown::render("participant-demographics/participant-demog-report.Rmd", 
                    params=list(update_demo_csvs=FALSE))
  
}

## Tags and keywords

message("----- Generating tags and keywords report -----")
source("tags-keywords/R/helpers.R")
copy_to_archive("tags-keywords", "tags-keywords-report.html")
rmarkdown::render("tags-keywords/tags-keywords-report.Rmd")

## Clean-up

databraryapi::logout_db()
if (file.exists(".databrary.RData")) unlink(".databrary.RData")
if (file.exists("funders/.databrary.RData")) unlink("funders/.databrary.RData")
if (file.exists("institutions-investigators/.databrary.RData")) unlink("institutions-investigators/.databrary.RData")
if (file.exists("shared-volumes-sessions/.databrary.RData")) unlink("shared-volumes-sessions/.databrary.RData")
if (file.exists("volumes-with-videos-annotations/.databrary.RData")) unlink("volumes-with-videos-annotations/.databrary.RData")
if (file.exists("tags-keywords/.databrary.RData")) unlink("tags-keywords/.databrary.RData")
