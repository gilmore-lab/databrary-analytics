# Generate reports

## Log in to Databrary

login <- databraryapi::login_db()
max_party_id <- 8104
update_asset_data <- FALSE # This is very time consuming, but should be done periodically, probably quarterly
update_demog_data <- FALSE # This is very time consuming, but should be done periodically, probably quarterly

## Funders report

message("----- Generating funders report -----")
rmarkdown::render(rmarkdown::render(input = "funders/funder-report.Rmd",
                                    output = "funders/funder-report.html",
                                    params = list(use_saved_file = "False")))

## Institutions

message("----- Generating institutions report -----")
source("institutions-investigators/R/helpers.R")
render_institutions_report(max_party_id = max_party_id)

## Investigators

message("----- Generating investigators report -----")
render_investigators_report()

## Shared volumes and sessions
message("----- Generating volumes and sessions report -----")
rmarkdown::render("shared-volumes-sessions/shared-volumes-sessions.Rmd")

## Shared assets

message("----- Generating volume assets report -----")
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
if (update_demog_data) {
  message("-- Regenerating demog data from all volumes --")
  rmarkdown::render("participant-demographics/participant-demog-report.Rmd", 
                    params=list(update_demo_csvs=TRUE))
} else {
  message("-- Using previously saved volume asset data --")
  rmarkdown::render("volumes-with-videos-annotations/assets-stats.Rmd", 
                    params=list(update_demo_csvs=FALSE))
  
}

