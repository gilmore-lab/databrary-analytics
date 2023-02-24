# report_rendering_functions.R

# These functions update the analytics data and render the report.

#-------------------------------------------------------------------------------
update_analytics_data <- function() {
  library(targets)
  tar_make()
}

#-------------------------------------------------------------------------------
render_report_and_open <- function() {
  bookdown::render_book('src')
  browseURL("docs/index.html")
}