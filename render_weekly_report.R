render_weekly_report <- function ()
{
  rmarkdown::render("weekly/databrary_weekly_report.Rmd", "html_document")
  source("weekly/copy_move_rpt.R")
  copy_move_rpt()
}