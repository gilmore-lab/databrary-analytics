render_weekly_report <- function (db_account="yourname@email.com")
{
  rmarkdown::render("weekly/databrary_weekly_report.Rmd", output_format = "html_document", 
                    params = list(db_account = db_account, 
                                  update_gs = TRUE,
                                  update_stats = TRUE))
  source("weekly/copy_move_rpt.R")
  copy_move_rpt()
}
