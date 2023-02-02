# Authenticate to Google for Sheets access
run_db_weekly <- function(db_account = "rogilmore@psu.edu") {
  db <- googlesheets::gs_title('Databrary-analytics')
  rmarkdown::render('databrary_weekly_rpt.Rmd', 
                    params = list(db_account = db_account,
                                  update_gs = TRUE,
                                  update_stats = TRUE))  
}
