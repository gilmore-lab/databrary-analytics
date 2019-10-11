copy_move_rpt <- function()

  {

# run from "/R/databrary-analytics" directory

current_weekly_rpt <- list.files(path = "weekly",".html$")

new_weekly_rpt <- paste0(Sys.Date(), '_', current_weekly_rpt)

file.exists(paste0("weekly/", current_weekly_rpt))
file.copy(paste0("weekly/",current_weekly_rpt), paste0("weekly/",new_weekly_rpt))


file.rename(paste0("weekly/",new_weekly_rpt), paste0("old_weekly_reports/",new_weekly_rpt))
          }