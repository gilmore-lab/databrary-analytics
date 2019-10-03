copy_move_file <- function()

  {
  
# Copy weekly report .html
# Rename by prepending date
# Move to old reports folder

current_weekly_rpt <- list.files(path = "weekly",".html$")


new_weekly_rpt <- paste0(Sys.Date(), '_', current_weekly_rpt)

file.exists(paste0("/weekly/", new_weekly_rpt))
file.copy(current_weekly_rpt, new_weekly_rpt)

library(filesstrings)
file.move(new_weekly_rpt, paste0("/old_reports", new_weekly_rpt))
          
          }