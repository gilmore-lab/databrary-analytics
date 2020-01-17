# render shared-volumes-sessions.Rmd

render_volumes_report <- function(db_account) {
  if (!is.character(db_account)) stop("Databrary login required.")
  rmarkdown::render("shared-volumes-sessions/shared-volumes-sessions.Rmd",
                    params = list(db_login=db_account))  
}