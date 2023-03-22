# R/databrary_functions.R
# Databrary-related functions

#-------------------------------------------------------------------------------
db_credentials_valid <- function() {
  if (file.exists('.databrary.RData')) {
    TRUE
  } else {
    FALSE
  }
}