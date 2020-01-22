# get_citation_stats returns the number of citations to Databrary or Datavyu
# from Google Scholar
get_citation_stats <- function(project = 'databrary') {
  if (project %in% c('databrary', 'Databrary')) {
    url <- 'https://scholar.google.com/scholar?hl=en&as_sdt=1%2C39&as_vis=1&q=%22databrary%22&btnG='
  } else if (project %in% c('datavyu', 'Datavyu')) {
    url <- 'https://scholar.google.com/scholar?hl=en&as_sdt=1%2C39&as_vis=1&q=%22datavyu%22&btnG='
  }
  
  r <- httr::GET(url = url)
  if (httr::status_code(r) == 200) {
    content <- httr::content(r, 'text')
  } else {
    message(paste0('Download Failed, HTTP status ', httr::status_code(r)))
  }
  
  n_results <- stringr::str_match(content, pattern = "About ([0-9]+)")[2]
  if (is.null(n_results)) {
    message(paste0('Unable to parse results from search.'))
    return(NULL)
  } else {
    return(as.numeric(n_results))
  }
}