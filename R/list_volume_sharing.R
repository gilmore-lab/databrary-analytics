# Creates a data frame with volume id and the sharing level ('full', 'none', 'restricted')
# 
# NOTE: This is based on databraryapi::download_containers_records, and could probably
# be sped up and made more efficient.
list_volume_sharing <- function(min_vol = 1, max_vol = 800) {
  if (max_vol < 1) {
    stop("max_vol must be >= 1")
  }
  if (min_vol < 1) {
    stop("min_vol must be >= 1")
  }
  if (min_vol >= max_vol) {
    stop("min_vol must be < max_vol")
  }
  
  vol_index <- min_vol:max_vol
  vol_sharing_list <- lapply(vol_index, get_sharing_level)
  if (!is.null(vol_sharing_list)) {
    plyr::rbind.fill(vol_sharing_list)
  }
}

get_sharing_level <- function(id) {
  vol_data <- databraryapi::download_containers_records(id)
  if (is.null(vol_data)) {
    NULL
  } else {
    data.frame(id = vol_data$id, sharing_level = vol_data$publicaccess)
  }
}