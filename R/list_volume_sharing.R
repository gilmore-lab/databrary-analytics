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

get_volume_owners <- function(min_vol = 1, max_vol = 10) {
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
  vol_own_list <- lapply(vol_index, get_volume_owner)
  if (!is.null(vol_own_list)) {
    plyr::rbind.fill(vol_own_list)
  }
}

get_volume_owner <- function(id) {
  vol_data <- databraryapi::download_containers_records(id)
  if (is.null(vol_data)) {
    NULL
  } else {
    data.frame(id = vol_data$id, owners = vol_data$owners)
  }
}

list_shared_vols_owns <- function(min_vol = 1, max_vol = 10) {
  if (max_vol < 1) {
    stop("max_vol must be >= 1")
  }
  if (min_vol < 1) {
    stop("min_vol must be >= 1")
  }
  if (min_vol >= max_vol) {
    stop("min_vol must be < max_vol")
  }
  
  #vol_index <- min_vol:max_vol
  vols_shared <- list_volume_sharing(min_vol, max_vol)
  vols_own <- get_volume_owners(min_vol, max_vol)
  
  # Merge data frames
  dplyr::left_join(vols_shared, vols_own, by = ("id" = "id"))
}