list_shared_volumes <- function(min_vol = 1, max_vol = 800, sharing_level = 'full') {
  if (max_vol < 1) {
    stop("max_vol must be >= 1")
  }
  if (min_vol < 1) {
    stop("min_vol must be >= 1")
  }
  if (min_vol >= max_vol) {
    stop("min_vol must be < max_vol")
  }
  source("R/list_volume_sharing.R")
  
  # Add Databrary login here...
  
  # what is the largest volume id? Should know
  cat("Getting shared volume indices.\n")
  volume_sharing_df <- list_volume_sharing(min_vol, max_vol)
  if (is.null(volume_sharing_df)) {
    error("Shared volume list is empty.")
  }
  shared_vol_ids <- volume_sharing_df[volume_sharing_df$sharing_level == sharing_level, 'id']
  cat(paste0("Found ", length(shared_vol_ids), " volumes with 'sharing_level' == '", sharing_level, "'.\n"))
  
  cat("Getting metadata from shared volumes.\n")
  shared_vol_metadata_list <- lapply(shared_vol_ids, databraryapi::list_volume_metadata)
  if (is.null(shared_vol_metadata_list)) {
    error("Volume metadata list is empty.")
  } else {
    plyr::rbind.fill(shared_vol_metadata_list)
  }
}

save_shared_volumes_csv <- function(min_vol = 1, max_vol = 800, 
                                    out.fn = "csv/shared_vols_metadata.csv") {
  df <- list_shared_volumes(min_vol = min_vol, max_vol = max_vol, sharing_level = 'full')
  if (!is.null(df)) {
    # Should check to see if file exists before overwriting
    write.csv(df, file = out.fn, row.names = FALSE)
    cat(paste0("Saved volume metadata to '", out.fn, "'\n"))
  } else {
    error("List of shared volumes is empty.")
  }
}