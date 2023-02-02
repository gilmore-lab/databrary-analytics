list_shared_volumes <- function(min_vol = 1, max_vol = 800, sharing_level = 'full',
                                sharing_permission_lvls = c("none", "shared_w_db", "collaborator", "co-owner", "owner"),
                                vb = TRUE) {
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
  if (vb) message(paste0("Getting shared volume numbers."))
  volume_sharing_df <- list_volume_sharing(min_vol, max_vol)
  if (is.null(volume_sharing_df)) {
    error("Shared volume list is empty.")
  }
  shared_vol_ids <- volume_sharing_df[volume_sharing_df$sharing_level == sharing_level, 'id']
  if (vb) message(paste0("Found ", length(shared_vol_ids), " volumes with 'sharing_level' == '", 
                 sharing_level, "'."))
  
  if (vb) message("Getting metadata from shared volumes.")
  shared_vol_metadata_list <- lapply(shared_vol_ids, databraryapi::list_volume_metadata)
  if (is.null(shared_vol_metadata_list)) {
    if (vb) message("Volume metadata list is empty.")
    NULL
  } else {
    df <- plyr::rbind.fill(shared_vol_metadata_list)
    df$how_shared_w_me <- sharing_permission_lvls[df$permission]
    df
  }
}

save_shared_volumes_csv <- function(min_vol = 1, max_vol = 800, 
                                    out.fn = "csv/shared_vols_metadata.csv") {
  df <- list_shared_volumes(min_vol = min_vol, max_vol = max_vol, sharing_level = 'full')
  if (!is.null(df)) {
    # Should check to see if file exists before overwriting
    write.csv(df, file = out.fn, row.names = FALSE)
    cat(paste0("Saved volume metadata to '", out.fn, "'."))
  } else {
    error("List of shared volumes is empty.")
  }
}