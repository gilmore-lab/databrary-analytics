extract_particip_info <- function(df) {
  select(df, contains('participant'))
}

remove_materials <- function(df) {
  filter(df, !str_detect(`session-date`, '[Mm]aterials'))
}

extract_unique_participant_info <- function(df, vb = FALSE) {
  if(vb) message("Extracting participant info.")
  df %>%
    remove_materials() %>%
    extract_particip_info() %>%
    distinct()
}

extract_single_participant <- function(i, df) {
    d <- select(df, contains(paste0('participant', i)))
    rename_with(d, ~ gsub("[0-9]+", "", .x))
}

extract_participant_ids <- function(df) {
  these_cols <- colnames(df)
  str_extract(these_cols, '([0-9]+)') |> unique()
}

convert_vol_ss_to_particip_df <- function(df, vol_id, vb = FALSE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(vol_id))
  
  unique_p_info <- extract_unique_participant_info(df, vb)
  if (dim(unique_p_info)[2] == 0) {
    if (vb) message("No participant data.")
    return(NULL)
  }
  if (vb) message("Making participant df.")
  p_ids <- extract_participant_ids(unique_p_info)
  if (length(p_ids) > 1) {
    df <- purrr::map(p_ids, extract_single_participant, unique_p_info) |>
      list_rbind()
  } else {
    df <- unique_p_info
  }
  df$vol_id <- vol_id
  df
}

convert_vol_ss_csv_to_particip_df <- function(csv_fn,
                                              vb = FALSE) {
  stopifnot(is.character(csv_fn))
  
  vol_id <- unique(str_extract(basename(csv_fn), '([0-9]{4})'))
  
  df <- readr::read_csv(csv_fn, col_types = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
                        show_col_types = FALSE)
  convert_vol_ss_to_particip_df(df, vol_id, vb)
}

create_aggregate_demog_df <- function(csv_dir = 'src/csv', vb = FALSE) {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  fl <- list.files(csv_dir, "[0-9]+\\-sess\\-materials.csv", full.names = TRUE)
  purrr::map(fl, convert_vol_ss_csv_to_particip_df, vb, .progress = "Particip df") %>%
    list_rbind()
}

