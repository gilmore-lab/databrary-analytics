# _targets.R

library(targets)
library(tarchetypes)

source("R/functions.R")
source("R/volume_asset_functions.R")
source("R/institution_investigator_functions.R")
source("R/constants.R")

tar_option_set(
  packages = c(
    "readr",
    "dplyr",
    "ggplot2",
    "purrr",
    "stringr",
    "databraryapi",
    "broom",
    "knitr",
    "rmarkdown",
    "cowplot",
    "lubridate"
  )
)

list(
  tar_target(
    max_ids,
    update_max_vol_party_ids()
  ),
  tar_target(
    max_party_id,
    max_ids$MAX_PARTY_ID
  ),
  tar_target(
    max_vol_id,
    max_ids$MAX_VOL_ID
  ),
  # institution and investigator aggregate numbers
  tar_target(
    inst_invest_df,
    update_inst_invest_df("src/csv")
  ),
  tar_target(
    inst_invest_csv,
    update_inst_invest_csv(inst_invest_df, "src/csv")
  ),
  #----------------------------------------------------------------------------
  # Volume tags and keywords
  tar_target(
    volume_tags_df,
    refresh_volume_tags_df(1:max_vol_id)
  ),
  tar_target(vol_tags_csv,
             update_vol_tags_csv(volume_tags_df, "src/csv")),
  #----------------------------------------------------------------------------
  # Funders
  tar_target(
    volume_funders_df,
    refresh_volume_funders_df(1:max_vol_id)
  ),
  tar_target(
    volume_funders_csv,
    update_volume_funders_csv(volume_funders_df, "src/csv")
  ),
  #----------------------------------------------------------------------------
  # Volume assets
  tar_target(
    volume_asset_csv_list,
    generate_volume_asset_csv_list("src/csv"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    volume_asset_stats_csvs,
    update_all_vol_stats(max_vol_id)
  ),
  tar_target(
    volume_asset_stats_df,
    make_volume_assets_stats_df(volume_asset_csv_list)
  ),
  #----------------------------------------------------------------------------
  # Volume demographics from spreadsheets
  tar_target(
    volume_ss_csvs,
    get_volume_demo_save_csv_mult(1, max_vol_id)
  ),
  tar_target(
    volume_owners_csv,
    get_all_owners_save_csvs(max_vol_id)
  ),
  tar_target(
    volume_ss_csv_fl,
    list.files('src/csv', "[0-9]+\\-sess\\-materials\\.csv", full.names = TRUE),
    cue = tar_cue(mode = "always")
  ),
  tar_target(volume_demog_df,
             create_complete_demog_df(volume_ss_csv_fl)),
  #----------------------------------------------------------------------------
  # Institutions and investigators (detailed)
  #
  # tar_target(
  #   update_all_inst_csvs,
  #   get_save_many_inst_csvs(1, max_party_id, update_geo = TRUE)
  # ),
  tar_target(
    inst_df,
    make_inst_df_from_csvs(n_inst_csvs),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    n_inst_csvs,
    length(list.files('src/csv', "inst\\-[0-9]+\\.csv"))
  ),
  tar_target(
    add_new_inst_csvs,
    get_save_many_inst_csvs(max(extract_inst_csv_id()), max_party_id, update_geo = TRUE)
  ),
  # tar_target(
  #   invest_df,
  #   readr::read_csv('src/csv/all-ais.csv', show_col_types = FALSE)
  # ),
  tar_target(invest_df,
             make_ais_df(inst_df)),
  # tar_target(
  #   institution_csv_fl,
  #   list.files("src/csv", "inst\\-[0-9]+\\.csv", full.names = TRUE)
  # ),
  tar_target(make_all_ais_csv,
             update_invest_csv(inst_df)),
  
  # Volume-level sessions
  tar_target(
    vols_sess_df,
    get_many_volumes_data(1, max_vol_id)
  )
)
