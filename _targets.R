# _targets.R

library(targets)
library(tarchetypes)

source("R/functions.R")
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
  tar_target(max_vol_id,
             1520),
  tar_target(max_party_id,
             10868),
  # institution and investigator aggregate numbers
  tar_target(
    inst_invest_df,
    update_inst_invest_df("src/csv"),
    cue = tarchetypes::tar_cue_age(name = inst_invest_df,
                                   age = as.difftime(1, units = "weeks"))
  ),
  tar_target(
    inst_invest_csv,
    update_inst_invest_csv(inst_invest_df, "src/csv")
  ),
  # Volume tags and keywords
  tar_target(
    volume_tags_df,
    refresh_volume_tags_df(1:max_vol_id),
    cue = tarchetypes::tar_cue_age(name = volume_tags_df,
                                   age = as.difftime(13, units = "weeks"))
  ),
  tar_target(vol_tags_csv,
             update_vol_tags_csv(volume_tags_df, "src/csv")),
  # Funders
  tar_target(
    volume_funders_df,
    refresh_volume_funders_df(1:max_vol_id),
    cue = tarchetypes::tar_cue_age(name = volume_funders_df,
                                   age = as.difftime(13, units = "weeks"))
  ),
  tar_target(
    volume_funders_csv,
    update_volume_funders_csv(volume_funders_df, "src/csv")
  ),
  # Volume assets
  tar_target(
    volume_asset_csv_list,
    generate_volume_asset_csv_list("src/csv")
  ),
  tar_target(
    volume_asset_stats_csvs,
    update_all_vol_stats(max_vol_id),
    cue = tarchetypes::tar_cue_age(name = volume_asset_stats_csvs,
                                   age = as.difftime(13, units = "weeks"))
  ),
  tar_target(volume_asset_stats_df,
             make_volume_assets_stats_df(volume_asset_csv_list)),
  # Volume demographics from spreadsheets
  tar_target(
    volume_ss_csvs,
    get_volume_demo_save_csv_mult(1, max_vol_id),
    cue = tarchetypes::tar_cue_age(name = volume_ss_csvs,
                                   age = as.difftime(13, units = "weeks"))
  ),
  tar_target(
    volume_owners_csv,
    get_all_owners_save_csvs(max_vol_id),
    cue = tarchetypes::tar_cue_age(name = volume_owners_csv,
                                   age = as.difftime(13, units = "weeks"))
  ),
  tar_target(volume_ss_csv_fl,
             list.files('src/csv', "[0-9]+\\-sess\\-materials\\.csv", full.names = TRUE)),
  tar_target(volume_demog_df,
             create_complete_demog_df(volume_ss_csv_fl)),
  # Institutions and investigators (detailed)
  tar_target(make_inst_csvs,
             get_save_many_inst_csvs(10868, max_party_id, update_geo = TRUE)),
  tar_target(invest_df, 
             readr::read_csv('src/csv/all-ais.csv', show_col_types = FALSE)),
  tar_target(institution_csv_fl,
             list.files("src/csv", "inst\\-[0-9]+\\.csv", full.names = TRUE)),
  tar_target(make_all_ais_csv, 
             update_invest_csv(inst_df)),
  #--- Skip party 2 (staff)
  tar_target(inst_df,
             load_inst_df_from_csvs(institution_csv_fl[2:length(institution_csv_fl)]),
            ),
  # Volume-level sessions
  tar_target(vols_sess_df,
             get_many_volumes_data(1, max_vol_id),
             cue = tarchetypes::tar_cue_age(name = vols_sess_df,
                                            age = as.difftime(13, units = "weeks"))
  )
)
