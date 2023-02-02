# Generating Databrary site map

0. Resources

The `databrary-analytics` repository on GitHub:

<https://github.com/gilmore-lab/databrary-analytics>

1. Updating authorized institutions

When the "investigators.Rmd" report gets rendered via `render_institutions_report(max_party_id = max_party_id)` using the `R/generate_reports.R` script, an `update_ins_csv()` function gets run. The code for this function is in `institions-investigators/helpers.R`. If the `update_geo` flag is set to TRUE, then the `get_inst_info()` function is run for new institutions and their geographic coordinates are retrieved using the `update_inst_lat_lon()` function. This function relies on the `databraryapi` and `ggmap` package functions. 

2. Updating JSON files

Databrary copies/sources its map markers file from `https://gitcdn.link/repo/gilmore-lab/databrary-analytics/master/institutions-investigators/js/institutions.js`. 

To generate/update this file:

- Load `institutions.csv` as a data frame.
- run `export_cleaned_inst_json_from_csv()` with the default values.

**N.B.** This should be added to the R Markdown report process.
