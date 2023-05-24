# databrary-analytics

Code to produce and plot analytics about Databrary.

## Set-up

There are a number of components to set up for before you are able to render the full set of reports.

### `renv` dependencies

- Install `renv` via `install.packages('renv')`.
- Run `renv::activate()` at the R console.
- Run `renv::repair()`, if prompted to do so. This will install any packages that are needed.

### GitHub Access

- Use `usethis::create_github_token()` to create a GitHub access token and `gitcreds::gitcreds_set()` to set it for the local environment. Running `usethis::git_sitrep()` is a useful diagnostic. See <https://usethis.r-lib.org/articles/git-credentials.html>

### Databrary Access

- The `databraryapi` package should be installed when you activate `renv`. Test that by running `library(databraryapi)` at the R console.
    - If needed, install the `databraryapi` package via `devtools::install_github("PLAY-behaviorome/databraryapi")`.
- Run `databraryapi::config_passwd()` to create a local keyring store associated with your Databrary user name (email) and password.
- Test that your password works by running `databraryapi::login_db("YOUR_DATABRARY_ID")` substituting your Databrary ID (email). This should return `TRUE`.

### Environment variables

- Create an `.Renviron` file in your root directory using `usethis::edit_r_environ('user')`.
- Add a line to the `.Renviron` file as follows: `DATABRARY_LOGIN="YOUR_DATABRARY_ID"`, substituting your actual Databrary login ID (not password!).
- You may also want to add a Google maps API key (if you have one) as follows: `GGMAP_GOOGLE_API_KEY=your_google_api_key`.

### Check credentials

- Run `source("R/starting_fresh_functions.R")`, then run `check_all_credentials("YOUR_DATABRARY_ID")` to verify that your Databrary and Google maps credentials have been properly stored.

### Generate 'fresh' CSVs

- It's a good idea to generate some of the CSV dependencies anew when you first clone the repo. **NOTE**: The following can take some time, so make sure to start this when you have time to let it complete.
- Run `generate_fresh_inst_csvs()` to generate a new CSV file with all of the Databrary institutions.
- Run `generate_fresh_asset_stats_csvs()` to generate new CSVs about each volume's assets.

### Use `targets` to generate other report 'inputs'

- Run `targets::tar_make()` to generate/update other files that support the set of reports. This can take awhile.

### Render the reports

- At this point, you should be able to render the full set of reports. This does not take very much time.
- Run `bookdown::render_book('src')`.
- If this completes successfully, you can view the rendered site in `docs/`.
