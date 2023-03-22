# databrary-analytics

Code to produce and plot analytics about Databrary.

## GitHub Access

- Use `usethis::create_github_token()` to create a GitHub access token and `gitcreds::gitcreds_set()` to set it for the local environment. Running `usethis::git_sitrep()` is a useful diagnostic. See <https://usethis.r-lib.org/articles/git-credentials.html>


## Databrary Access

- Install the `databraryapi` package via `devtools::install_github("PLAY-behaviorome/databraryapi")`.
- Run `databraryapi::config_passwd()` to create a local keyring store associated with your Databrary user name and password.
- Test that your password works by running `databraryapi::login_db("YOUR_DATABRARY_ID")` substituting your Databrary ID (email). This should return `TRUE`.

## Environment variables

- Create an `.Renviron` file in your root directory using `usethis::edit_r_environ('user')`.
