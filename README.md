# databrary-analytics
Code to produce and plot analytics about Databrary

- `visits-downloads.Rmd` produces plots and tables from a March 2018 analysis of unique visits and downloads of the site. `visit-downloads.md` shows the output.
- `databrary-user-growth.Rmd` produces a plot of the growth in authorized users and institutions.
- `databrary_monthly.Rmd` produces a combined report on Databrary. An [HTML](https://gilmore-lab.github.io/databrary-analytics/databrary_monthly.html) version is also available.
- `databrary_weekly_rpt.Rmd` produces a combined report about Databrary. It is run approximately weekly. An [HTML](https://gilmore-lab.github.io/databrary-analytics/databrary_weekly_rpt.html) version is available at the link.

## Weekly report

- To run the report, update the data, and write the new data to the accumulated dataset hosted on Google Sheets (https://docs.google.com/spreadsheets/d/1tvlIQzULrMtXo97aJu71ljdTmNXkwwpU9eOOasVer3g/edit#gid=1355057375), `rmarkdown::render('databrary_weekly_rpt.Rmd', params = list(db_account = YOUREMAIL@YOURDOMAIN), update_gs = 'true', update_stats = 'true')` at the R console. Replace `YOUREMAIL@YOURDOMAIN` with your Databrary account ID (email). Note that by default, `update_gs` and `update_stats` are 'false'. This makes it easier to debug the report.
- It may be easier to run the report using this new procedure:
    - `source("R/run_db_weekly.R")
    - `run_db_weekly(db_account = <YOUREMAIL@YOURDOMAIN>)`, where `YOUREMAIL@YOURDOMAIN` is replaced by your Databrary account.

# Data acquired monthly

- These numbers are from the Databrary Actitity site https://nyu.databrary.org/api/activity

- "Databrary" and "Datavyu" (with quotes) are used to reduce bad hits in [Google Scholar](https://scholar.google.com)     
    - choose the 'anytime' option for Citations Monthly
    - choose 'Since 2019' option for Citations Yearly
    - the boxes for *Include Patents* and *Include Citations* remain checked

# Shared volumes and owners report

- [Report](https://gilmore-lab.github.io/databrary-analytics/shared-volumes-sessions.html) about full shared volumes and volume overviews only with session statistics.
- <https://gilmore-lab.github.io/databrary-analytics/list-shared-volumes-owners.html>
    - To run this report run `rmarkdown::render("list-shared-volumes-owners.Rmd", params = list(databrary_login = <YOURDATABRARYLOGIN))` replacing `<YOURDATABRARYLOGIN>` with your actual Databrary login (email).