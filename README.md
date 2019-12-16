# databrary-analytics
Code to produce and plot analytics about Databrary

*- `visits-downloads.Rmd` produces plots and tables from a March 2018 analysis of unique visits and downloads of the site.*   
    *- `visit-downloads.md` shows the output.*  
*- `databrary-user-growth.Rmd` produces a plot of the growth in authorized users and institutions.*   
*- `databrary_monthly.Rmd` produces a combined report on Databrary. An [HTML](https://gilmore-lab.github.io/databrary-analytics/databrary_monthly.html) version is also available.*   
- `databrary_weekly_rpt.Rmd` produces a combined report about Databrary. It is run approximately weekly. An [HTML](https://gilmore-lab.github.io/databrary-analytics/databrary_weekly_rpt.html) version is available at the link.  

## Weekly report

- To run the report:  
    - `source('render_weekly_report.R')`  
    - `render_weekly_report(db_account = '<YOUREMAIL@YOURDOMAIN>')`  
        - where `YOUREMAIL@YOURDOMAIN` is replaced by your Databrary account.  

# Citation Data

- These numbers are from the Databrary Actitity site https://nyu.databrary.org/api/activity  

- "Databrary" and "Datavyu" (with quotes) are used to reduce bad hits in [Google Scholar](https://scholar.google.com)      
    - choose the 'anytime' option for Citations Monthly  
    - choose 'Since 2019' option for Citations Yearly  
    - the boxes for *Include Patents* and *Include Citations* remain checked  

# Shared volumes and owners report

*- [Report](https://gilmore-lab.github.io/databrary-analytics/shared-volumes-sessions.html) about full shared volumes and volume overviews only with session statistics.*  
    *- To run this report run  *  
        *-`rmarkdown::render("list-shared-volumes-sessions.Rmd", params = list(databrary_login = <YOURDATABRARYLOGIN))`  *
            *- replacing `<YOURDATABRARYLOGIN>` with your actual Databrary login (email).*  
*- [Report](https://gilmore-lab.github.io/databrary-analytics/list-shared-volumes-owners.html) with a summary of shared volumes but no session data.  *
    *- To run this report run `rmarkdown::render("list-shared-volumes-owners.Rmd", params = list(databrary_login = <YOURDATABRARYLOGIN))`  *
        *- replacing `<YOURDATABRARYLOGIN>` with your actual Databrary login (email).*  