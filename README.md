# databrary-analytics
Code to produce and plot analytics about Databrary

*- `visits-downloads.Rmd` produces plots and tables from a March 2018 analysis of unique visits and downloads of the site.*   
    *- `visit-downloads.md` shows the output.*   
*- `databrary-user-growth.Rmd` produces a plot of the growth in authorized users and institutions.*   
*- `databrary_monthly.Rmd` produces a combined report on Databrary. An [HTML](https://gilmore-lab.github.io/databrary-analytics/databrary_monthly.html) version is also available.*   

## Weekly report

[Current report](https://gilmore-lab.github.io/databrary-analytics/weekly/databrary_weekly_report.html) about Databrary Institutions & Authorized Users, Volumes, and Citations.

- To update the report:  
    - `source('weekly/R/helpers.R')`  
    - `update_weekly_report(db_account = "<YOUREMAIL@YOURDOMAIN>")`  
        - replacing `<YOUREMAIL@YOURDOMAIN>` with your actual Databrary login (email). 

- To run the report without updating:
    - `source('weekly/R/helpers.R')`  
    - `render_weekly_report(db_account = "<YOUREMAIL@YOURDOMAIN>")`  
        - replacing `<YOUREMAIL@YOURDOMAIN>` with your actual Databrary login (email). 

# Citation Data

- These numbers are from the Databrary Actitity site https://nyu.databrary.org/api/activity  

- "Databrary" and "Datavyu" (with quotes) are used to reduce bad hits in [Google Scholar](https://scholar.google.com)      
    - choose the 'anytime' option for Citations Monthly  
    - choose 'Since 2019' option for Citations Yearly  
    - the boxes for *Include Patents* and *Include Citations* remain unchecked  

# Shared volumes and owners report

- [Report](https://gilmore-lab.github.io/databrary-analytics/shared-volumes-sessions/shared-volumes-sessions.html) about full shared volumes and volume overviews only with session statistics.* 
    - To run this report run  
        - `rmarkdown::render("shared-volumes-sessions/shared-volumes-sessions.Rmd", params = list(db_login = "<YOUREMAIL@YOURDOMAIN>"))`  
        - replacing `<YOUREMAIL@YOURDOMAIN>` with your actual Databrary login (email).
        
# Tags and keywords report

- [Report](https://gilmore-lab.github.io/databrary-analytics/tags-keywords/tags-keywords-report.html) on the tags and keywords linked to Databrary volumes.
    - To run this report, run `source("tags-keywords/R/helpers.R")` from the R console.
    - Then run `render_tags_keywords_report("<YOUREMAIL@YOURDOMAIN>")`  
        - replacing `<YOUREMAIL@YOURDOMAIN>` with your actual Databrary login (email). 

# Institutions and investigators report

- [Report](https://gilmore-lab.github.io/databrary-analytics/institutions-investigators/institutions-investigators.html) on number of investigators at each authorizing institution.
    - To run this report, run `source("institutions-investigators/R/helpers.R")` from the R console.
    - Then run `render_institutions_investigators_report("<YOUREMAIL@YOURDOMAIN>")  
        - replacing `<YOUREMAIL@YOURDOMAIN>` with your actual Databrary login (email). 

# Participant demographics report

- [Report](https://gilmore-lab.github.io/databrary-analytics/participant-demographics/participant-demog-report.html) on the site-wide reported participant demographics.
    - To run this report, run `source("participant-demographics/R/helpers.R")` from the R console.
    - Then run `rmarkdown::render("participant-demographics/participant-demog-report.Rmd", params = list(db_login="<YOUREMAIL@YOURDOMAIN>"))`  
        - replacing `<YOUREMAIL@YOURDOMAIN>` with your actual Databrary login (email).
        
# Funders

- [Report](https://gilmore-lab.github.io/databrary-analytics/funders/funder-report.html) on the number of shared volumes by funding source.
    - Then run `rmarkdown::render("funders/funder-report.Rmd", params = list(db_login="<YOUREMAIL@YOURDOMAIN>"))`  
        - replacing `<YOUREMAIL@YOURDOMAIN>` with your actual Databrary login (email).
