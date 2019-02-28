Databrary Report
================
Rick O. Gilmore
2019-02-28 16:12:32

Institutions & Authorized Users
===============================

At the moment, the `old_stats` come from a CSV stored locally. Once we have API commands to upload new data, the `old_stats` can come from Databrary directly.

``` r
new_stats <- databraryapi::get_db_stats()

old_stats <- read_csv("csv/institutionAuthCounts.csv")

# initialize updated_stats
updated_stats <- old_stats
next_entry <- dim(updated_stats)[1] + 1
updated_stats[next_entry,] = NA

# fill with new data
updated_stats$YearMonth[next_entry] <- new_stats$date
updated_stats$Institutions[next_entry] <- new_stats$institutions
updated_stats$`Authorized Investigators`[next_entry] <- new_stats$investigators

# hate the variable names
updated_stats <- 
  updated_stats %>%
  rename(date = YearMonth, institutions = Institutions, investigators = `Authorized Investigators`)
```

``` r
updated_stats <- updated_stats %>%
  gather(., key = "type", value = "count", -date)

# Plot
p <- updated_stats %>%
  ggplot(., aes(x = date, y = count, color = type, group = type)) +
  geom_point() + 
  geom_line(size=3) +
  theme_classic(base_size = 14) +                        #Axis Label Size
  scale_colour_manual(values=c("#ec7751", "#4CAE99")) +  #Saturated Databrary Colors 
  ylab("Authorizations") +
  #xlab("Year") +
  #labs(title="Databrary User Growth") +
  theme(axis.title.x=element_blank()) +
  theme(legend.position="none", axis.text = element_text(size = rel(0.8), colour = "black")) +    #Axis text size and color
  theme(axis.line = element_blank()) +
  scale_y_continuous(breaks = seq(0, 900, 100), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 900))
  
ggdraw(p) + 
  draw_label("Investigators", colour = "#4CAE99", .9, .73) +
  draw_label("Institutions", colour = "#ec7751", .9, .38)
```

![](databrary_weekly_files/figure-markdown_github/db-inst-user-plot-1.png)

Volumes
=======

The volumes data can also be rewritten back to Databrary at some future date.

``` r
old_vols <- read_csv("csv/db-volumes-monthly.csv")

# Update
updated_vols <- old_vols
next_entry <- dim(updated_vols)[1] + 1
updated_vols[next_entry,] = NA

updated_vols$date[next_entry] <- new_stats$date
updated_vols$shared_volumes[next_entry] <- new_stats$datasets.shared
updated_vols$unshared_volumes[next_entry] <- 
  new_stats$datasets.total - new_stats$datasets.shared
```

``` r
updated_vols <- updated_vols %>%
  gather(., key = "type", value = "count", -date)

# Plot
vols_plot <- updated_vols %>%
  ggplot(., aes(x = date, y = count, color = type, group = type)) +
  geom_point() + 
  geom_line(size=3) +
  theme_classic(base_size = 14) +                        #Axis Label Size
  scale_colour_manual(values=c("#ec7751", "#4CAE99")) +  #Saturated Databrary Colors 
  ylab("Volumes") +
  #xlab("Year") +
  #labs(title="Databrary Citation Growth") +
  theme(axis.title.x=element_blank()) +
  theme(legend.position="none", axis.text = element_text(size = rel(0.8), colour = "black")) +    #Axis text size and color
  theme(axis.line = element_blank()) +
  scale_y_continuous(breaks = seq(0, 500, 100), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 500))
  
  
ggdraw(vols_plot) + 
  draw_label("Unshared", colour = "#4CAE99", .86, .88) +
  draw_label("Shared", colour = "#ec7751", .84, .65)
```

![](databrary_weekly_files/figure-markdown_github/db-vols-plot-1.png)

Citation counts
===============

This section will eventually update and show the citation counts for Databrary and Datavyu. For now, it just shows the stored data file that we update manually. This can be automated, we just have to write a function to parse the HTML returned from Google Scholar to pull the relevant data.

<!-- Here we plot all Databrary and Datavyu Citations vs. date. I used https://www.hexcolortool.com to saturate the Databrary colors ("#fadbc7", "#b2ddd4") so they are darker on the graph. -->
``` r
citations <- read_csv("csv/citations-monthly.csv")

citations <- citations %>%
  gather(., key = "type", value = "count", -date)

# Plot
citations_plot <- 
  citations %>%
  ggplot(., aes(x = date, y = count, color = type, group = type)) +
  geom_point() + 
  geom_line(size=3) +
  theme_classic(base_size = 14) +                        #Axis Label Size
  scale_colour_manual(values=c("#ec7751", "#4CAE99")) +  #Saturated Databrary Colors 
  ylab("Citations") +
  #xlab("Year") +
  #labs(title="Databrary Citation Growth") +
  theme(axis.title.x=element_blank()) +
  theme(legend.position="none", axis.text = element_text(size = rel(0.8), colour = "black")) +    #Axis text size and color
  theme(axis.line = element_blank()) +
  scale_y_continuous(breaks = seq(0, 250, 50), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 250))
  
ggdraw(citations_plot) + 
  draw_label("Datavyu", colour = "#4CAE99", .9, .6) +
  draw_label("Databrary", colour = "#ec7751", .7, .85)
```

![](databrary_weekly_files/figure-markdown_github/db-dv-citations-plot-1.png)
