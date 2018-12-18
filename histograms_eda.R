# HISTOGRAM OF RETURN TIME AROUND ANOMALIES 

library(tidyverse)
library(ggplot2)
library(scales)
library(anytime)
library(knitr)
library(ggridges)
library(RColorBrewer)


#Create function to generate daily histograms of return time for several (e.g. +/-3) days around identified anomalies
#TODO: Need to revise histogram functions to update to the plot titles to specify specific country in each data set. I currently manually adjusted each title.

#Function with ability to break out by project, browser_family and os_family.
plot_return_histograms_breakdown <- function(date_var,df,fill_var) #where x is date and y is the breakout group
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)", fill = fill_var), 
             stat = "identity") +
    scale_y_continuous("Returns each day", labels = percent,  limits = c(0 , 0.20)) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
    labs(title = paste("Days until next access for", date_var, "\n on desktop on all Wikis by",fill_var))  +
    wmf::theme_min() +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(plot.subtitle=element_text(hjust=0.5))
  
  ggsave(filename=paste0(fill_var,"_return_histogram",date_var,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = 192, height = 6, width = 10, limitsize = FALSE)  
  
}

#Function to generate daily return histograms without breakdown
plot_return_histograms <- function(date_var,df) #where date_var is the date
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)"), 
             stat = "identity", fill = "dark blue") +
    scale_y_continuous("Returns each day", labels = percent, limits = c(0 , 0.20)) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1)) +
    labs(title = paste("Days until next access for", date_var, "\n  on desktop on all Wikis"))  +
    wmf::theme_min() +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(plot.subtitle=element_text(hjust=0.5))
  
  ggsave(filename=paste0("return_histogram",date_var,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = 192, height = 6, width = 10, limitsize = FALSE)  
  
}

#Function to generate histograms with main view page breakdown

plot_return_histograms_mainpages <- function(date_var,df,fill_var) #where x is date and y is the breakout group
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)", fill = fill_var), 
             stat = "identity") +
    scale_y_continuous("Returns each day", labels = percent) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
    labs(title = paste("Days until next access for", date_var, "\n on desktop on all Wikipedia Projects broken down by main page views"),
         caption = "Main page views also include pages where page title was not extracted")  +
    wmf::theme_min() +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(plot.subtitle=element_text(hjust=0.5))
  
  ggsave(filename=paste0(fill_var,"_return_histogram",date_var,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  
}


#In Germany (DE), there was a spike in avg user returns within 31 days on desktop on 2018-03-01 (rose from an avg of 5 to 8.58)

fig_path <- file.path("figures/return_histograms/march_2018_de")

return_frequency_Mar18 <- rbind(readr::read_rds("data/de_return_frequency_Mar18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2018-02-26"), as.Date("2018-03-04"), by=1), plot_return_histograms, df =return_frequency_Mar18, fill_var = "project")
lapply(seq(as.Date("2018-02-26"), as.Date("2018-03-04"), by=1), plot_return_histograms, df =return_frequency_Mar18, fill_var = "os_family")
lapply(seq(as.Date("2018-02-26"), as.Date("2018-03-04"), by=1), plot_return_histograms, df =return_frequency_Mar18, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
library(magick)
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/march_2018_de/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/march_2018_de/gif")


lapply(seq(as.Date("2018-02-26"), as.Date("2018-03-04"), by=1), plot_return_histograms, df =return_frequency_Mar18)

# convert the .png files to one .gif file using ImageMagick.
system("convert -delay 80 *.png march_2018_de.gif")



#Look at non-peak time period in US July 2017 to compare differences in daily returns.

fig_path <- file.path("figures/return_histograms/july_2017_us")

return_frequency_Jul17 <- rbind(readr::read_rds("data/us_return_frequency_July17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms_breakdown, df =return_frequency_Jul17, fill_var = "project")
lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms_breakdown, df =return_frequency_Jul17, fill_var = "os_family")
lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms_breakdown, df =return_frequency_Jul17, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/july_2017_us/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/july_2017_us/gif")

lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms, df =return_frequency_Jul17)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png july_2017_us.gif")




#In Spain (ES), there was a spike in avg user returns within 31 days on desktop on 2017-12-21 (rose from an avg of 6 to 8.9)

fig_path <- file.path("figures/return_histograms/dec_2017_es")

es_return_frequency_Dec17 <- rbind(readr::read_rds("data/es_return_frequency_dec17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#Create daily histograms of returns with breakdown
#by browser family
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/browser_family")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/browser_family")


lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =es_return_frequency_Dec17, fill_var = "browser_family")
#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png dec_2017_es_bybrowser.gif")

#by os family
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/os_family")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/os_family")


lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =es_return_frequency_Dec17, fill_var = "os_family")
#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png dec_2017_es_byos.gif")



#by project
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/project")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/project")

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =es_return_frequency_Dec17, fill_var = "project")

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png dec_2017_es_byproject.gif")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/total")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_es/total")

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =es_return_frequency_Dec17)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png dec_2017_es.gif")



#In France (FR), there was also a spike in avg user returns within 31 days on desktop on 2017-12-21 (rose from an avg of around 6 to 8.2)

fig_path <- file.path("figures/return_histograms/dec_2017_fr")

fr_return_frequency_Dec17 <- rbind(readr::read_rds("data/fr_return_frequency_dec17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =fr_return_frequency_Dec17, fill_var = "project")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =fr_return_frequency_Dec17, fill_var = "os_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =fr_return_frequency_Dec17, fill_var = "browser_family")

#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_fr/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis//figures/return_histograms/dec_2017_fr/gif")

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =fr_return_frequency_Dec17)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png dec_2017_fr.gif")



#In the United States, there was a spike in avg user returns within 31 days on 2017-12-21 (rose from an avg of around 5.5 to 7.4)

fig_path <- file.path("figures/return_histograms/dec_2017_us")

us_return_frequency_Dec17 <- rbind(readr::read_rds("data/us_return_frequency_dec17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =us_return_frequency_Dec17, fill_var = "project")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =us_return_frequency_Dec17, fill_var = "os_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms_breakdown, df =us_return_frequency_Dec17, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_us/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/dec_2017_us/gif")

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =us_return_frequency_Dec17)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png dec_2017_usv2.gif")




#On Wikisource, there is a drop on 2017-07-12 to 2.4 avg returns within 31 days on desktop from all countries

fig_path <- file.path("figures/return_histograms/sept_2017_wikisource")

wikisource_return_frequency_Sept17 <- rbind(readr::read_rds("data/wikisource_return_frequency_Sept17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#project class break down not needed since data only includes Wikisource data 
lapply(seq(as.Date("2017-09-05"), as.Date("2017-09-11"), by=1), plot_return_histograms_breakdown, df =wikisource_return_frequency_Sept17, fill_var = "os_family")
lapply(seq(as.Date("2017-09-05"), as.Date("2017-09-11"), by=1), plot_return_histograms_breakdown, df =wikisource_return_frequency_Sept17, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/sept_2017_wikisource/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/sept_2017_wikisource/gif")

lapply(seq(as.Date("2017-09-05"), as.Date("2017-09-11"), by=1), plot_return_histograms, df =wikisource_return_frequency_Sept17)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png sept_2017_wikisource.gif")



#On Wikisource, there were a number of drops in avg returns within 31 days on desktop from all countries in July 2017. 
#Plot Histograms around drop 2017-07-12 to 2.0 avg returns and 2017-07-12 to 1.0 avg returns.  

fig_path <- file.path("figures/return_histograms/july_2017_wikisource")

wikisource_return_frequency_Jul17 <- rbind(readr::read_rds("data/wikisource_return_frequency_Jul17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#project class break down not needed since data only includes Wikisource data 
lapply(seq(as.Date("2017-07-09"), as.Date("2017-07-15"), by=1), plot_return_histograms_breakdown, df =wikisource_return_frequency_Jul17, fill_var = "os_family")
lapply(seq(as.Date("2017-07-09"), as.Date("2017-07-15"), by=1), plot_return_histograms_breakdown, df =wikisource_return_frequency_Jul17, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/july_2017_wikisource/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/july_2017_wikisource/gif")

lapply(seq(as.Date("2017-07-09"), as.Date("2017-07-15"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul17)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png july_2017_wikisource.gif")




#There was a also a recent drop on 2018-07-18 from an average of around 6/7 to 3.63 on Wikisource from all countries

fig_path <- file.path("figures/return_histograms/july_2018_wikisource")

wikisource_return_frequency_Jul18 <- rbind(readr::read_rds("data/wikisource_return_frequency_Jul18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Android", "iOS", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other'))

#project class break down not needed since data only includes Wikisource data 
lapply(seq(as.Date("2018-07-15"), as.Date("2018-07-21"), by=1), plot_return_histograms_breakdown, df =wikisource_return_frequency_Jul18, fill_var = "os_family")
lapply(seq(as.Date("2018-07-15"), as.Date("2018-07-21"), by=1), plot_return_histograms_breakdown, df =wikisource_return_frequency_Jul18, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/july_2018_wikisource/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/july_2018_wikisource/gif")


lapply(seq(as.Date("2018-07-15"), as.Date("2018-07-21"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul18)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png july_2018_wikisource.gif")



#On Wikidata, there was a recent drop on 2018-06-30 from an average of around 7.5 to 1.69 on desktop

fig_path <- file.path("figures/return_histograms/june_2018_wikidata")

wikidata_return_frequency_Jun18 <- rbind(readr::read_rds("data/wikidata_return_frequency_Jun18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Android", "iOS", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#project class break down not needed since data only includes Wikidata  
lapply(seq(as.Date("2018-06-27"), as.Date("2018-07-03"), by=1), plot_return_histograms_breakdown, df =wikidata_return_frequency_Jun18, fill_var = "os_family")
lapply(seq(as.Date("2018-06-27"), as.Date("2018-07-03"), by=1), plot_return_histograms_breakdown, df =wikidata_return_frequency_Jun18, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_wikidata/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_wikidata/gif")

lapply(seq(as.Date("2018-06-27"), as.Date("2018-07-03"), by=1), plot_return_histograms, df =wikidata_return_frequency_Jun18)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png june_2018_wikidata.gif")


#Redid query to add break out to main pages using available webrequest table data from August 2018. 
# Data queries from mneisler.last_access_main_page_views_by_country
# Below queries only looked at wikipedia projects as that's where the spikes in Aug 2018 occured. 


#Bangladesh spike to a peak of 7.9 on 2018-08-16 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_bd")

bd_return_frequency_Aug18 <- rbind(readr::read_rds("data/bd_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari', 'UC Browser'), browser_family, 'Other')) 


#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms_mainpages, df =bd_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms_breakdown, df =bd_return_frequency_Aug18 , fill_var = "os_family")
lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms_breakdown, df =bd_return_frequency_Aug18 , fill_var = "browser_family")



#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_bd/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_bd/gif")

lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_bd.gif")




#Bangladesh spike to a peak of 7.9 on 2018-08-15 on mobile web
fig_path <- file.path("figures/return_histograms/aug_2018_bd_mw")

bd_return_frequency_Aug18_mw <- rbind(readr::read_rds("data/bd_return_frequency_Aug18_mw.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c('Android', 'iOS', 'Windows Phone', 'Windows', 'Linux'), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome Mobile', 'UC Browser', 'Opera Mini', 'Samsung Internet', 'Chrome Mobile WebView',
                                                            'Opera Mobile', 'Android', 'Mobile Safari'), browser_family, 'Other')) 


#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms_mainpages, df =bd_return_frequency_Aug18_mw, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms_breakdown, df =bd_return_frequency_Aug18_mw, fill_var = "os_family")
lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms_breakdown, df =bd_return_frequency_Aug18_mw, fill_var = "browser_family")



#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_bd_mw/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_bd_mw/gif")


lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18_mw)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_bd_mw.gif")



#Indonesia spike to a peak of 8.3 on 2018-08-07 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_id")

id_return_frequency_Aug18 <- rbind(readr::read_rds("data/id_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms_mainpages, df =id_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms_breakdown, df =id_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms_breakdown, df =id_return_frequency_Aug18 , fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_id/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_id/gif")


lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms, df =id_return_frequency_Aug18)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_id.gif")




#Indonesia spike to a peak of 8.3 on 2018-08-09 on mobile web
fig_path <- file.path("figures/return_histograms/aug_2018_id_mw")

id_return_frequency_Aug18_mw <- rbind(readr::read_rds("data/id_return_frequency_Aug18_mw.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c('Android', 'iOS', 'Windows CE', 'Windows Phone', 'Windows', 'BlackBerry OS', 'Linux'), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome Mobile', 'UC Browser', 'Samsung Internet', 'Chrome Mobile WebView', 'Mobile Safari',
                                                            'Opera Mobile', 'Chrome', 'Android', 'Opera Mini'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms_mainpages, df =id_return_frequency_Aug18_mw, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms_breakdown, df =id_return_frequency_Aug18_mw, fill_var = "os_family")
lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms_breakdown, df =id_return_frequency_Aug18_mw, fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_id_mw/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_id_mw/gif")

lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms, df =id_return_frequency_Aug18_mw)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_id_mw.gif")




#France spike to a peak of 6.5 on 2018-08-03 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_fr")

fr_return_frequency_Aug18 <- rbind(readr::read_rds("data/fr_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms_mainpages, df =fr_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms_breakdown, df =fr_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms_breakdown, df =fr_return_frequency_Aug18 , fill_var = "browser_family")


#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_fr/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_fr/gif")

lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms, df =fr_return_frequency_Aug18)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_fr.gif")


plot_return_histograms_breakdown <- function(date_var,df,fill_var) #where x is date and y is the breakout group
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)", fill = fill_var), 
             stat = "identity") +
    scale_y_continuous("Returns each day", labels = percent,  limits = c(0 , 0.20)) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
    labs(title = paste("Days until next access for", date_var, "\n on desktop on all Wikis by",fill_var))  +
    wmf::theme_min() +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(plot.subtitle=element_text(hjust=0.5))
  
  ggsave(filename=paste0(fill_var,"_return_histogram",date_var,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = 192, height = 6, width = 10, limitsize = FALSE)  
  
}

#India spike to a peak of 6.5 on 2018-08-14 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_in")

in_return_frequency_Aug18 <- rbind(readr::read_rds("data/in_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms_mainpages, df =in_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms_breakdown, df =in_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms_breakdown, df =in_return_frequency_Aug18 , fill_var = "browser_family")



#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_in/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_in/gif")

lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms, df =in_return_frequency_Aug18)


#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_in.gif")



#Japan spike to a peak of 6.7 on 2018-08-10 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_jp")

jp_return_frequency_Aug18 <- rbind(readr::read_rds("data/jp_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms_mainpages, df =jp_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms_breakdown, df =jp_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms_breakdown, df =jp_return_frequency_Aug18 , fill_var = "browser_family")



#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_jp/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_jp/gif")

lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =jp_return_frequency_Aug18)

#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_jp.gif")



#Spain spike to a peak of 6.9 on 2018-08-10 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_es")

es_return_frequency_Aug18 <- rbind(readr::read_rds("data/es_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms_mainpages, df =es_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms_breakdown, df =es_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms_breakdown, df =es_return_frequency_Aug18 , fill_var = "browser_family")

#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_es/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/aug_2018_es/gif")

lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =es_return_frequency_Aug18)


#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png aug_2018_es.gif")

#Generate histograms of daily return time around external events
#Page preview rollout on English Wikipedia on April 18, 2018
fig_path <- file.path("figures/return_histograms/apr_2018_enwikipedia")

enwikipedia_return_frequency_Apr18 <- rbind(readr::read_rds("data/en_return_frequency_Apr18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c('Mac OS X', 'Windows 10', 'Windows 7', 'Windows 8.1', 'Linux', 'Ubuntu', 'Windows 8', 'Windows XP'), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari', 'Edge'), browser_family, 'Other')) 


top_os <- enwikipedia_return_frequency_Apr18  %>%
  group_by(browser_family) %>%
  summarise(total = sum(returns_each_day)) %>%
  arrange(desc(total))
#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-04-12"), as.Date("2018-04-22"), by=1), plot_return_histograms_breakdown, df =enwikipedia_return_frequency_Apr18, fill_var = "os_family")
lapply(seq(as.Date("2018-04-12"), as.Date("2018-04-22"), by=1), plot_return_histograms_breakdown, df =enwikipedia_return_frequency_Apr18 , fill_var = "browser_family")

#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/apr_2018_enwikipedia/gif")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/apr_2018_enwikipedia/gif")


lapply(seq(as.Date("2018-04-12"), as.Date("2018-04-22"), by=1), plot_return_histograms, df =enwikipedia_return_frequency_Apr18)


#Create animated gif to show changes in histograms.
# convert the .png files to one .gif file using ImageMagick.

system("convert -delay 80 *.png apr_2018_enwikipedia.gif")


#Wikipedia Zero shutdown on Angola on June 29, 2018

ao_return_frequency_Jun18 <- rbind(readr::read_rds("data/ao_return_frequency_Jun18.rds")) %>%
  filter(access_method == 'mobile web') %>% #Reviewing mobile web to start as that is where the spike occured.
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c('Android', 'Windows', 'iOS', 'Mac OS X', 'Windows Phone'), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome Mobile', 'Chrome', 'Android', 'Samsung Internet', 'Mobile Safari', 'Opera Mini'), browser_family, 'Other')) 

plot_return_histograms_breakdown <- function(date_var,df,fill_var) #where x is date and y is the breakout group
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)", fill = fill_var), 
             stat = "identity") +
    scale_y_continuous("Returns each day", labels = percent, limits = c(0 , 0.40)) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
    labs(title = paste("Days until next access for", date_var, "\n on mobile web from Angola on all Wikipedia projects by",fill_var))  +
    wmf::theme_min() +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(plot.subtitle=element_text(hjust=0.5))
  
  ggsave(filename=paste0(fill_var,"_return_histogram",date_var,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = 192, height = 6, width = 10, limitsize = FALSE)  
  
}



#generate histograms of various dates aroud spike date.
#breakdown by os family
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_ao/os_family")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_ao/os_family")

lapply(seq(as.Date("2018-06-24"), as.Date("2018-07-04"), by=1), plot_return_histograms_breakdown, df =ao_return_frequency_Jun18, fill_var = "os_family")

#Create animated gif to show changes in histograms.
system("convert -delay 80 *.png june_2018_ao_byos.gif")

#breakdown by browser family
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_ao/browser_family")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_ao/browser_family")

lapply(seq(as.Date("2018-06-24"), as.Date("2018-07-04"), by=1), plot_return_histograms_breakdown, df =ao_return_frequency_Jun18 , fill_var = "browser_family")

#Create animated gif to show changes in histograms.
system("convert -delay 80 *.png june_2018_ao_bybrowser.gif")

#Create daily histograms of returns without breakdown
setwd("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_ao/total")
fig_path <- file.path("~/Code/new-reader-retention-metric-analysis/figures/return_histograms/june_2018_ao/total")


lapply(seq(as.Date("2018-06-24"), as.Date("2018-07-04"), by=1), plot_return_histograms, df =ao_return_frequency_Jun18)


#Create animated gif to show changes in histograms.

system("convert -delay 80 *.png june_2018_aov2.gif")


