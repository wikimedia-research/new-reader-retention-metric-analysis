# Generate various histograms and time series for exploring the new reader retention metric

library(tidyverse)
library(ggplot2)
library(scales)
library(anytime)
library(knitr)

#Time series of the average next return time (within 31 days) by country on all Wikipedia projects

fig_path <- file.path("figures/avg_user_returns/31days")
plot_resolution <- 192

user_returns_31days_bycountry <- read.delim("data/user_returns_allwikis_31days_bycountry.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_31days_bycountry$last_seen_date <- as.Date(user_returns_31days_bycountry$last_seen_date, format = "%d-%b-%Y")
#filter out malformed data point on 3.00000 in Japan on December 31, 2018 on mobile web. 
user_returns_31days_bycountry <-  user_returns_31days_bycountry %>% filter(avg_days_till_next_access != 3.000000) 


#Replace country codes with names (currently just replacing countries used in analysis)
user_returns_31days_bycountry$country  <- plyr::mapvalues(
  user_returns_31days_bycountry$country, from=c("BV","IN", "TT", "MT", "TW", "CC", "FK", "NF", "SH", "SJ", "US",
                                               "ES", "DE", "JP", "GB", "FR", "BD", "ID"),
  to=c("Bouvet Island", "India", "Trinidad and Tobago", "Malata", "Taiwan", "Cocos (Keeling) Islands", "Falkland Islands (Malvinas)", 
       "Norfolk Island","Saint Helena", "Svalbard and Jan Mayen", "United States", "Spain", "Germany", "Japan", "United Kingdom", "France", "Bangladesh", "Indonesia"))

#order countries by the max avg days till next access.

user_returns_31days_max <- user_returns_31days_bycountry %>%
  filter(country != "AQ") %>% #remove Antartica as outlier
  group_by(country) %>%
  summarise(max_return_time = max(avg_days_till_next_access))  %>%
  arrange(desc(max_return_time))

#Highest and lowest peaks are seen in the smaller countries

#Create list of countries with the by highest and lowest peak return times over 31 days and countries with large-sized language wikis
lowest_max <- c("Bouvet Island", "India", "Trinidad and Tobago", "Malata", "Taiwan")
highest_max <- c("Cocos (Keeling) Islands", "Falkland Islands (Malvinas)", 
                 "Norfolk Island","Saint Helena", "Svalbard and Jan Mayen")
largewikicountries <- c("United States", "Spain", "Germany", "Japan", "United Kingdom", "France") #based on countries with largest language wikis by size

#Create function to plot average returns within 31 days by country code. 

plot_31day_returns_bycountry <- function(x) {
  
  p <- ggplot(subset(user_returns_31days_bycountry, country == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
    labs(title = paste("Average user returns within 31 days on all Wikipedia projects from", x)) +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename=paste0("user_returns_31days_",x,".png"), plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)
}

#Generate charts for lists of countries of interest
lapply(largewikicountries, plot_31day_returns_bycountry)
  
lapply(lowest_max, plot_31day_returns_bycountry)

lapply(highest_max, plot_31day_returns_bycountry)
  
plot_31day_returns_bycountry("Bangladesh")
plot_31day_returns_bycountry("Indonesia")

# Time series of the average next return time (within 31 days) by project (language) [Only Wikipedia]

user_returns_31days_byproject <- read.delim("data/user_returns_allwikis_31days_byproject.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_31days_byproject$last_seen_date <- as.Date(user_returns_31days_byproject$last_seen_date, format = "%d-%b-%Y")
#filter out malformed data point on 3.00000 in Japan on December 31, 2018 on mobile web. 
user_returns_31days_byproject <-  user_returns_31days_byproject %>% filter(avg_days_till_next_access != 3.000000) 

#Create lists of largest and smallest sized wiki based on Wiki Segmentation database

topsizedwikis <- c('en', 'es', 'de', 'ja', 'fr')
smallsizedwikis <- c("pi", "ti","krc", "ady", "dz")

#Function to plot average returns within 31 days by project (language)

plot_31day_returns_byproject <- function(x) {
  
  p <- ggplot(subset(user_returns_31days_byproject, project == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
    labs(title = paste("Average user returns within 31 days from ",x,".wikipedia", sep = "")) +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename=paste0("user_returns_31days_",x,".wikpedia.png"), plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)
}

lapply(topsizedwikis, plot_31day_returns_byproject)

lapply(smallsizedwikis, plot_31day_returns_byproject)

# Look at various categories of wikis grouped together in same chart.

#Chart of top 5 wikis by size

topsizedwikis_31days <- user_returns_31days_byproject %>%
  filter(project %in% topsizedwikis) 

p <- ggplot(topsizedwikis_31days, aes(x = last_seen_date, y = avg_days_till_next_access, color = project)) +
  geom_line() + 
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user returns within 31 days from top ranked Wikipedias by size",
       subtitle = "Desktop and Mobile Web Combined") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave(filename= "user_returns_31days_topwikipedias.png", plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
rm(p)

#Chart of small project wikis by size 

smallsizedwikis_31days <- user_returns_31days_byproject %>%
  filter(project %in% smallsizedwikis) 

p <- ggplot(smallsizedwikis_31days, aes(x = last_seen_date, y = avg_days_till_next_access, color = project)) +
  geom_line() + 
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user returns within 31 days from lowest ranked Wikipedias by size",
       subtitle = "Desktop and Mobile Web Combined") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave(filename= "user_returns_31days_smallwikipedias.png", plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
rm(p)


# Time series of the average next return time (within 31 days) by project class (Wikipedia, Commons, Wikidata, etc)

user_returns_31days_byprojectclass <- read.delim("data/user_returns_allwikis_31days_byprojectclass.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_31days_byprojectclass$last_seen_date <- as.Date(user_returns_31days_byprojectclass$last_seen_date, format = "%d-%b-%Y")
#filter out malformed data point on 3.00000 in Japan on December 31, 2018 on mobile web. 
user_returns_31days_byprojectclass <-  user_returns_31days_byprojectclass %>% filter(avg_days_till_next_access != 3.000000) 

us_peak <- user_returns_31days_byprojectclass %>%
  filter(project_class == "wikidata",
         last_seen_date >= "2018-06-25" & last_seen_date <= "2018-07-04")  %>%
  arrange(last_seen_date)

#Function to plot average returns within 31 days by project class for all countries and languages. 

project_classes <- c("mediawiki", "wikidata", "wikimedia", "wikipedia", "wikisource", "wikivoyage", "wiktionary", "wikinews")

plot_31day_returns_byprojectclass <- function(x) {
  
  p <- ggplot(subset(user_returns_31days_byprojectclass, project_class == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
    labs(title = paste("Average user returns within 31 days from", x)) +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename=paste0("user_returns_31days_", x, ".png"), plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)
}

lapply(project_classes,plot_31day_returns_byprojectclass)


#Create plot of avg returns within 31 days for all countries on all wikipedia projects. 

user_returns_31days_all <- read.delim("data/user_returns_allwikis_31days_all.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_31days_all$last_seen_date <- as.Date(user_returns_31days_all$last_seen_date, format = "%d-%b-%Y")
#filter out malformed data point on 3.00000 in Japan on December 31, 2018 on mobile web. 
user_returns_31days_all <-  user_returns_31days_all %>% filter(avg_days_till_next_access != 3.000000) 


p <- ggplot(user_returns_31days_all, aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
  geom_line() + 
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user returns within 31 days on all Wikipedia projects from all countries",
       subtitle = "Desktop and Mobile Web Combined") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave(filename="user_returns_31days_all.png", plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
rm(p)

#7 DAY RETURNS 

##Time series of the average next return time (within 7 days) by country

fig_path <- file.path("figures/avg_user_returns/7days")
plot_resolution <- 192

user_returns_7days_bycountry <- read.delim("data/user_returns_allwikis_7days_bycountry.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_7days_bycountry$last_seen_date <- as.Date(user_returns_7days_bycountry$last_seen_date, format = "%d-%b-%Y")


#Replace country codes with names (currently just replacing countries used in analysis)
user_returns_7days_bycountry$country  <- plyr::mapvalues(
  user_returns_7days_bycountry$country, from=c("RS", "DZ", "GR", "BB", "AU", "BV","CC", "CX", "EH", "FK",
                                                "US", "ES", "DE", "JP", "GB", "FR"),
  to=c("Serbia", "Algeria", "Greece", "Barbados", "Australia", "Bouvet Island", "Cocos (Keeling) Islands", 
       "Christmas Island", "Western Sahara", "Falkland Islands (Malvinas)", "United States", "Spain", "Germany", "Japan", "United Kingdom", "France"))


#order countries by the max avg days till next access over time series.

user_returns_7days_max <- user_returns_7days_bycountry %>%
  filter(country != "AQ") %>% #remove Antartica as outlier
  group_by(country) %>%
  summarise(max_return_time = max(avg_days_till_next_access))  %>%
  arrange(max_return_time)


#Create list of countries with the by highest and lowest max return times over 31 days and countries with large-sized language wikis

lowest_max_7days <- c("Serbia", "Algeria", "Greece", "Barbados", "Australia")
highest_max_7days <- c("Bouvet Island", "Cocos (Keeling) Islands", 
                       "Christmas Island", "Western Sahara", "Falkland Islands (Malvinas)")


#Function to plot average returns within 7 days by country code. 

plot_7day_returns_bycountry <- function(x) {
  
  p <- ggplot(subset(user_returns_7days_bycountry, country == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
    labs(title = paste("Average user returns within 7 days on all Wikipedia projects from", x)) +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename=paste0("user_returns_7days_",x,".png"), plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)
}

#Generate plots for various countries of interest
lapply(largewikicountries, plot_7day_returns_bycountry)
lapply(lowest_max_7days, plot_7day_returns_bycountry)
lapply(highest_max_7days, plot_7day_returns_bycountry)


# Time series of the average next return time (within 7 days) by project (language) [Only Wikipedia]

user_returns_7days_byproject <- read.delim("data/user_returns_allwikis_7days_byproject.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_7days_byproject$last_seen_date <- as.Date(user_returns_7days_byproject$last_seen_date, format = "%d-%b-%Y")


#Create function to plot average returns within 31 days by project (language)


plot_7day_returns_byproject <- function(x) {
  
  p <- ggplot(subset(user_returns_7days_byproject, project == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
    labs(title = paste("Average user returns within 7 days from ",x,".wikipedia", sep = "")) +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename=paste0("user_returns_7days_",x,".wikpedia.png"), plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)
}

#Look at lists of largest and smallest sized wiki based on Wiki Segmentation database

lapply(smallsizedwikis, plot_7day_returns_byproject)
lapply(topsizedwikis, plot_7day_returns_byproject)
       

# Look at various categories of wikis grouped together in same chart.
# Top 5 wikis by size - Returns with 7 days

topsizedwikis_7days <- user_returns_7days_byproject %>%
  filter(project %in% topsizedwikis)

p <- ggplot(topsizedwikis_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = project)) +
  geom_line() + 
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user returns within 7days from top ranked Wikipedias by size",
       subtitle = "Desktop and Mobile Web Combined") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave(filename= "user_returns_7days_topwikipedias.png", plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
rm(p)

#Top small project wikis by size all in the same plot

smallsizedwikis_7days <- user_returns_7days_byproject %>%
  filter(project %in% smallsizedwikis) 

p <- ggplot(smallsizedwikis_7days, aes(x = last_seen_date, y = avg_days_till_next_access, color = project)) +
  geom_line() + 
  scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
  scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
  labs(title = "Average user returns within 7 days from lowest ranked Wikipedias by size",
       subtitle = "Desktop and Mobile Web Combined") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        panel.grid = element_line("gray70"))

ggsave(filename= "user_returns_7days_smallwikipedias.png", plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
rm(p)


# Time series of the average next return time (within 7 days) by project class (Wikipedia, Commons, Wikidata, etc)

user_returns_7days_byprojectclass <- read.delim("data/user_returns_allwikis_7days_byprojectclass.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_7days_byprojectclass$last_seen_date <- as.Date(user_returns_7days_byprojectclass$last_seen_date, format = "%d-%b-%Y")

us_peak <- user_returns_31days_byprojectclass %>%
  filter(project_class== "wikidata")
         
#Plot average returns within 7 days by project class for all countries and languages. 

project_classes <- c("mediawiki", "wikidata", "wikimedia", "wikipedia", "wikisource", "wikivoyage", "wiktionary", "wikinews")

plot_7day_returns_byprojectclass <- function(x) {
  
  p <- ggplot(subset(user_returns_7days_byprojectclass, project_class == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%Y-%m-%d"), date_breaks = "1 month")  +
    labs(title = paste("Average user returns within 7 days from", x)) +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename=paste0("user_returns_7days_", x, ".png"), plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)
}

lapply(project_classes, plot_7day_returns_byprojectclass)


# HISTOGRAM OF RETURN TIME AROUND ANOMALIES 

#Create function to generate histograms of return time for several (e.g. +/-3) days around identified anomalies.
#With ability to break out by project, browser_family and os_family.

plot_return_histograms <- function(date_var,df,fill_var) #where x is date and y is the breakout group
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)", fill = fill_var), 
             stat = "identity") +
    scale_y_continuous("Returns each day", labels = percent) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
    labs(title = paste("Days until next access for", date_var),
         subtitle = paste("on desktop from Germany on all Wikis by",fill_var))  +
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
lapply(seq(as.Date("2018-02-26"), as.Date("2018-03-04"), by=1), plot_return_histograms, df =return_frequency_Mar18, fill_var = NULL)



#From US, there were several drop in avg user returns within 31 days in July 2017 to exactly 1.0 or 2.0 days (even).

fig_path <- file.path("figures/return_histograms/july_2017_us")

return_frequency_Jul17 <- rbind(readr::read_rds("data/us_return_frequency_July17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms, df =return_frequency_Jul17, fill_var = "project")
lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms, df =return_frequency_Jul17, fill_var = "os_family")
lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms, df =return_frequency_Jul17, fill_var = "browser_family")
lapply(seq(as.Date("2017-07-05"), as.Date("2017-07-11"), by=1), plot_return_histograms, df =return_frequency_Jul17, fill_var = NULL)


#The histograms don't show any abnormalities around this date. Taking a further look into raw data is appears this is a data error that should be filtered.

#In Spain (ES), there was a spike in avg user returns within 31 days on desktop on 2017-12-21 (rose from an avg of 6 to 8.9)

fig_path <- file.path("figures/return_histograms/dec_2017_es")

es_return_frequency_Dec17 <- rbind(readr::read_rds("data/es_return_frequency_dec17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =es_return_frequency_Dec17, fill_var = "project")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =es_return_frequency_Dec17, fill_var = "os_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =es_return_frequency_Dec17, fill_var = "browser_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =es_return_frequency_Dec17, fill_var = NULL)



#In France (FR), there was also a spike in avg user returns within 31 days on desktop on 2017-12-21 (rose from an avg of around 6 to 8.2)

fig_path <- file.path("figures/return_histograms/dec_2017_fr")

fr_return_frequency_Dec17 <- rbind(readr::read_rds("data/fr_return_frequency_dec17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =fr_return_frequency_Dec17, fill_var = "project")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =fr_return_frequency_Dec17, fill_var = "os_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =fr_return_frequency_Dec17, fill_var = "browser_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =fr_return_frequency_Dec17, fill_var = NULL)



#In the United States, there was also a spike in avg user returns within 31 days on 2017-12-21 (rose from an avg of around 5.5 to 7.4)

fig_path <- file.path("figures/return_histograms/dec_2017_us")

us_return_frequency_Dec17 <- rbind(readr::read_rds("data/us_return_frequency_dec17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =us_return_frequency_Dec17, fill_var = "project")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =us_return_frequency_Dec17, fill_var = "os_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =us_return_frequency_Dec17, fill_var = "browser_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =us_return_frequency_Dec17, fill_var = NULL)


#On Wikisource, there is a drop on 2017-07-12 to 2.4 avg returns within 31 days on desktop from all countries

fig_path <- file.path("figures/return_histograms/sept_2017_wikisource")

wikisource_return_frequency_Sept17 <- rbind(readr::read_rds("data/wikisource_return_frequency_Sept17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#project class break down not needed since data only includes Wikisource data 
lapply(seq(as.Date("2017-09-05"), as.Date("2017-09-11"), by=1), plot_return_histograms, df =wikisource_return_frequency_Sept17, fill_var = "os_family")
lapply(seq(as.Date("2017-09-05"), as.Date("2017-09-11"), by=1), plot_return_histograms, df =wikisource_return_frequency_Sept17, fill_var = "browser_family")
lapply(seq(as.Date("2017-09-05"), as.Date("2017-09-11"), by=1), plot_return_histograms, df =wikisource_return_frequency_Sept17, fill_var = NULL)



#On Wikisource, there were a number of drops in avg returns within 31 days on desktop from all countries in July 2017. 
#Plot Histograms around drop 2017-07-12 to 2.0 avg returns and 2017-07-12 to 1.0 avg returns.  

fig_path <- file.path("figures/return_histograms/july_2017_wikisource")

wikisource_return_frequency_Jul17 <- rbind(readr::read_rds("data/wikisource_return_frequency_Jul17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#project class break down not needed since data only includes Wikisource data 
lapply(seq(as.Date("2017-07-09"), as.Date("2017-07-15"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul17, fill_var = "os_family")
lapply(seq(as.Date("2017-07-09"), as.Date("2017-07-15"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul17, fill_var = "browser_family")
lapply(seq(as.Date("2017-07-09"), as.Date("2017-07-15"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul17, fill_var = NULL)



#There was a also a recent drop on 2018-07-18 from an average of around 6/7 to 3.63 on Wikisource from all countries

fig_path <- file.path("figures/return_histograms/july_2018_wikisource")

wikisource_return_frequency_Jul18 <- rbind(readr::read_rds("data/wikisource_return_frequency_Jul18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Android", "iOS", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other'))

#project class break down not needed since data only includes Wikisource data 
lapply(seq(as.Date("2018-07-15"), as.Date("2018-07-21"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul18, fill_var = "os_family")
lapply(seq(as.Date("2018-07-15"), as.Date("2018-07-21"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul18, fill_var = "browser_family")
lapply(seq(as.Date("2018-07-15"), as.Date("2018-07-21"), by=1), plot_return_histograms, df =wikisource_return_frequency_Jul18, fill_var = NULL)




#On Wikidata, there was a recent drop on 2018-06-30 from an average of around 7.5 to 1.69 on desktop

fig_path <- file.path("figures/return_histograms/june_2018_wikidata")

wikidata_return_frequency_Jun18 <- rbind(readr::read_rds("data/wikidata_return_frequency_Jun18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Android", "iOS", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#project class break down not needed since data only includes Wikisource data 
lapply(seq(as.Date("2018-06-27"), as.Date("2018-07-03"), by=1), plot_return_histograms, df =wikidata_return_frequency_Jun18, fill_var = "os_family")
lapply(seq(as.Date("2018-06-27"), as.Date("2018-07-03"), by=1), plot_return_histograms, df =wikidata_return_frequency_Jun18, fill_var = "browser_family")
lapply(seq(as.Date("2018-06-27"), as.Date("2018-07-03"), by=1), plot_return_histograms, df =wikidata_return_frequency_Jun18, fill_var = NULL)


#Added break out to main pages using available webrequest table data from August 2018. 
# Data queries from mneisler.last_access_main_page_views_by_country
#This query only looked at wikipedia projects as that's where the spikes in Aug 2018 occured. 

#Main Page View histogram function (adjusted title to clarify)
plot_return_histograms_mainpages <- function(date_var,df,fill_var) #where x is date and y is the breakout group
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)", fill = fill_var), 
             stat = "identity") +
    scale_y_continuous("Returns each day", labels = percent) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
    labs(title = paste("Days until next access for", date_var),
         subtitle = "on desktop from Indonesia on all Wikipedia Projects broken down by main page views",
         caption = "Main page views also include pages where page title was not extracted")  +
    wmf::theme_min() +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(plot.subtitle=element_text(hjust=0.5))
  
  ggsave(filename=paste0(fill_var,"_return_histogram",date_var,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  
}

#Bangladesh spike to a peak of 7.9 on 2018-08-16 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_bd")

bd_return_frequency_Aug18 <- rbind(readr::read_rds("data/bd_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari', 'UC Browser'), browser_family, 'Other')) 


#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms_mainpages, df =bd_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18 , fill_var = "os_family")
lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18 , fill_var = "browser_family")
lapply(seq(as.Date("2018-08-13"), as.Date("2018-08-19"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18 , fill_var = NULL)



#Bangladesh spike to a peak of 7.9 on 2018-08-15 on mobile web
fig_path <- file.path("figures/return_histograms/aug_2018_bd_mw")

bd_return_frequency_Aug18_mw <- rbind(readr::read_rds("data/bd_return_frequency_Aug18_mw.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c('Android', 'iOS', 'Windows Phone', 'Windows', 'Linux'), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome Mobile', 'UC Browser', 'Opera Mini', 'Samsung Internet', 'Chrome Mobile WebView',
                                                            'Opera Mobile', 'Android', 'Mobile Safari'), browser_family, 'Other')) 


#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms_mainpages, df =bd_return_frequency_Aug18_mw, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18_mw, fill_var = "os_family")
lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18_mw, fill_var = "browser_family")
lapply(seq(as.Date("2018-08-12"), as.Date("2018-08-18"), by=1), plot_return_histograms, df =bd_return_frequency_Aug18_mw, fill_var = NULL)



#Indonesia spike to a peak of 8.3 on 2018-08-07 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_id")

id_return_frequency_Aug18 <- rbind(readr::read_rds("data/id_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms_mainpages, df =id_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms, df =id_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms, df =id_return_frequency_Aug18 , fill_var = "browser_family")
lapply(seq(as.Date("2018-08-04"), as.Date("2018-08-10"), by=1), plot_return_histograms, df =id_return_frequency_Aug18 , fill_var = NULL)



#Indonesia spike to a peak of 8.3 on 2018-08-09 on mobile web
fig_path <- file.path("figures/return_histograms/aug_2018_id_mw")

id_return_frequency_Aug18_mw <- rbind(readr::read_rds("data/id_return_frequency_Aug18_mw.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c('Android', 'iOS', 'Windows CE', 'Windows Phone', 'Windows', 'BlackBerry OS', 'Linux'), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome Mobile', 'UC Browser', 'Samsung Internet', 'Chrome Mobile WebView', 'Mobile Safari',
                                                            'Opera Mobile', 'Chrome', 'Android', 'Opera Mini'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms_mainpages, df =id_return_frequency_Aug18_mw, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms, df =id_return_frequency_Aug18_mw, fill_var = "os_family")
lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms, df =id_return_frequency_Aug18_mw, fill_var = "browser_family")
lapply(seq(as.Date("2018-08-06"), as.Date("2018-08-12"), by=1), plot_return_histograms, df =id_return_frequency_Aug18_mw, fill_var = NULL)



#France spike to a peak of 6.5 on 2018-08-03 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_fr")

fr_return_frequency_Aug18 <- rbind(readr::read_rds("data/fr_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms_mainpages, df =fr_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms, df =fr_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms, df =fr_return_frequency_Aug18 , fill_var = "browser_family")
lapply(seq(as.Date("2018-08-01"), as.Date("2018-08-07"), by=1), plot_return_histograms, df =fr_return_frequency_Aug18 , fill_var = NULL)




#India spike to a peak of 6.5 on 2018-08-14 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_in")

in_return_frequency_Aug18 <- rbind(readr::read_rds("data/in_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms, df =in_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms, df =in_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms, df =in_return_frequency_Aug18 , fill_var = "browser_family")
lapply(seq(as.Date("2018-08-11"), as.Date("2018-08-17"), by=1), plot_return_histograms, df =in_return_frequency_Aug18 , fill_var = NULL)


#Japan spike to a peak of 6.7 on 2018-08-10 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_jp")

jp_return_frequency_Aug18 <- rbind(readr::read_rds("data/jp_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =jp_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =jp_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =jp_return_frequency_Aug18 , fill_var = "browser_family")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =jp_return_frequency_Aug18 , fill_var = NULL)



#Spain spike to a peak of 6.9 on 2018-08-10 on desktop
fig_path <- file.path("figures/return_histograms/aug_2018_es")

es_return_frequency_Aug18 <- rbind(readr::read_rds("data/es_return_frequency_Aug18.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Windows", "Mac OS X", "Linux", "Ubuntu", "Windows CE", "Chrome OS"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

#generate histograms by various dates aroud spike date.

lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =es_return_frequency_Aug18, fill_var = "is_main_page")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =es_return_frequency_Aug18, fill_var = "os_family")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =es_return_frequency_Aug18 , fill_var = "browser_family")
lapply(seq(as.Date("2018-08-07"), as.Date("2018-08-13"), by=1), plot_return_histograms, df =es_return_frequency_Aug18 , fill_var = NULL)



