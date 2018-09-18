
library(tidyverse)
library(ggplot2)
library(scales)
library(anytime)

#Time series of the average next return time (within 31 days) by country on all Wikipedia projects

fig_path <- file.path("figures/avg_user_returns/31days")
plot_resolution <- 192

user_returns_31days_bycountry <- read.delim("data/user_returns_allwikis_31days_bycountry.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_31days_bycountry$last_seen_date <- as.Date(user_returns_31days_bycountry$last_seen_date, format = "%d-%b-%Y")

#order countries by the max avg days till next access.

user_returns_31days_max <- user_returns_31days_bycountry %>%
  filter(country != "AQ") %>% #remove Antartica as outlier
  group_by(country) %>%
  summarise(max_return_time = max(avg_days_till_next_access))  %>%
  arrange(desc(max_return_time))

#Create list of countries with the by highest and lowest max return times over 31 days and countries with large-sized language wikis
lowest_max <- c("BV", "IN", "TT", "MT", "TW")
highest_max <- c("CC","FK", "NF", "SH", "SJ")
largewikicountries <- c("US", "ES", "DE", "JP", "GB", "FR") #based on countries with largest language wikis by size

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
lapply(largewikicountries, plot_31day_returns)
  
lapply(lowest_max, plot_31day_returns) 
  
lapply(highest_max, plot_31day_returns)  
  
  

# Time series of the average next return time (within 31 days) by project (language) [Only Wikipedia]

user_returns_31days_byproject <- read.delim("data/user_returns_allwikis_31days_byproject.tsv", sep = "\t", stringsAsFactors =FALSE)
user_returns_31days_byproject$last_seen_date <- as.Date(user_returns_31days_byproject$last_seen_date, format = "%d-%b-%Y")

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


#Function to plot average returns within 31 days by project class for all countries and languages. 

project_classes <- c("mediawiki", "wikidata", "wikimedia", "wikipedia", "wikisource", "wikivoyage", "wiktionary", "wikinews")

plot_31day_returns_byprojectclass <- function(x) {
  
  p <- ggplot(subset(user_returns_31days_byprojectclass, project == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
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


#order countries by the max avg days till next access over time series.

user_returns_7days_max <- user_returns_7days_bycountry %>%
  filter(country != "AQ") %>% #remove Antartica as outlier
  group_by(country) %>%
  summarise(max_return_time = max(avg_days_till_next_access))  %>%
  arrange(max_return_time)


#Create list of countries with the by highest and lowest max return times over 31 days and countries with large-sized language wikis

lowest_max_7days <- c("RS", "DZ", "GR", "BB", "AU")
highest_max_7days <- c("BV","CC", "CX", "EH", "FK")


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


#Plot average returns within 7 days by project class for all countries and languages. 

project_classes <- c("mediawiki", "wikidata", "wikimedia", "wikipedia", "wikisource", "wikivoyage", "wiktionary", "wikinews")

plot_7day_returns_byprojectclass <- function(x) {
  
  p <- ggplot(subset(user_returns_7days_byprojectclass, project == x), aes(x = last_seen_date, y = avg_days_till_next_access, color = access_method)) +
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

#Create function to generate histograms of return time for several (e.g. +/-3) days around identifeid anomalies.
#With ability to break out by project, browser_family and os_family.

plot_return_histograms <- function(date_var,df,fill_var) #where x is date and y is the breakout group
{
  temp_plot = ggplot(subset(df, date == date_var)) + 
    geom_bar(aes_string(x="days_till_next_access", y= "returns_each_day/sum(returns_each_day)", fill = fill_var), 
             stat = "identity") +
    scale_y_continuous("Returns each day", labels = percent) +
    scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
    labs(title = paste("Days until next access for", date_var, "on desktop from United States on all Wikipedia Projects by", fill_var )) +
    wmf::theme_min()
  
  ggsave(filename=paste0(fill_var,"_return_histogram",date_var,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  
}

## DEMCEMBER 2017 Spike on all wikipedia projects on desktop ###

fig_path <- file.path("figures/return_histograms/dec_2017_all")

return_frequency_Dec17 <- rbind(readr::read_rds("data/allwikis_return_frequency_Dec17.rds")) %>%
  dplyr::mutate(date = anydate(date),
                os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", 
                                                    "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other'),
                browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) 

lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =return_frequency_Dec17, fill_var = "project")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =return_frequency_Dec17, fill_var = "os_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =return_frequency_Dec17, fill_var = "browser_family")
lapply(seq(as.Date("2017-12-18"), as.Date("2017-12-24"), by=1), plot_return_histograms, df =return_frequency_Dec17, fill_var = NULL)


#In Germany (DE), there was a spike in avg user returns within 31 days on 2018-03-01 (rose from an avg of 5 to 8.58)

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

