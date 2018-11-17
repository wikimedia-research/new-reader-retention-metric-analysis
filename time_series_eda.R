
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


peak_views<- user_returns_31days_bycountry %>%
  filter(country == "ID",
         access_method == 'desktop',
         last_seen_date >= "2017-06-01" & last_seen_date <= "2017-07-01")  %>%
  arrange(last_seen_date)



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

#Generate plots for countries of interest
lapply(largewikicountries, plot_31day_returns_bycountry)
  
lapply(lowest_max, plot_31day_returns_bycountry)

lapply(highest_max, plot_31day_returns_bycountry)
  
plot_31day_returns_bycountry("Bangladesh")
plot_31day_returns_bycountry("Indonesia")


#Look at 1-month period for large countries to identify weekly seasonality. 
# Looked at February 2018 as there were limited anomalies this month.

#Desktop
avg_returns_desktop_1month <- user_returns_31days_bycountry %>%
  filter(country %in% (largewikicountries),
         access_method == 'desktop',
         last_seen_date >= "2018-02-01" & last_seen_date <= "2018-03-01")  %>%
  arrange(last_seen_date)


  p <- ggplot(avg_returns_desktop_1month, aes(x = last_seen_date, y = avg_days_till_next_access, color = country)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%a, %b-%d"), date_breaks = "2 days")  +
    labs(title ="Average user returns within 31 days on desktop on all Wikipedia projects by Country",
         subtitle = "Over 1-month period in February 2018") +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename="user_returns_31days_1month_desktop.png", plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)

  #Mobile Web
  
  avg_returns_mobile_1month <- user_returns_31days_bycountry %>%
    filter(country %in% (largewikicountries),
           access_method == 'mobile web',
           last_seen_date >= "2018-02-01" & last_seen_date <= "2018-03-01")  %>%
    arrange(last_seen_date)
  
  
  p <- ggplot(avg_returns_mobile_1month, aes(x = last_seen_date, y = avg_days_till_next_access, color = country)) +
    geom_line() + 
    scale_y_continuous("Average number of days until next access", labels = polloi::compress) +
    scale_x_date("Last access date", labels = date_format("%a, %b-%d"), date_breaks = "2 days")  +
    labs(title ="Average user returns within 31 days on mobile web on all Wikipedia projects by Country",
         subtitle = "Over 1-month period in February 2018 ") +
    ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid = element_line("gray70"))
  
  ggsave(filename="user_returns_31days_1month_mobileweb.png", plot = p, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  
  rm(p)
  
  
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


