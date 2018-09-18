
#Plot histograms showing frequeny of next return datea (days until next access) around avg user return spikes in Indonesia and Bangladesh. 

library(tidyverse)
library(ggplot2)
library(scales)
library(anytime)


# Indonesia June 2017 spike on desktop (peak of 9.9 avg days until next access on June 22, 2017)
fig_path <- file.path("figures/Indonesia/June_2017_peak")
plot_resolution <- 192


#Find total sum of returns each day for June 2017 for all Wikipedia Projects
id_data_june2017 <- rbind(readr::read_rds("data/id_return_frequency_june17.rds"))

id_return_frequency_june2017<- id_data_june2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


#Generate frequency histograms for 5 days around the June 2017 peak 

lapply(seq(as.Date("2017-06-20"), as.Date("2017-06-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_june2017, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by all non-wikipedia projects

id_return_frequency_june2017_byproject<- id_data_june2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate histograms by project for 5 days around the June 2017 peak 

lapply(seq(as.Date("2017-06-20"), as.Date("2017-06-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_june2017_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary os_family types

id_return_frequency_june2017_byos<- id_data_june2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by os family for 5 days around the June 2017 peak 

lapply(seq(as.Date("2017-06-20"), as.Date("2017-06-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_june2017_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary browser_family types

id_return_frequency_june2017_bybrowser<- id_data_june2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))

##Generate histograms by browser_family for 5 days around the June 2017 peak 

lapply(seq(as.Date("2017-06-20"), as.Date("2017-06-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_june2017_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by browser family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

# Indonesia December 2017 spike on desktop (Peak of 8.55 on 2017-12-22)
fig_path <- file.path("figures/Indonesia/Dec 2017 peak")
plot_resolution <- 192


#Find total sum of returns each day for June 2017 for all Wikipedia Projects

id_data_dec2017 <- rbind(readr::read_rds("data/id_return_frequency_dec17.rds"))

#Look at frequency of returns for all wikipedia projects and all browser/os types
id_return_frequency_dec2017<- id_data_dec2017 %>%
  dplyr::mutate(date = anydate(date)) %>% # convert UNIIX to date format
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


#Generate frequency histograms for 5 days around the Dec 2017 peak 

lapply(seq(as.Date("2017-12-20"), as.Date("2017-12-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_dec2017, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Wikipedia Projects"))  +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by all non-wikipedia projects

id_return_frequency_dec2017_byproject<- id_data_dec2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


#Generate histograms by project for 5 days around the Dec 2017 peak 

lapply(seq(as.Date("2017-12-20"), as.Date("2017-12-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_dec2017_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary os_family types

id_return_frequency_dec2017_byos<- id_data_dec2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by os family for 5 days around the Dec 2017 peak 

lapply(seq(as.Date("2017-12-20"), as.Date("2017-12-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_dec2017_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by primary browser_family types
id_return_frequency_dec2017_bybrowser<- id_data_dec2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by browser_family for 5 days around the Dec 2017 peak 
lapply(seq(as.Date("2017-12-20"), as.Date("2017-12-24"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_dec2017_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by browser family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Indonesia February 2018 spike on desktop (peak of 8.6 avg days until next access on February 09, 2018)
fig_path <- file.path("figures/Indonesia/Feb_2018_peak")
plot_resolution <- 192

id_data_feb2018 <- rbind(readr::read_rds("data/id_return_frequency_feb18.rds"))

#Find total sum of returns each day for February 2018 for all Wikipedia Projects
id_return_frequency_feb2018<- id_data_feb2018 %>%
  dplyr::mutate(date = anydate(date)) %>% # convert UNIIX to date format
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate frequency histograms for 5 days around the February 2018 peak 

lapply(seq(as.Date("2018-02-07"), as.Date("2018-02-11"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_feb2018, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by all non-wikipedia projects

id_return_frequency_feb2018_byproject<- id_data_feb2018 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate histograms by project for 5 days around the February 2018 peak 

lapply(seq(as.Date("2018-02-07"), as.Date("2018-02-11"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_feb2018_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary os_family types

id_return_frequency_feb2018_byos<- id_data_feb2018 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


#Generate histograms by os_family for 5 days around the Feb 2018 peak 

lapply(seq(as.Date("2018-02-07"), as.Date("2018-02-11"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_feb2018_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary browser_family types
id_return_frequency_feb2018_bybrowser<- id_data_feb2018 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by browser_family for 5 days around the February 2018 peak 
lapply(seq(as.Date("2018-02-07"), as.Date("2018-02-11"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_feb2018_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by browser family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

#Indonesia October 2017: Look at non-spike time for comparison.
fig_path <- file.path("figures/Indonesia/Oct_2017_normal")
plot_resolution <- 192

id_data_oct2017 <- rbind(readr::read_rds("data/id_return_frequency_oct17.rds"))

##Find total sum of returns each day for October 2017 for all Wikipedia Projects
id_return_frequency_oct2017<- id_data_oct2017 %>%
  dplyr::mutate(date = anydate(date)) %>% # convert UNIIX to date format
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate frequency histograms for 5 days around the October 2017 

lapply(seq(as.Date("2017-10-13"), as.Date("2017-10-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_oct2017, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by all non-wikipedia projects
id_return_frequency_oct2017_byproject<- id_data_oct2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate histograms by project for 5 days around the Oct 2017  

lapply(seq(as.Date("2017-10-13"), as.Date("2017-10-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_oct2017_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary os_family types

id_return_frequency_oct2017_byos<- id_data_oct2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by os family for 5 days around the October 2017  

lapply(seq(as.Date("2017-10-13"), as.Date("2017-10-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_oct2017_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary browser_family types
id_return_frequency_oct2017_bybrowser<- id_data_oct2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by browser_family for 5 days around the October 2017  
lapply(seq(as.Date("2017-10-13"), as.Date("2017-10-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(id_return_frequency_oct2017_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Indonesia on desktop on by browser family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("id_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


# Bangladesh
fig_path <- file.path("figures/Bangladesh/January_2017_peak")
plot_resolution <- 192

# Bangladesh January 2017 spike on desktop (peak of 13 avg days until next access on Jan 27, 2017)
bd_data_jan2017 <- rbind(readr::read_rds("data/bd_return_frequency_jan17.rds"))

##Find total sum of returns each day for Jan 2017 for all Wikipedia Projects
bd_return_frequency_jan2017<- bd_data_jan2017  %>%
  dplyr::mutate(date = anydate(date)) %>% # convert UNIIX to date format
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate frequency histograms for 5 days around the Jan 2017 peak 

lapply(seq(as.Date("2017-01-25"), as.Date("2017-01-29"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_jan2017, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on desktop on all Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by all non-wikipedia projects

bd_return_frequency_jan2017_byproject<- bd_data_jan2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate histograms by project for 5 days around the Jan 2017 peak 

lapply(seq(as.Date("2017-01-25"), as.Date("2017-01-29"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_jan2017_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on desktop on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by primary os_family types

bd_return_frequency_jan2017_byos<- bd_data_jan2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by os family for 5 days around the Jan 2017 peak 

lapply(seq(as.Date("2017-01-25"), as.Date("2017-01-29"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_jan2017_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on desktop on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary browser_family types
bd_return_frequency_jan2017_bybrowser<- bd_data_jan2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Chrome', 'Firefox', 'IE', 'Opera', 'Safari'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))

##Generate histograms by browser_family for 5 days around the Jan 2017 peak 
lapply(seq(as.Date("2017-01-25"), as.Date("2017-01-29"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_jan2017_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on desktop on by browser family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

# Bangladesh December 2017 spike on mobile web (peak of 7.67 avg number of days until next access on December 30, 2017 )
fig_path <- file.path("figures/Bangladesh/December_2017_peak")
plot_resolution <- 192


bd_data_dec2017 <- rbind(readr::read_rds("data/bd_return_frequency_dec17.rds"))

#Find total sum of returns each day for Dec 2017 for all Wikipedia Projects
bd_return_frequency_dec2017<- bd_data_dec2017  %>%
  dplyr::mutate(date = anydate(date)) %>% # convert UNIIX to date format
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))



##Generate frequency histograms for 5 days around the Dec 2017 peak 

lapply(seq(as.Date("2017-12-28"), as.Date("2018-01-01"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_dec2017, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on all Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by all non-wikipedia projects

bd_return_frequency_dec2017_byproject<- bd_data_dec2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate histograms by project for 5 days around the Dec 2017 peak 

lapply(seq(as.Date("2017-12-28"), as.Date("2018-01-01"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_dec2017_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by primary os_family types

bd_return_frequency_dec2017_byos<- bd_data_dec2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS", "Windows 8.1", "Mac OS X", "Windows XP", "Windows 10", "Windows 8", "Windows  Vista"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by os family for 5 days around the Dec 2017 peak 

lapply(seq(as.Date("2017-12-28"), as.Date("2018-01-01"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_dec2017_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary browser_family types
bd_return_frequency_dec2017_bybrowser<- bd_data_dec2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Android', 'Chrome Mobile', 'Firefox iOS', 'Firefox Mobile', 'IE Mobile', 'Opera Mobile', 'Safari', 'UC Browser', 'Opera Mini'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by browser_family for 5 days around the Dec 2017 peak 
lapply(seq(as.Date("2017-12-28"), as.Date("2018-01-01"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_dec2017_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on by browser family")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

# Bangladesh April 2018 spike on mobile web (peak of 7.6 occuring on 2018-04-10)
fig_path <- file.path("figures/Bangladesh/April_2018_peak")
plot_resolution <- 192

bd_data_apr2018 <- rbind(readr::read_rds("data/bd_return_frequency_apr18.rds"))

##Find total sum of returns each day for April 2018 for all Wikipedia Projects
bd_return_frequency_apr2018<- bd_data_apr2018  %>%
  dplyr::mutate(date = anydate(date)) %>% # convert UNIIX to date format
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate frequency histograms for 5 days around the April 2018 peak  

lapply(seq(as.Date("2018-04-08"), as.Date("2018-04-12"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_apr2018, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on all Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by all non-wikipedia projects

#For further breakdown by non-wikipedia project
bd_return_frequency_apr2018_byproject<- bd_data_apr2018 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate histograms by project for 5 days around the April 2018 peak 

lapply(seq(as.Date("2018-04-08"), as.Date("2018-04-12"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_apr2018_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by primary os_family types

bd_return_frequency_apr2018_byos<- bd_data_apr2018 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by os family for 5 days around the April 2018 peak  

lapply(seq(as.Date("2018-04-08"), as.Date("2018-04-12"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_apr2018_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary browser_family types
bd_return_frequency_apr2018_bybrowser<- bd_data_apr2018 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Android', 'Chrome Mobile', 'Firefox iOS', 'Firefox Mobile', 'IE Mobile', 'Opera Mobile', 'Safari', 'UC Browser', 'Opera Mini'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))

##Generate histograms by browser_family for 5 days around the April 2018 peak 
lapply(seq(as.Date("2018-04-08"), as.Date("2018-04-12"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_apr2018_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on by browser family")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

#Bangladesh October 2017: Look at non-spike time on mobile web for comparison.
fig_path <- file.path("figures/Bangladesh/November_2017_normal")
plot_resolution <- 192

bd_data_nov2017 <- rbind(readr::read_rds("data/bd_return_frequency_nov17.rds"))

##Find total sum of returns each day for Nov 2017 for all Wikipedia Projects
bd_return_frequency_nov2017<- bd_data_nov2017 %>%
  dplyr::mutate(date = anydate(date)) %>% # convert UNIIX to date format
  dplyr::filter(project == 'wikipedia') %>%
  dplyr::group_by(date, days_till_next_access) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate frequency histograms for 5 days around the Nov 2017 peak 

lapply(seq(as.Date("2017-11-13"), as.Date("2017-11-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_nov2017, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day)), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on all Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by all non-wikipedia projects
bd_return_frequency_nov2017_byproject<- bd_data_nov2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr::filter(project != 'wikipedia') %>% 
  dplyr::group_by(date, days_till_next_access, project) %>%
  dplyr::summarise(returns_each_day= sum(returns_each_day))


##Generate histograms by project for 5 days around the Nov 2017  

lapply(seq(as.Date("2017-11-13"), as.Date("2017-11-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_nov2017_byproject, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = project), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on all Non-Wikipedia Projects")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byproject_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})


##Breakout by primary os_family types

bd_return_frequency_nov2017_byos<- bd_data_nov2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
 dplyr::mutate(os_family = ifelse(os_family %in% c("Android", "Windows 7", "iOS"), os_family, 'Other')) %>% #Look at primary os_family groups
  dplyr::group_by(date, days_till_next_access, os_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))


##Generate histograms by os family for 5 days around the Nov 2017  

lapply(seq(as.Date("2017-11-13"), as.Date("2017-11-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_nov2017_byos, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = os_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on by os Family")) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_byOs_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})

##Breakout by primary browser_family types
bd_return_frequency_nov2017_bybrowser<- bd_data_nov2017 %>%
  dplyr::mutate(date = anydate(date)) %>%
  dplyr:: mutate(browser_family=ifelse(browser_family %in% c('Android', 'Chrome Mobile', 'Firefox iOS', 'Firefox Mobile', 'IE Mobile', 'Opera Mobile', 'Safari', 'UC Browser', 'Opera Mini'), browser_family, 'Other')) %>%
  dplyr::group_by(date, days_till_next_access, browser_family) %>%
  dplyr::summarise(returns_each_day = sum (returns_each_day))

##Generate histograms by browser_family for 5 days around the Nov 2017  
lapply(seq(as.Date("2017-11-13"), as.Date("2017-11-17"), by=1), function(x)
  {
  temp_plot = ggplot(subset(bd_return_frequency_nov2017_bybrowser, date== x)) + 
    geom_bar(aes(x=days_till_next_access, y= returns_each_day/sum(returns_each_day), fill = browser_family), 
           stat = "identity") +
  scale_y_continuous("Returns each day", labels = percent) +
  scale_x_continuous("Days until next access", breaks=seq(1,31,1))  +
  labs(title = paste("Days until next access for", x, "from Bangladesh on mobile web on by browser family")) +
    wmf::theme_min()
  
 ggsave(filename=paste0("bd_return_bybrowser_",x,".png"), plot = temp_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 6, width = 10, limitsize = FALSE)  

})



