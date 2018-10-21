#Daily histograms with added breakdown of main page views and pages where title was not extracted.

#Query data for return frequency in Bangladesh around 2018-08-16 spike date (both on desktop). Use Webrequest table to also find views to main pages and special pages.
#Remotely from Stat 005
start_date <- 1534118400 ## 08/13/2018 @ 12:00am (UTC)
end_date <- 1534636800 ## 08/19/2018 @ 12:00am (UTC)

bd_return_frequency_Aug18 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop'
                 AND country_code = 'BD'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(bd_return_frequency_Aug18, "bd_return_frequency_Aug18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/bd_return_frequency_Aug18.rds bd_return_frequency_Aug18.rds")

#Query data for return frequency in Bangladesh around 2018-08-15 spike date on mobile web. Use Webrequest table to also find views to main pages and special pages.
#Remotely from Stat 005

start_date <- 1534032000 ## 08/12/2018 @ 12:00am (UTC)
end_date <- 1534550400 ## 08/18/2018 @ 12:00am (UTC)

bd_return_frequency_Aug18_mw <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'mobile web'
                 AND country_code = 'BD'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(bd_return_frequency_Aug18_mw, "bd_return_frequency_Aug18_mw.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/bd_return_frequency_Aug18_mw.rds bd_return_frequency_Aug18_mw.rds")



#Query data for return frequency in Indonesia on desktop around 2018-08-07 spike date (both on desktop and mobile web). Use Webrequest table to also find views to main pages
#Remotely from Stat 005
start_date <- 1533340800 ## 08/04/2018 @ 12:00am (UTC)
end_date <- 1533859200 ## 08/10/2018 @ 12:00am (UTC)

id_return_frequency_Aug18 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop'
                 AND country_code = 'ID'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(id_return_frequency_Aug18, "id_return_frequency_Aug18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/id_return_frequency_Aug18.rds id_return_frequency_Aug18.rds")

#Query data for return frequency in Indonesia around 2018-08-09 spike date on mobile web. Use Webrequest table to also find views to main pages and special pages.
#Remotely from Stat 005

start_date <- 1533513600 ## 08/06/2018 @ 12:00am (UTC)
end_date <- 1534032000 ## 08/12/2018 @ 12:00am (UTC)

id_return_frequency_Aug18_mw <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'mobile web'
                 AND country_code = 'ID'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(id_return_frequency_Aug18_mw, "id_return_frequency_Aug18_mw.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/id_return_frequency_Aug18_mw.rds id_return_frequency_Aug18_mw.rds")






#Query data for return frequency in France on desktop around 2018-08-03 spike date. Use Webrequest table to also find views to main pages
#Remotely from Stat 005
start_date <- 1533081600 ## 08/01/2018 @ 12:00am (UTC)
end_date <- 1533600000 ## 08/07/2018 @ 12:00am (UTC)

fr_return_frequency_Aug18 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop'
                 AND country_code = 'FR'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                 user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(fr_return_frequency_Aug18, "fr_return_frequency_Aug18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/fr_return_frequency_Aug18.rds fr_return_frequency_Aug18.rds")


#Query data for return frequency in India on desktop around 2018-08-14 spike date. Use Webrequest table to also find views to main pages
#Remotely from Stat 005
start_date <- 1533945600 ## 08/11/2018 @ 12:00am (UTC)
end_date <- 1534464000 ## 08/17/2018 @ 12:00am (UTC)

in_return_frequency_Aug18 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop'
                 AND country_code = 'IN'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                 user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(in_return_frequency_Aug18, "in_return_frequency_Aug18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/in_return_frequency_Aug18.rds in_return_frequency_Aug18.rds")


#Query data for return frequency in Japan on desktop around 2018-08-10 spike date. Use Webrequest table to also find views to main pages
#Remotely from Stat 005
start_date <- 1533600000 ## 08/07/2018 @ 12:00am (UTC)
end_date <- 1534118400 ## 08/13/2018 @ 12:00am (UTC)

jp_return_frequency_Aug18 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop'
                 AND country_code = 'JP'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                 user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(jp_return_frequency_Aug18, "jp_return_frequency_Aug18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/jp_return_frequency_Aug18.rds jp_return_frequency_Aug18.rds")


#Query data for return frequency in Spain on desktop around 2018-08-10 spike date. Use Webrequest table to also find views to main pages
#Remotely from Stat 005
start_date <- 1533600000 ## 08/07/2018 @ 12:00am (UTC)
end_date <- 1534118400 ## 08/13/2018 @ 12:00am (UTC)

es_return_frequency_Aug18 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 is_main_page, (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM mneisler.last_access_main_page_views_by_country
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop'
                 AND country_code = 'ES'
                 AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, 
                 user_agent_map['os_family'], user_agent_map['browser_family'], is_main_page
                 ;")
  
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(es_return_frequency_Aug18, "es_return_frequency_Aug18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/es_return_frequency_Aug18.rds es_return_frequency_Aug18.rds")


