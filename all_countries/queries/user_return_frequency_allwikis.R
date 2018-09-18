#Query to generate daily histograms of return time 

#Remotely from Stat005. 

#Query next return data around peak date on March 1, 2018 on wikipedia on desktop from Germany
start_date <- 1519603200 ## 02/26/2018 @ 12:00am (UTC)
end_date <- 1520121600 ## 03/04/2018 @ 12:00am (UTC)

de_return_frequency_Mar18 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, project_class AS project, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND access_method = 'desktop'
                 AND country_code = 'DE'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(de_return_frequency_Mar18, "de_return_frequency_Mar18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/de_return_frequency_Mar18.rds de_return_frequency_Mar18.rds")

#Query next return data around drops in July 2017 on wikipedia on desktop from United States
#remotely from Stat 005
start_date <- 1499212800 ## 07/05/2017 @ 12:00am (UTC)
end_date <- 1499731200 ## 07/11/2017 @ 12:00am (UTC)

us_return_frequency_July17 <- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, project_class AS project, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2017
                 AND access_method = 'desktop'
                 AND country_code = 'US'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(us_return_frequency_July17, "us_return_frequency_July17.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/us_return_frequency_July17.rds us_return_frequency_July17.rds")