
#Indonesia Data
##Frequency of Returns for Indonesia around peak date of 2017-06-22

#Remotely from Stat005
#Indonesia Peak on 2017-06-22 on desktop
start_date <- 1497916800  #2017-06-20
end_date <-  1498262400 #2017-06-24

id_return_frequency_june17<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
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
    AND country_code = 'ID'
    AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
    AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
    GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(id_return_frequency_june17, "id_return_frequency_june17.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/id_return_frequency_june17.rds id_return_frequency_june17.rds")


##Frequency of Returns for Indonesia around peak date of 2017-12-22

#Remotely from Stat005
#Indonesia Peak on 2017-12-22 on desktop
start_date <- 1513728000  #2017-12-20
end_date <-  1514073600 #2017-12-24

id_return_frequency_dec17<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, project_class AS project, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND (year = 2017 or year= 2018)
                 AND access_method = 'desktop'
                 AND country_code = 'ID'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))
readr::write_rds(id_return_frequency_dec17, "id_return_frequency_dec17.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/id_return_frequency_dec17.rds id_return_frequency_dec17.rds")


##Frequency of Returns for Indonesia around peak date of 2018-02-09 (peak of 8.6)

#Remotely from Stat005
#Indonesia Peak on 2018-2-09 on desktop
start_date <- 1517961600  #2018-02-07
end_date <-  1518307200 #2018-02-11

id_return_frequency_feb18<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, project_class AS project, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year= 2018
                 AND access_method = 'desktop'
                 AND country_code = 'ID'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(id_return_frequency_feb18, "id_return_frequency_feb18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/id_return_frequency_feb18.rds id_return_frequency_feb18.rds")


#Look at non-spike month in Indonesia for comparison

##Frequency of Returns for Indonesia around 2017-10-15

#Remotely from Stat005
start_date <- 1507852800  #2017-10-13
end_date <-  1508198400 #2017-10-17

id_return_frequency_oct17<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
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
                 AND country_code = 'ID'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(id_return_frequency_oct17, "id_return_frequency_oct17.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/id_return_frequency_oct17.rds id_return_frequency_oct17.rds")



#Bangladesh Data#

##Frequency of Returns for Bangladesh around peak date of 2017-01-27

#Remotely from Stat005
#Frequency of returns around  2017-01-27 on desktop
start_date <- 1485302400  #2017-01-25
end_date <-  1485648000 #2017-01-29

bd_return_frequency_jan17<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
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
                 AND country_code = 'BD'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(bd_return_frequency_jan17, "bd_return_frequency_jan17.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/bd_return_frequency_jan17.rds bd_return_frequency_jan17.rds")

#Frequency of Returns for Bangladesh around peak date of 2017-12-30

#Remotely from Stat005
#Indonesia Peak on 2017-12-30 on mobile web 
start_date <- 1514419200  #2017-12-28
end_date <-  1514764800 #2018-01-01

bd_return_frequency_dec17<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, project_class AS project, access_method, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND (year = 2017 or year= 2018)
                 AND country_code = 'BD'
                 AND access_method = 'mobile web'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family'],
                 access_method
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(bd_return_frequency_dec17, "bd_return_frequency_dec17.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/bd_return_frequency_dec17.rds bd_return_frequency_dec17.rds")


##Frequency of Returns for Bangladesh around peak date of 2018-04-10

#Remotely from Stat005
#Indonesia Peak on 2018-04-10 on mobile web
start_date <- 1523145600  #2018-04-08
end_date <-  1523491200 #2018-04-12

bd_return_frequency_apr18<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, project_class AS project, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2018
                 AND country_code = 'BD'
                 AND access_method = 'mobile web' 
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(bd_return_frequency_apr18, "bd_return_frequency_apr18.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/bd_return_frequency_apr18.rds bd_return_frequency_apr18.rds")


#Look at non-spike month in Brangladesk for comparison

#Frequency of Returns for Bangladesh around 2017-11-15 on mobile web

#Remotely from Stat005
start_date <- 1510531200  #2017-11-13
end_date <-  1510876800 #2017-11-17

bd_return_frequency_nov17<- do.call(rbind, lapply(seq(start_date, end_date, by=86400), function(date) {
  cat("Fetching webrequest data from ", as.character(date), "\n")
  
  query <- paste("
                 SELECT '",date,"' AS date, project_class AS project, user_agent_map['os_family'] as os_family, user_agent_map['browser_family'] as browser_family,
                 (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') 
                 - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400 AS days_till_next_access, 
                 COUNT (*) AS returns_each_day
                 FROM tbayer.webrequest_extract_bak
                 WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') = ", date, "
                 AND year = 2017
                 AND access_method = 'mobile web'
                 AND country_code = 'BD'
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
                 AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
                 GROUP BY (unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400, project_class, user_agent_map['os_family'], user_agent_map['browser_family']
                 ;") 
  cat(query)
  results <- wmf::query_hive(query)
  return(results)
}))

readr::write_rds(bd_return_frequency_nov17, "bd_return_frequency_nov17.rds", "gz")

#LOCAL
system("scp mneisler@stat5:/home/mneisler/bd_return_frequency_nov17.rds bd_return_frequency_nov17.rds")




