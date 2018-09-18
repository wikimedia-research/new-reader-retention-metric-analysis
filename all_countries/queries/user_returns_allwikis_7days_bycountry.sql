--average days until next access, 31 days of returns, mobile vs desktop, by country 2016-2018
-- for all countries
SELECT country_code as country, access_method, wmf_last_access as last_seen_date, 
-- average days until next access
SUM(((unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400) * view_count)/ SUM(view_count) AS avg_days_till_next_access 
FROM tbayer.webrequest_extract_bak 
WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') IS NOT NULL 
-- accessed between December 15, 2016 and April 29, 2018
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') >= 1481760000 -- 12/15/2016 @ 12:00am (UTC)
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') < 1532819808 -- 07/28/2018 @ 12:00am (UTC) 
AND ( (year = 2016) OR (year = 2017) OR (year = 2018) ) 
AND (access_method = 'desktop' OR access_method = 'mobile web') 
AND project_class = 'wikipedia' 
-- avg returns within 31 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 691200) -- 691200 seconds = 8 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
GROUP BY country_code, wmf_last_access, access_method;