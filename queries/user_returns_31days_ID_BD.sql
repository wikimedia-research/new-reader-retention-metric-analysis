--average days until next access, 31 days of returns, mobile vs desktop, by country 2016-2018
-- for all regional eqsin countries. 

SELECT country_code as country, access_method, wmf_last_access as last_seen_date, 
-- average days until next access
SUM(((unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400) * view_count)/ SUM(view_count) AS avg_days_till_next_access 
FROM tbayer.webrequest_extract_bak 
WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') IS NOT NULL 
-- accessed between December 15, 2016 and July 7, 2018
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') >= 1481760000 -- 12/15/2016 @ 12:00am (UTC)
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') < 1532736000 -- 07/28/2018 @ 12:00am (UTC) Table has data until August 31, 2018 right now
AND ( (year = 2016) OR (year = 2017) OR (year = 2018) ) 
AND (access_method = 'desktop' OR access_method = 'mobile web') 
AND project_class = 'wikipedia' 
--isolate to Indonesia and Bangladesh countries 
AND country_code IN ("BD", "ID")
-- avg returns within 31 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
GROUP BY country_code, wmf_last_access, access_method;