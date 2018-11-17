--average days until next access, 31 days of returns, mobile vs desktop, by project class 2016-2018
-- for all countries
SELECT project_class, access_method, wmf_last_access as last_seen_date, 
-- average days until next access
SUM(((unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400) * view_count)/ SUM(view_count) AS avg_days_till_next_access 
FROM tbayer.webrequest_extract_bak 
WHERE unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') IS NOT NULL 
AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time. 
-- accessed between December 15, 2016 and August 27, 2018
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') >= 1481760000 -- 12/15/2016 @ 12:00am (UTC)
AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') < 1535328000 -- 8/27/2018 12:00:00 AM (UTC)
AND ( (year = 2016) OR (year = 2017) OR (year = 2018 and month <=9) )
AND (access_method = 'desktop' OR access_method = 'mobile web') 
-- avg returns within 31 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
GROUP BY project_class, wmf_last_access, access_method;