SET hive.mapred.mode='nonstrict';
SET hive.exec.dynamic.partition = 'true';
SET hive.exec.dynamic.partition.mode = 'nonstrict';
SET hive.exec.max.dynamic.partitions = 2000;
SET hive.exec.max.dynamic.partitions.pernode = 1000;
SET mapred.job.queue.name=nice;

SET parquet.compression              = SNAPPY;
SET mapred.reduce.tasks              = 16;


SET hive.exec.compress.output=true;
SET mapreduce.output.fileoutputformat.compress.codec=org.apache.hadoop.io.compress.GzipCodec;


ADD JAR /usr/lib/hive-hcatalog/share/hcatalog/hive-hcatalog-core.jar;


CREATE EXTERNAL TABLE IF NOT EXISTS mneisler.avg_returns_31_days (  
	`country` string, 
	`access_method` string, 
	`dt` string, --reference to last seen date
	`avg_days_till_next_access` bigint
) 

ROW FORMAT SERDE 'org.apache.hive.hcatalog.data.JsonSerDe'
STORED AS TEXTFILE;



INSERT OVERWRITE TABLE mneisler.avg_returns_31_days
SELECT 
	from_unixtime(unix_timestamp(wmf_last_access,'dd-MMM-yyyy'), CONCAT(   ----Convert wmf_last_access (02-Jan-2019) to "2019-02-06T00:00:00Z" 
        LPAD(year, 4, '0'), '-',
        LPAD(month, 2, '0'), '-',
        LPAD(day, 2, '0'), 'T',
        LPAD(hour, 2, '0'), ':00:00Z')) AS dt,
	country_code as country, 
	access_method as access_method, 
	SUM(((unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') - unix_timestamp(wmf_last_access, 'dd-MMM-yyyy'))/86400) * view_count)/ SUM(view_count) AS avg_days_till_next_access
FROM tbayer.webrequest_extract_bak 
WHERE 
	unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') IS NOT NULL 
	AND length(wmf_last_access) < 15 -- length check to remove malformed data around that time. 
-- accessed between December 15, 2016 and December 30, 2018
	AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') >= 1481760000 -- 12/15/2016 @ 12:00am (UTC)
	AND unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') < 1546128000 -- 12/30/2018  @ 12:00:00 AM (UTC)
	AND ( (year = 2016) OR (year = 2017) OR (year = 2018) OR (year = 2019 and month <=1) )
	AND (access_method = 'desktop' OR access_method = 'mobile web') 
	AND project_class = 'wikipedia' 
-- avg returns within 31 days
	AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') < (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 2764800) -- 2764800 seconds = 32 days
	AND unix_timestamp(CONCAT(year,'-',LPAD(month,2,'0'),'-',LPAD(day,2,'0')), 'yyyy-MM-dd') >= (unix_timestamp(wmf_last_access, 'dd-MMM-yyyy') + 86400) -- 86400 seconds = 1 day
	GROUP BY country_code, access_method, from_unixtime(unix_timestamp(wmf_last_access,'dd-MMM-yyyy'), CONCAT(   ----Convert wmf_last_access (02-Jan-2019) to "2019-02-06T00:00:00Z" 
        LPAD(year, 4, '0'), '-',
        LPAD(month, 2, '0'), '-',
        LPAD(day, 2, '0'), 'T',
        LPAD(hour, 2, '0'), ':00:00Z'))
	;

