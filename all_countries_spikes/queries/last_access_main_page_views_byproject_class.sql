
CREATE TABLE if not exists mneisler.last_access_main_page_views_project_class
(project string, project_class string, access_method string, country_code string, user_agent_map map<string,string>, wmf_last_access string, is_main_page boolean, 
  view_count bigint, hour int) partitioned by (year int, month int, day int) 


SET hive.mapred.mode='nonstrict';
SET hive.exec.dynamic.partition = 'true';
SET hive.exec.dynamic.partition.mode = 'nonstrict';
SET hive.exec.max.dynamic.partitions = 2000;
SET hive.exec.max.dynamic.partitions.pernode = 1000;
SET mapred.job.queue.name=nice;

SET parquet.compression              = SNAPPY;
SET mapred.reduce.tasks              = 16;

INSERT INTO TABLE mneisler.last_access_main_page_views_project_class
PARTITION(year, month, day)
    SELECT project, project_class, access_method, country_code, user_agent_map, wmf_last_access, is_main_page,
    COUNT(1) as view_count, hour, year, month, day
      FROM(
        SELECT
        normalized_host.project AS project,
        normalized_host.project_class AS project_class,
        access_method,
        geocoded_data['country_code'] AS country_code,
        user_agent_map,
        x_analytics_map['WMF-Last-Access'] as wmf_last_access,
        -- View to main pages and special pages where title was not extracted.
        CASE WHEN pageview_info['page_title'] IN('-', 'Main_Page' ) THEN TRUE ELSE FALSE END AS is_main_page,
         year,month, day, hour
FROM wmf.webrequest
    WHERE webrequest_source = 'text'
        AND (year = 2018 AND month = 8) 
        AND is_pageview = TRUE
        AND agent_type='user'
        AND (normalized_host.project_class = "mediawiki" OR normalized_host.project_class = "wikinews") -- project classes with spikes in August 
        AND COALESCE(normalized_host.project, '') != ''
        AND COALESCE(normalized_host.project_class, '') != '' --look at requests coming from select countries with peaks
) as refined_webrequests
    GROUP BY project, project_class, access_method, country_code, user_agent_map, wmf_last_access, is_main_page, year,month, day, hour
;