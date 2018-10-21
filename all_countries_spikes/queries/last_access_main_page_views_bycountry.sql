
SET hive.mapred.mode='nonstrict';
SET hive.exec.dynamic.partition = 'true';
SET hive.exec.dynamic.partition.mode = 'nonstrict';
SET hive.exec.max.dynamic.partitions = 2000;
SET hive.exec.max.dynamic.partitions.pernode = 1000;
SET mapred.job.queue.name=nice;

SET parquet.compression              = SNAPPY;
SET mapred.reduce.tasks              = 16;

INSERT INTO TABLE mneisler.last_access_main_page_views_by_country
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
        -- View to main pages and pages where title was not extracted.
        CASE WHEN pageview_info['page_title'] IN('-', 'Main_Page', -- English
          '%E0%A6%AA%E0%A7%8D%E0%A6%B0%E0%A6%A7%E0%A6%BE%E0%A6%A8_%E0%A6%AA%E0%A6%BE%E0%A6%A4%E0%A6%BE', --Bengali
          'Halaman_Utama', --Indonesian
          '%E0%AE%AE%E0%AF%81%E0%AE%A4%E0%AE%B1%E0%AF%8D_%E0%AE%AA%E0%AE%95%E0%AF%8D%E0%AE%95%E0%AE%AE%E0%AF%8D', --Tamil (common project in India)
          '%E0%B4%AA%E0%B5%8D%E0%B4%B0%E0%B4%A7%E0%B4%BE%E0%B4%A8_%E0%B4%A4%E0%B4%BE%E0%B5%BE', -- Malayalam (common project in India)
          'Wikipédia:Accueil_principal', --French
          '%E3%83%A1%E3%82%A4%E3%83%B3%E3%83%9A%E3%83%BC%E3%82%B8', --Japanese
          'Wikipedia:Portada' --Spanish
          ) THEN TRUE ELSE FALSE END AS is_main_page,
         year,month, day, hour
FROM wmf.webrequest
    WHERE webrequest_source = 'text'
        AND (year = 2018 AND month = 9) 
        AND is_pageview = TRUE
        AND agent_type='user'
        AND COALESCE(normalized_host.project, '') != ''
        AND COALESCE(normalized_host.project_class, '') != ''
 AND geocoded_data['country_code'] IN ('BD', 'ID', 'FR', 'IN', 'JP', 'ES') --look at requests coming from select countries with peaks
 AND normalized_host.project_class = 'wikipedia' --spikes occured only from wikipedia projects
) as refined_webrequests
    GROUP BY project, project_class, access_method, country_code, user_agent_map, wmf_last_access, is_main_page, year,month, day, hour
;