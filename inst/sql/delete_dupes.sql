DELETE FROM
   events_extract 
WHERE
   event_date || '-' || event_id IN 
   (
      SELECT event_date || '-' || event_id
      FROM events_extract 
      GROUP BY
         event_id,
         event_date 
      HAVING
         COUNT(*) > 1
   )
   AND source_file LIKE '%-icews-events-1.tab';
