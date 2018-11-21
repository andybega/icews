/*
   This sets up a stats table, for now containing only the number of records in
   events (events_n), and triggers to UPDATE the row count when any records
   are added or deleted FROM events.

   Point is to make dr_icews faster when querying # of records
*/

CREATE TABLE IF NOT EXISTS stats (
  name TEXT,
  value INTEGER
);

INSERT INTO stats VALUES ('events_n', NULL);

UPDATE stats SET value = ( SELECT count(*) FROM events )
WHERE name=='events_n';

CREATE trigger update_events_n_after_insert AFTER INSERT ON events
BEGIN
  UPDATE stats SET value = ( SELECT count(*) FROM events )
  WHERE name=='events_n';
END;

CREATE trigger update_events_n_after_delete AFTER DELETE ON events
BEGIN
  UPDATE stats SET value = ( SELECT count(*) FROM events )
  WHERE name=='events_n';
END;
