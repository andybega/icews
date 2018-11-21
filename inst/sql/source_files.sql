/*
   Create a table containing the currently ingested source_files, and update
   it via triggers if anything changes in the events table.

   Point is to make get_db_state() and thus update_icews(), etc. faster
*/

CREATE TABLE source_files (
  name TEXT
);

INSERT INTO source_files (name)
SELECT DISTINCT(source_file) AS name FROM events;

CREATE trigger update_source_files_after_insert AFTER INSERT ON events
BEGIN
  DELETE FROM source_files;
  INSERT INTO source_files (name)
  SELECT DISTINCT(source_file) AS name FROM events;
END;

CREATE trigger update_source_files_after_delete AFTER DELETE ON events
BEGIN
  DELETE FROM source_files;
  INSERT INTO source_files (name)
  SELECT DISTINCT(source_file) AS name FROM events;
END;

CREATE trigger update_source_files_after_update AFTER UPDATE ON events
BEGIN
  DELETE FROM source_files;
  INSERT INTO source_files (name)
  SELECT DISTINCT(source_file) AS name FROM events;
END;
