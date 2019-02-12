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

/*
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
*/


/*
    With the daily update repository, there can be source files from which no
    events are added to the DB because all events are already present. To avoid
    re-ingesting these files every time an update is run, because they are not
    in the stats source file column, keep track of them in a separate table.
*/

CREATE TABLE null_source_files (
  name TEXT PRIMARY KEY
);

CREATE trigger update_source_files_after_null_ingest AFTER INSERT ON null_source_files
BEGIN
  INSERT INTO source_files (name)
  SELECT name FROM null_source_files EXCEPT SELECT name FROM source_files;
END;

/*
    Also update source_files when the table is destroyed/recreated with
    update_stats()
*/

CREATE trigger add_null_ingests_to_source_files AFTER INSERT ON source_files
BEGIN
  INSERT INTO source_files (name)
  SELECT name FROM null_source_files EXCEPT SELECT name FROM source_files;
END;
