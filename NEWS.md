# icews 0.2.0.9000

Addresses various relatively minor issues that are still related to the big issue with new non-standard file names in the daily DVN repo. 

- Fix an update error due to a duplicate filename ("20190309-icews-events.zip") on dataverse (#45). The non-unique dataverse file label is now converted to a unique local file name (see `normalize_label()`) by appending "part1", "part2", etc. as needed. This local file name is used to track state between the local downloaded files and/or database and dataverse. Download now occurs through the integer file ID instead of the file label.
- Fix ingest file to DB problem with duplicate events (#46). A regex in `write_data_to_db` did not recognize a data file ending with "-1.tab" as a daily data file, and thus skipped the duplicate events check that is done for daily data files only.
- Fix missing source files in DB state (#47). Source files that all duplicate events, i.e. no events that will end up in the "events" table, are saved in "null_source_files". This was not being used to update the main "source_files" table on which the state is based on. 
- Fix an issue in `dr_icews()` that would erroneously indicate the need to sync local files and database. This was due to the internal changes in v0.2.0. 

# icews 0.2.0 (2018-02-12)

Major rewrite of the download and synchronization code, with some **breaking changes**.

A major non-breaking change is that the second ICEWS dataverse repository, which has been providing daily updates since November 2018, has been integrated. `update_icews()` will seamlessly pull/update data from both repositories. 

Big changes, including breaking changes:

* Use package options via `options(x = y)` and `.Rprofile` instead of environment variables to keep track of the data directory path and global option settings. Helper functions like `setup_icews()` have been adjusted accordingly.
* Rename `download_icews` to `download_data`; the old version is still around but will be taken out at some point.
* Rename `query` to `query_icews` to avoid ambiguity. 
* Replace `sync_db` with `update_icews` for general purpose use, and `sync_db_with_files()` for synchronizing the database with local data files without downloading any new ones. The `update_icews()` function is setup to work with the path and other options. 
* Column names in the database are always, and with raw files by default, normalized by lower-casing and replacing all spaces with underscores, e.g. "Event ID" to "event_id". This is to make working in R and SQL with the events data easier, i.e. no need to backquote names. 

Other changes/additions:

* Add `icews_to_gwcode` to translate ICEWS country names to their respective Gleditsch & Ward country code. 
* Add a downloader for documentation files, `download_docs`. Find out in which directory the doc files are with `find_docs`. 
* Change `read_icews` to also work if a database backend is used, previously only for reading from files. 
* Change `cameo_codes` data to include columns for quad and penta categories. 


# icews 0.1.0 (2018-10-26)

This is the initial package version with minimal functionality for what *I* need. Mileage might vary. 

* Added a `NEWS.md` file to track changes to the package.
* Added download functinality that will keep a local copy of the ICEWS event TSV files synchronized with dataverse, including updating outdated files.
* Added basic database functionality that will create a database containing downloaded events; will synchronize events so that they always reflect the latest version on DVN; however still relies on local copies of the raw files. 


