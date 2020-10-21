
# icews 1.0.0

This is the first version released to CRAN. 

## Resumption of ICEWS updates with weekly data drops

The ICEWS project ceased updates in late 2019, but resumed again in May 2020. However, instead of daily updates for the most recent data, it switched to weekly updates for recent data. This required changes in the package. 

- The basic update for the new dataverse repo structure is documented in #54. For databases that were setup with the old daily structure, the only change that should be needed is to run `query_icews("delete from null_source_files;")` to remove some old references in a utility table in SQLite. For databases that were created after May 2020, everything is good. 

## Enhancements

- The path finders (`find_raw()`, `find_docs()`, and `find_db()`) now also take
and optional argument that is combined with the base path returned using `base::file.path()`. This makes it easier to get the full path for files within the data directories. 

## Minor fixes, etc. 

- Fix bugs in the non-standard 2017 events file; non-standard file name ("Events.2017.20200602.tab.zip") and dates formatted like "12/1/2017" (#57).

## Fixes, etc. for the old daily repo structure

Some of the earlier commits in 2019, when the updates were still daily, fixed various minor issues:

- Fix an update error due to a duplicate filename ("20190309-icews-events.zip") on dataverse (#45). The non-unique dataverse file label is now converted to a unique local file name (see `normalize_label()`) by appending "part1", "part2", etc. as needed. This local file name is used to track state between the local downloaded files and/or database and dataverse. Download now occurs through the integer file ID instead of the file label.
- Fix ingest file to DB problem with duplicate events (#46). A regex in `write_data_to_db` did not recognize a data file ending with "-1.tab" as a daily data file, and thus skipped the duplicate events check that is done for daily data files only.
- Fix missing source files in DB state (#47). Source files that all duplicate events, i.e. no events that will end up in the "events" table, are saved in "null_source_files". This was not being used to update the main "source_files" table on which the state is based on. 
- Fix an issue in `dr_icews()` that would erroneously indicate the need to sync local files and database. This was due to the internal changes in v0.2.0. 


# icews 0.2.0 (2019-02-12)

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


