# icews 0.1.0.9000

Major rewrite of the download and synchronization code, with some **breaking changes**:

* Use package options via `options(x = y)` and `.Rprofile` instead of environment variables to keep track of the data directory path and global option settings. Helper functions like `setup_icews()` have been adjusted accordingly.
* Rename `download_icews` to `download_data`; the old version is still around but will be taken out at some point.
* Replace `sync_db` with `update` for general purpose use, and `sync_db_with_files()` for synchronizing the database with local data files without downloading any new ones. The `update()` function is setup to work with the path and other options. 

# icews 0.1.0

This is the initial package version with minimal functionality for what *I* need. Mileage might vary. 

* Added a `NEWS.md` file to track changes to the package.
* Added download functinality that will keep a local copy of the ICEWS event TSV files synchronized with dataverse, including updating outdated files.
* Added basic database functionality that will create a database containing downloaded events; will synchronize events so that they always reflect the latest version on DVN; however still relies on local copies of the raw files. 


