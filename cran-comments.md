## Test environments

- local R installation (macOS), R 4.0.3
- win-builder (release)
- win-builder (devel)

On GitHub Actions:

- macOS R-release and R-devel
- Windows R-release
- Ubuntu R-release, R-oldrel, R 3.5

On R-Hub:

- Windows, R-devel
- Ubuntu, R-release
- Fedora, R-devel



## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new submission.




On R-hub Windows there is 1 ERROR:

> Package which is only available in source form, and may need compilation of C/C++/Fortran: 'utf8'
> These will not be installed. 

It does work without issues on win-builder and GitHub Actions Windows.

On R-hub, Ubuntu has a PREPERROR related to 'xml2' / 'libxml2-dev':

> #> Package libxml-2.0 was not found in the pkg-config search path.

It's declared as a system requirement and seems to be installed by the worker. Not sure what the problem is, but the icews package does build and pass R check on GitHub Actions Ubuntu. 
