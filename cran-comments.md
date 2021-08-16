## Test environments

- local R installation (macOS), R 4.1.1
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


This is a new submission; 2nd attempt, first was last October and see below for the comments.


## Comments on original submission (2020-10-21)

> Please always explain all acronyms in the description text.

Done

> Please provide a link to the used webservices to the description field of your DESCRIPTION file in the form
> <http:...> or <https:...>
> with angle brackets for auto-linking and no space after 'http:' and
> 'https:'.

Done

> Please put functions which download data in \donttest{}.

Instead of \donttest{}, I have commented out the relevant lines. They should never be run, neither for the example nor R check. 

> You write information messages to the console that cannot be easily suppressed.
> It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
> Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
> (except for print, summary, interactive functions)

I have added 'quiet' arguments to several functions, excluding interactive functions and print methods. 


## Misc. R check issues

With R-devel on winbuilder (R Under development (unstable) (2021-08-13 r80752)) there are recurring errors during the package tests. They all seem to be related to an error somewhere in tibble. 

This only happens with winbuilder, the Windows builds with GitHub Actions and R-Hub pass only with the 'New submission' NOTE. 


