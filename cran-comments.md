## General comments

Uwe requested to provide a doi for the Saaty reference in the vignette. However, it's a book, so
I included the ISBN instead. Hope that works. I also added the same reference to the package
documentation.

Minor upgrade to avoid warning about deprecated function.

## Test environments

* linux / travis (release, devel, oldrel) -> ok
* Win / appveyor (devel 32, devel 64, release 64, stable, patched) -> ok

## R CMD check results

There were no ERRORs or WARNINGs. 

There was a NOTE:

Possibly mis-spelled words in DESCRIPTION:
  AHP (13:43)
  Saaty (13:58)
  
Both are not misspelled.

## Downstream dependencies

No downstream dependencies