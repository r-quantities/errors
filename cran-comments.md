## Patch release

The maintainer's address has changed. It is the same account, just a different
alias. I sent confirmation from the previous address to Uwe Ligges and 
CRAN@r-project.org on September 7, 2018.

This submission contains several bug fixes, but it is primarily motivated by the
recent delayed S3 registration mechanism added in R 3.6.0.

## Test environments

* Fedora 29 + GCC + clang (local), R 3.5.1
* Ubuntu 14.04 + GCC (on travis-ci), R 3.4.4, 3.5.1, devel
* win-builder, R devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are two downstream dependencies, quantities and constants, for which I'm
the maintainer too. Package quantities requires the same update.
