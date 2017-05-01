## Test environments
* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
* There is a 404 url in the README, but this is just the CRAN badge url.

### From win-builder check:
Possibly mis-spelled words in DESCRIPTION:
  APIs (8:98)
  - this is not misspelt

The examples with CPU or elapsed time > 10s are due to the fact that these rely on downloading data from two APIs. We have reduced all examples to less than 5 seconds.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.


