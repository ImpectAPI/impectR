# impectR 2.0.0

## Major changes
* Modify package to support the IMPECT API V5 instead of V4
* Add `getPlayerIterationAverages()` function
* Add `getSquadIterationAverages()` function

# impectR 1.0.0

## Major changes
* Release package

## Minor improvements and bug fixes
* remove stop on error to enable `purrr::map` for functions that take an ID as argument

# impectR 0.1.1

## Minor improvements and bug fixes
* Replace `httr::GET()` and `httr::POST()` with `httr::RETRY()` to account for HTTP errors that might occur 

# impectR 0.1.0

## Major changes
* Added basic package build
* Added `getAccessToken()` function
* Added `getCompetitions()` function
* Added `getMatchplan()` function
* Added `getEventData()` function
* Added `getMatchsums()` function


## Minor improvements and bug fixes
* Added a `NEWS.md` file to track changes to the package.
* Added `README.md` & `README.Rmd`
* Added roxygen documentation for functions.
