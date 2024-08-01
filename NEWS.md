# impectPy 2.1.0

## Major changes
* add new attributes from dataVersion V4 to `getEvents()`

# impectR 2.0.4

## Minor changes
* add more player master data to `getPlayerMatchsums()` and `getPlayerIterationAverages()`


# impectR 2.0.3

## Minor changes
* fix bug in `getSquadMatchsums()` and `getPlayerMatchsums()` caused by duplicates
* fix bug in `getMatches()` function caused by addition of wyscoutIds
* 
* improve error handling for functions that use match ids as input
* improve error handling for `getMatches()` function
* add `playDuration` on player level to `getSquadMatchsums()`, `getPlayerMatchsums()`, `getPlayerIterationAverages()` and `getSquadIterationAverages()`
* fix bug in `getEvents()`, `getSquadMatchsums()`, `getPlayerMatchsums()`, `getPlayerIterationAverages()` and `getSquadIterationAverages()` that was caused by the addition of several new keys to the KPI endpoint

# impectR 2.0.2

## Minor changes
* fix bug in `getEvents()` function caused by querying data for multiple iterations of the same competition

# impectR 2.0.1

## Minor changes
* fix bug in `getPlayerIterationAverages()` function caused by user access rights
* fix bug in `getIterations()` function caused by addition of wyscoutIds
* fix bug in `getMatches()` function caused by addition of wyscoutIds

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
