# impectR 2.4.3

## Minor Changes
* Fix bug in `getSubstitutions()` that occurred if one team did not substitute
* Fix bug in `getPlayerProfileScores()` that occurred if more than one position was supplied
* Fix bug in `getPlayerIterationScores()` that occurred if more than one position was supplied
* Fix bug in `getEvents()` that prevented the column `duelPlayerName`from properly being populated

# ImpectR 2.4.2

## Minor Changes
* Fix error in `getSubstitutions()` that occured if one team did not substitute

# ImpectR 2.4.1

## Minor Changes
* Fix error in `getSquadRatings()` function

# ImpectR 2.4.0

## Major Changes
* Add new function `getSquadRatings()` to retrieve squad ratings
* Add new function `getFormations()` to retrieve squad formations on match level
* Add new function `getStartingPositions()` to retrieve starting positions on match level
* Add new function `getSubstitutions()` to retrieve substitutions on match level

## Minor changes
* Add attribute `inferredSetPiece` to `getEvents()` function
* Add ID Mappings for HeimSpiel, Wyscout and Skillcorner to several functions
* Add proper error handling for iterations/matches with squads without players at given positions
* Remove excessive usage of helper functions
* Clean Up Comments in Code
* Fix join from events to set pieces in `getEvents()` function

# impectR 2.3.2

## Major Changes
* Add function `getSquadRatings()` to retrieve squad ratings

## Minor changes
* Add attribute `inferredSetPiece`to `getEvents()` function
* Add ID mappings to other providers (HeimSpiel, SkillCorner, Wyscout) to several functions
* Fix bug in `getPlayerProfileScores()`that occurred if no player played at given positions for any squad in the given iteration.
* Improved error handling for all functions including a `positions` argument

# impectR 2.3.1

## Minor changes
* Fix error in `getEvents()` for matches without any dribble objects (data version V2 or V3)

# impectR 2.3.0

## Major changes
* Add new `getSetPieces()` function
* Add set piece data to `getEvents()`
* Add arguments to `getEvents()` function that control the addition of KPIs and set piece data to the events dataframe

## Minor changes
* Fix error in `getEvents()` for matches without any tagged duels

# impectR 2.2.0

## Major changes
* add new functions to query the new customer API endpoints that provide ratios & scores

## Minor changes
* switch from German country name to FIFA country name
* Update to readme structure

# impectR 2.1.0

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
