
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impectR <img src="https://github.com/ImpectAPI/logos/blob/main/impectR.png" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/Flosch1006/impectR/actions/workflows/check-release.yaml/badge.svg)](https://github.com/Flosch1006/impectR/actions/workflows/check-release.yaml)
<!-- badges: end -->

A package provided by: Impect GmbH

Updated: April 25th 2023

**Supported API Version: V4**  
For older versions, please see list below:

- API V3: not supported by this package

The goal of the impectR package is to provide an easy way for Impect
Customers to access data from the customer API. This API includes basic
information about competitions, competition iterations, and matches as
well as event data and aggregated scorings per player and position on
match level.

## Installation

You can install the developmental version of impectR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ImpectAPI/impectR@v1.0.0")
```

## Getting started

Before accessing any data via our API, you will need to request a bearer
token for authorization. You can get this authorization token using the
following code snippet:

``` r
library(impectR)

# define login credentials
username <- "yourUsername"
password <- "yourPassword"

# get access token
token <- getAccessToken(username = username, password = password)
```

This access token is a requirement to use any of the functions that
requests data from the API. We recommend to first get a list of
competition iterations that are enabled for your account.

``` r
# get list of competition iterations
competitions <- getCompetitions(token = token)

# print competition iterations to console
competitions
```

If any competition iteration you were expected to see is not listed,
please contact your sales representative. Now let’s assume you are
interested in data for 2022/23 season of the 1. Bundesliga
(competitionIteration = 518). The following snippet gets you a list of
matches for this competition and season:

``` r
# get match plan for competition iteration
matchplan <-
  getMatchplan(competitionIterationId = 518, token = token)

# print match to console
matchplan
```

The column `available` denotes whether a given match has been tagged by
Impect and the data is available to you. Let’s assume you are interested
in the FC Bayern München vs Borussia Dortmund game from April 1st 2023
(matchId = 84344). To request the event data for this game, run the
following code snippet:

``` r
# define match ID
matchId <- 84344

# get event data for match
events <- getEventData(match = matchId, token = token)

# print first few rows from events dataframe to console
head(events)
```

You can access the aggregated scores per player and position for this
match in a similar way:

``` r
# define match ID
matchId <- 84344

# get matchsums for match
matchsums <- getMatchsums(match = matchId, token = token)

# print first few rows from matchsums dataframe to console
head(matchsums)
```

In case you wish to retrieve data for multiple matches, we suggest using
the following method to do so. Let’s also get the event data for the RB
Leipzig vs FSV Mainz 05 game (matchId = 84350) from the same day:

``` r
# define list of matches
match_ids <- c(84344, 84350)

# apply getEventData function to a set of matchIds
events <-
  purrr::map_df(purrr::map(match_ids, ~ getEventData(match = ., token = token)),
                as.data.frame)

# apply getMatchsums function to a set of matchIds
matchsums <-
  purrr::map_df(purrr::map(match_ids, ~ getMatchsums(match = ., token = token)),
                as.data.frame)
```

Please keep in mind that Impect enforces a rate limit of 8 requests per
second per user. As the function usually runs for about 2 seconds, there
shouldn’t be any issues but it might be a potential issue.

## Final Notes

Further documentation on the data and explanations of variables can be
found in our [glossary](https://glossary.impect.com/).
