
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impectR <img src="https://github.com/ImpectAPI/logos/blob/main/impectR.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

A package provided by: Impect GmbH

Version: v2.1.0

**Updated: August 1st 2024**

------------------------------------------------------------------------

**Supported API Version: V5**  
For older versions, please see list below:

- API V4: [impectR
  v1.0.0](https://github.com/ImpectAPI/impectR/tree/v1.0.0)
- API V3: not supported by this package

------------------------------------------------------------------------

## Introduction

The goal of the impectR package is to provide an easy way for Impect
Customers to access data from the customer API. This API includes basic
information about competitions, competition iterations, and matches as
well as event data and aggregated scorings per player and position on
match and season level.

## Installation

You can install the latest version of impectR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ImpectAPI/impectR@v2.1.0")
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
# get list of iterations
iterations <- getIterations(token = token)

# print iterations to console
iterations
```

If any iteration you were expected to see is not listed, please contact
your sales representative. Now let’s assume you are interested in data
for 2022/23 season of the 1. Bundesliga (iteration = 518). The following
snippet gets you a list of matches for this iteration:

``` r
# get matches for iteration
matches <- getMatches(iteration = 518, token = token)

# print matches to console
matches
```

The column `available` denotes whether a given match has been tagged by
Impect and the data is available to you. Let’s assume you are interested
in the FC Bayern München vs Borussia Dortmund game from April 1st 2023
(matchId = 84344). As the function allow for multiple games to be
requested at once, we need to wrap the matchId into a list. Hence, to
request the event data for this game, run the following code snippet:

``` r
# define matches to get event data for
matchIds <- c(84344)

# get event data for match
events <- getEvents(matches = matchIds, token = token)

# print first few rows from events dataframe to console
head(events)
```

You can access the aggregated scores per player and position or per
squad for this match in a similar way:

``` r
# define matches to get matchsums for
matchIds <- c(84344)

# get matchsums for match per player and position
playerMatchsums <- getPlayerMatchsums(matches = matchIds, token = token)

# get matchsums for match per squad
squadMatchsums <- getSquadMatchsums(matches = matchIds, token = token)

# print first few rows from playerMatchsums dataframe to console
head(playerMatchsums)

# print first few rows from squadMatchsums dataframe to console
head(squadMatchsums)
```

In case you wish to retrieve data for multiple matches, we suggest using
the following method to do so in order to minimize the amount of
requests sent to the API. Let’s also get the event data for the RB
Leipzig vs FSV Mainz 05 game (matchId = 84350) from the same day:

``` r
# define list of matches
matchIds <- c(84344, 84350)

# get event data for matches
events <- getEvents(matches = matchIds, token = token)

# get matchsums for matches per player and position
playerMatchsums <- getPlayerMatchsums(matches = matchIds, token = token)

# get matchsums for matches per squad
squadMatchsums <- getSquadMatchsums(matches = matchIds, token = token)
```

Starting from API version V5, we also offer an endpoint to get KPI
average values per iteration on player as well as squad level. These
averages are calculated by dividing the kpi sum of all individual
matches by the sum of matchShares the player accumulated at a given
position. On a team level we divide the score by the amount of matches
played by the team. Let’s assume you were interested in the 2022/2023
Bundesliga season, then you could use this code snippet:

``` r
# define iteration ID
iteration <- 518

# get player averages for iteration
playerIterationAverages <-
  getPlayerIterationAverages(iteration = iteration, token = token)

# get squad averages for iteration
squadIterationAverages <-
  getSquadIterationAverages(iteration = iteration, token = token)
```

Please keep in mind that Impect enforces a rate limit of 10 requests per
second per user. A token bucket logic has been implemented to restrict
the amount of API calls made on the client side already. The rate limit
is read from the first limit policy sent back by the API, so if this
limit increases over time, this package will act accordingly.

## Final Notes

Further documentation on the data and explanations of variables can be
found in our [glossary](https://glossary.impect.com/).
