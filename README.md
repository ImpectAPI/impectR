
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impectR <img src="https://github.com/ImpectAPI/logos/blob/main/impectR.png" align="right" height="139" />

<!-- badges: start -->

<!-- badges: end -->

A package provided by: Impect GmbH

Version: v2.5.1

**Updated: October 15th 2025**

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

You can install the latest version of impectR from CRAN with:

``` r
install.packages("impectR")
```

You can also install it from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ImpectAPI/impectR@v2.5.1")
```

## Usage

### Getting started

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

### Retrieve Basic Information

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
Impect and the data is available to you.

### Retrieve Match Level Data

Let’s assume you are interested in the FC Bayern München vs Borussia
Dortmund game from April 1st 2023 (matchId = 84344) and want to retrieve
event level data as well as team formation, starting position and
substitution data. As the functions allows for multiple games to be
requested at once, we need to wrap the matchId into a list. Hence, to
request data for this game, run the following code snippet:

``` r
# define matches to get event data for
matchIds <- c(84344)

# get event data for match
events <- getEvents(
  matches = matchIds,
  token = token,
  include_kpis = TRUE,
  include_set_pieces = TRUE
  )

# get match info
formations = getFormations(matches, token)
substitutions = getSubstitutions(matches, token)
starting_positions = getStartingPositions(matches, token)

# print first few rows from events dataframe to console
head(events)
```

You can access the aggregated KPIs, scores and ratios per player and
position or per squad for this match in a similar way. You can also find
more detailed data around set piece situations within our API. Also, we
provide you with IMPECT scores and ratios that you might know from our
Scouting and Analysis portals. On player level, these are calculated
across positions which is why you have to supply the function with a
list of positions your want to retrieve data for:

``` r
# define matches to get further data for
matchIds <- c(84344)

# get set piece data including KPI aggregates
setPieces <- getSetPieces(matches = matchIds, token = token)


# get kpi matchsums for match per player and position
playerMatchsums <- getPlayerMatchsums(matches = matchIds, token = token)

# get kpi matchsums for match per squad
squadMatchsums <- getSquadMatchsums(matches = matchIds, token = token)

# define positions to get scores aggregated by
positions <- c("LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER")

# get player scores and ratios for match and positions per player
playerMatchScores <- getPlayerMatchScores(
  matches = matchIds, 
  token = token,
  positions = positions  # optional
)

# get squad scores and ratios for match per squad
squadMatchScores <- getSquadMatchScores(matches = matchIds, token = token)
```

In case you wish to retrieve data for multiple matches, we suggest using
the following method to do so in order to minimize the amount of
requests sent to the API. Let’s also get the event data for the RB
Leipzig vs FSV Mainz 05 game (matchId = 84350) from the same day:

``` r
# define list of matches
matchIds <- c(84344, 84350)

# get event data for matches
events <- getEvents(
    matches = matchIds, 
    token = token,
    include_kpis = True,
    include_set_pieces = True
)

# get set piece data including KPI aggregates
setPieces <- getSetPieces(ip.getSetPieces(matches = matchIds, token = token)
                            
# get matchsums for matches per player and position
playerMatchsums <- getPlayerMatchsums(matches = matchIds, token = token)

# get matchsums for matches per squad
squadMatchsums <- getSquadMatchsums(matches = matchIds, token = token)

# define positions to get scores aggregated by
positions <- c("LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER")

# get player scores and ratios for match and positions per player
playerMatchScores <- 
  getPlayerMatchScores(matches = matchIds, positions = positions, token = token)

# get squad scores and ratios for match per squad
squadMatchScores <- getSquadMatchScores(matches = matchIds, token = token)
```

### Retrieve Iteration Level Data

Starting from API version V5, we also offer an endpoint to get KPI
average values per iteration on player as well as squad level. These
averages are calculated by dividing the kpi sum of all individual
matches by the sum of matchShares the player accumulated at a given
position. On a team level we divide the score by the amount of matches
played by the team. Also, we provide you with IMPECT scores and ratios
that you might know from our Scouting and Analysis portals. On player
level, these are calculated across positions which is why you have to
supply the function with a list of positions your want to retrieve data
for. Let’s assume you were interested in wingbacks in the 2022/2023
Bundesliga season, then you could use this code snippet:

``` r
# define iteration ID
iteration <- 518

# define positions to get scores aggregated by
positions <- c("LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER")

# get player kpi averages for iteration
playerIterationAverages <-
  getPlayerIterationAverages(iteration = iteration, token = token)

# get squad kpi averages for iteration
squadIterationAverages <-
  getSquadIterationAverages(iteration = iteration, token = token)

# get player scores and ratios for iteration and positions
playerIterationScores <- 
  getPlayerIterationScores(
    iteration = iteration,
    token = token,
    positions = positions. #optional
  )

# get squad scores and ratios for iteration
squadIterationScores <- 
  getSquadIterationScores(iteration = iteration, token = token)
```

The squad rating values that you can find on the league ranking in the
Scouting portal can also be retrieved from the API. In addition, we also
provide you with the more detailed squad coefficients that can be used
to make match predictions. See [this example
script](https://github.com/ImpectAPI/impectPy/blob/release/examples/predict_matches.ipynb)
for further details.

``` r
# get squad rating for iteration
squadRatings <- getSquadRatings(iteration = iteration, token = token)

# get squad coefficients for iteration
squadCoefficients <- getSquadCoefficients(iteration = iteration, token = token)
```

You can now also retrieve the positional profile scores for players via
our API. This includes profiles that you created through the scouting
portal. The function requires a positional input that determines which
matchShares to consider when computing the scores. In the below example,
all matchShares that a player played as either a left back or a right
back are included for profile score calculation.

``` r
# define iteration ID
iteration = 518

# define positions to get scores aggregated by
positions = ["LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER"]

# get player profile scores
playerProfileScores <- 
  ip.getPlayerProfileScores(
    iteration = iteration,
    positions = positions,
    token = token
  )
```

Please keep in mind that Impect enforces a rate limit of 10 requests per
second per user. A token bucket logic has been implemented to restrict
the amount of API calls made on the client side already. The rate limit
is read from the first limit policy sent back by the API, so if this
limit increases over time, this package will act accordingly.

## Final Notes

Further documentation on the data and explanations of variables can be
found in our [glossary](https://glossary.impect.com/).
