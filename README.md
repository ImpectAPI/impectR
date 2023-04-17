
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impectR

<!-- badges: start -->
<!-- badges: end -->

The goal of impectR is to provide an easy way for Impect Customers to
access data from the customer API. This API includes basic information
about competitions, competition iterations, and matches as well as event
data and aggregated scorings per player and position on match level.

## Installation

You can install the development version of impectR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Flosch1006/impectR")
```

## Getting started

Before accessing any data via the API, you will need to request a bearer
token for authorization. You can get this authorization token using the
following code snippet:

This access token is a requirement to use any of the functions that
requests data from the API. We recommend to first get a list of
competition iterations that are enabled for your account.

If any competition iteration you were expected to see is not listed,
please contact your sales representative. Now let’s assume you are
interested in data for 2022/23 season of the 1. Bundesliga
(competitionIteration = 518). The following snippet gets you a list of
matches for this competition and season:

The column `available` denotes whether a given match has been tagged by
Impect and the data is available to you. Let’s assume you are interested
in the FC Bayern München vs Borussia Dortmund game from April 1st 2023
(matchId = 84344). To request the event data for this game, run the
following code snippet:

You can access the aggregated scores per player and position for this
match in a similar way:

In case you wish to retrieve data for multiple matches, we suggest using
the following method to do so. Let’s also get the event data for the RB
Leipzig vs FSV Mainz 05 game (matchId = 84350) from the same day:

Please keep in mind that Impect enforces a rate limit of 8 requests per
second per user. As the function usually runs for about 2 seconds, there
shouldn’t be any issues but it might be a potential issue.
