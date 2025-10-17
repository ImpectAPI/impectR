library(tidyverse)
library(lubridate)
library(fuzzyjoin)
library(impectR)
devtools::load_all()

# define login credentials
username <- "username"
password <- "password"

# set env
host <- "https://api.release.impect.com"
token_url <- "https://login.impect.com/auth/realms/release/protocol/openid-connect/token"

# get access token
token <- getAccessToken(username, password, token_url = token_url)

# define iteration Id
iteration <- 1385

# get matches for iteration
matches <- getMatches(iteration, token, host = host)

# get squad model coefficients
coefficients <- getSquadCoefficients(iteration, token, host = host)

# prepare columns for merging
matches$date <- as.POSIXct(matches$scheduledDate, tz = "UTC")  # convert to UTC
matches$date <- as.Date(matches$date)  # normalize to date only
matches$homeSquadId <- as.integer(matches$homeSquadId)
matches$awaySquadId <- as.integer(matches$awaySquadId)
coefficients$date <- as.Date(coefficients$date)  # normalize to date only
coefficients$squadId <- as.integer(coefficients$squadId)

# sort by date
matches <- matches[order(matches$date), ]
coefficients <- coefficients[order(coefficients$date), ]

# merge to coefficients to get the most recent coefficient date for each match
matches <- fuzzyjoin::difference_inner_join(
  matches,
  coefficients %>%
    select(date) %>%
    distinct(date),
  by = "date",
  max_dist = Inf,
  distance_col = "date_diff"
) %>%
  rename("dateMatch" = "date.x", "dateCoef" = "date.y") %>%
  # filter for only coefficient dates from the past of each match
  filter(dateMatch >= dateCoef) %>%
  group_by(dateMatch) %>%
  # pick most recent coefficient before the match
  slice_min(date_diff) %>%
  ungroup() %>%
  select(-date_diff) %>%
  # join with competition-specific coefficients using most recent date
  left_join(
    coefficients %>%
      select(date, interceptCoefficient, homeCoefficient, competitionCoefficient) %>%
      distinct(),
    by = c("dateCoef" = "date")
  )

# merge squad-specific coefficients using the most recent date
matches <- matches %>%
  # merge for home squad
  left_join(
    coefficients %>%
      select(date, squadId, attackCoefficient, defenseCoefficient),
    by = c("dateCoef" = "date", "homeSquadId" = "squadId")
  ) %>%
  rename(
    "attackCoefficientHome" = "attackCoefficient",
    "defenseCoefficientHome" = "defenseCoefficient"
  ) %>%
  # merge for away squad
  left_join(
    coefficients %>%
      select(date, squadId, attackCoefficient, defenseCoefficient),
    by = c("dateCoef" = "date", "awaySquadId" = "squadId")
  ) %>%
  rename(
    "attackCoefficientAway" = "attackCoefficient",
    "defenseCoefficientAway" = "defenseCoefficient"
  )

# compute predictions
matches <- matches %>%
  mutate(
    predHome = exp(
      interceptCoefficient +
      homeCoefficient +
      competitionCoefficient +
      attackCoefficientHome +
      defenseCoefficientAway
    ),
    predAway = exp(
      interceptCoefficient +
      competitionCoefficient +
      attackCoefficientAway +
      defenseCoefficientHome
    )
  )
