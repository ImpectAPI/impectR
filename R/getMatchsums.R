######
#
# This function returns a dataframe that contains all player matchsums for a
# given match ID
#
######

getMatchsums <- function (match, token) {
  # require packages
  require(httr)
  require(jsonlite)
  require(tidyverse)

  # get match info
  response <-
    httr::GET(
      base::paste0("https://api.impect.com/v4/customerapi/matches/", match),
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  data <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # define function to extract players
  extract_players <- function(dict, side) {
    # convert side data to df
    temp <-
      jsonlite::flatten(jsonlite::fromJSON(jsonlite::toJSON(data[side][[1]]$players),
                                           simplifyDataFrame = TRUE),
                        recursive = FALSE)

    # detect non-empty playTime column
    list <- base::sapply(temp$playTime, length) > 0

    # filter and add context data
    temp <- temp[list,] %>%
      dplyr::mutate(
        matchId = data$matchId,
        date = data$date,
        dateTime = data$dateTime,
        competition = data$competition$competition,
        competitionId = data$competition$competitionId,
        competitionType = data$competition$competitionType,
        competitionIterationId = data$competition$competitionIterationId,
        competitionIterationName = data$competition$competitionIterationName,
        competitionIterationStepId = data$competition$competitionIterationStepId,
        competitionIterationStepName = data$competition$competitionIterationStepName
      )

    # filter for only players with registered playTime
    temp <-
      dplyr::select(
        temp,
        "matchId",
        "date",
        "dateTime",
        "competition",
        "competitionId",
        "competitionType",
        "competitionIterationId",
        "competitionIterationName",
        "competitionIterationStepId",
        "competitionIterationStepName",
        "playerId",
        "commonname",
        "playTime"
      )

    return(temp)
  }

  # apply extract_players function to home and away squad
  players <-
    purrr::map_df(purrr::map(
      base::c("squadHome", "squadAway"),
      ~ extract_players(data, .)
    ),
    base::as.data.frame)

  # unnest playTime column
  players <- players %>%
    tidyr::unnest(playTime)

  # get match sums
  response <-
    httr::GET(
      base::paste0(
        "https://api.impect.com/v4/customerapi/matches/",
        match,
        "/matchsums"
      ),
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  stop_for_status(response)

  # get data from response
  match_sums <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # get match sums
  response <-
    httr::GET(
      base::paste0("https://api.impect.com/v4/customerapi/kpis"),
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  kpis <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # convert to df
  kpis <- base::data.frame(kpis)

  # define function to extract matchsums
  extract_matchsums <- function(dict, side) {
    # convert side data to df
    temp <-
      jsonlite::flatten(jsonlite::fromJSON(
        jsonlite::toJSON(match_sums[side][[1]]$players),
        simplifyDataFrame = TRUE
      ),
      recursive = FALSE)

    # detect non-empty scorings column
    list <- base::sapply(temp$scorings, length) > 0

    # filter and add context data
    temp <- temp[list,]

    # unnest scorings
    temp <- temp %>%
      tidyr::unnest(scorings) %>%
      dplyr::select(playerId, detailedPosition, kpiId, totalValue) %>%
      # join with kpis to ensure all kpiIds are present and order by kpiId
      dplyr::full_join(kpis, by = "kpiId") %>%
      dplyr::arrange(kpiId, playerId) %>%
      # drop kpiId column
      dplyr::select(-kpiId) %>%
      # pivot data
      tidyr::pivot_wider(
        names_from = kpiName,
        values_from = totalValue,
        values_fill = 0,
        values_fn = base::sum
      ) %>%
      # filter for non NA columns that were created by full join
      dplyr::filter(base::is.na(playerId) == FALSE)

    return(temp)
  }

  # apply extract_matchsums function to home and away squad
  match_sums <-
    purrr::map_df(purrr::map(
      base::c("squadHome", "squadAway"),
      ~ extract_matchsums(data, .)
    ),
    base::as.data.frame)


  # merge matchsums and players data
  data <-
    dplyr::inner_join(players, match_sums, by = base::c("playerId", "detailedPosition"))

  return(data)
}
