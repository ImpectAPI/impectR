######
#
# This function returns a dataframe that contains all events for a
# given match ID
#
######

getEventData <- function (match, token) {
  # require packages
  require(httr)
  require(jsonlite)
  require(tidyverse)

  # get match events
  response <-
    httr::GET(
      base::paste0(
        "https://api.impect.com/v4/customerapi/matches/",
        match,
        "/events"
      ),
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  data <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # convert to dataframe and add column with matchId
  events <- jsonlite::flatten(base::as.data.frame(data)) %>%
    dplyr::mutate(matchId = match)

  # fix column names using regex
  base::names(events) <-
    gsub("events.", "", base::names(events), perl = TRUE)
  base::names(events) <-
    gsub("\\.(.)", "\\U\\1", base::names(events), perl = TRUE)

  # get match data
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

  # get player data and convert to dataframe and convert id column to integer
  players <- base::sapply(base::c("playerId", "commonname"),
                          function(key)
                            base::unlist(c(
                              data$squadAway$players[[key]],
                              data$squadHome$players[[key]]
                            ))) %>%
    base::as.data.frame() %>%
    dplyr::mutate(playerId = base::as.integer(playerId))

  # get match info
  match_info <- data$competition

  # add basic info to match info
  for (key in c("matchId", "date", "dateTime")) {
    match_info[[key]] <- data[[key]]
  }

  # Iterate over sides
  sides <- c("squadHome", "squadAway")
  info_keys <- list("name", "squadId", "squadholderType")
  country_keys <- list("id", "name")

  for (side in sides) {
    for (key in info_keys) {
      # Add squad info
      side_info <- data[[side]][key]
      side_info_name <-
        base::paste0(side, base::toupper(substr(key, 1, 1)), base::substr(key, 2, 99))
      match_info[side_info_name] <- side_info
    }
    for (key in country_keys) {
      # Add country info
      side_country <- data[[side]][["country"]][key]
      side_country_name <-
        base::paste0(side,
                     "Country",
                     base::toupper(substr(key, 1, 1)),
                     base::substr(key, 2, 99))
      match_info[side_country_name] <- side_country
    }
  }

  # add playerName columns to events dataframe
  events <- events %>%
    dplyr::left_join(players, by = base::c("playerId" = "playerId")) %>%
    dplyr::rename(playerName = commonname) %>%
    dplyr::left_join(players, by = base::c("duelPlayerId" = "playerId")) %>%
    dplyr::rename(duelPlayerName = commonname) %>%
    dplyr::left_join(players, by = base::c("pressingPlayerId" = "playerId")) %>%
    dplyr::rename(pressingPlayerName = commonname) %>%
    dplyr::left_join(players, by = base::c("fouledPlayerId" = "playerId")) %>%
    dplyr::rename(fouledPlayerName = commonname) %>%
    dplyr::left_join(players, by = base::c("passReceiverPlayerId" = "playerId")) %>%
    dplyr::rename(passReceiverPlayerName = commonname)

  # add the values from the match_info list as new columns in the events dataframe
  for (name in base::names(match_info)) {
    events[[name]] <- base::rep(match_info[[name]], base::nrow(events))
  }

  # add squadName and currentAttackingSquadName
  events <- events %>%
    dplyr::mutate(
      squadName = base::ifelse(squadHomeSquadId == squadId,
                               squadHomeName,
                               squadAwayName),
      currentAttackingSquadName = base::ifelse(
        squadHomeSquadId == currentAttackingSquadId,
        squadHomeName,
        squadAwayName
      )
    )

  # get kpi list
  response <-
    httr::GET(
      "https://api.impect.com/v4/customerapi/kpis",
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  kpis <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # unnest scorings and full join with kpi list to ensure all kpis are present
  events <- events %>%
    tidyr::unnest(scorings, keep_empty = TRUE) %>%
    dplyr::full_join(kpis, by = "kpiId") %>%
    dplyr::arrange(kpiId) %>%
    dplyr::select(-kpiId) %>%
    tidyr::pivot_wider(
      names_from = "kpiName",
      values_from = "value",
      values_fn = sum,
      values_fill = 0
    ) %>%
    dplyr::filter(base::is.na(eventNumber) == FALSE)

  # rename some columns
  events <- events %>%
    dplyr::rename(
      attackingSquadId = currentAttackingSquadId,
      attackingSquadName = currentAttackingSquadName,
      playerPosition = playerPositionPosition,
      playerDetailedPosition = playerPositionDetailedPosition,
      duelType = duelDuelType
    )

  # reorder columns
  # define desired column order
  attribute_cols <- base::c(
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
    "squadHomeSquadId",
    "squadHomeName",
    "squadHomeCountryId",
    "squadHomeCountryName",
    "squadHomeSquadholderType",
    "squadAwaySquadId",
    "squadAwayName",
    "squadAwayCountryId",
    "squadAwayCountryName",
    "squadAwaySquadholderType",
    "eventNumber",
    "periodId",
    "gameTime",
    "gameTimeInSec",
    "duration",
    "squadId",
    "squadName",
    "attackingSquadId",
    "attackingSquadName",
    "phase",
    "playerId",
    "playerName",
    "playerPosition",
    "playerDetailedPosition",
    "actionType",
    "action",
    "bodyPart",
    "result",
    "startCoordinatesX",
    "startCoordinatesY",
    "startAdjCoordinatesX",
    "startAdjCoordinatesY",
    "startPackingZone",
    "startPitchPosition",
    "startLane",
    "endCoordinatesX",
    "endCoordinatesY",
    "endAdjCoordinatesX",
    "endAdjCoordinatesY",
    "endPackingZone",
    "endPitchPosition",
    "endLane",
    "opponents",
    "pressure",
    "distanceToGoal",
    "pxTTeam",
    "pxTOpponent",
    "pressingPlayerId",
    "pressingPlayerName",
    "distanceToOpponent",
    "passReceiverType",
    "passReceiverPlayerId",
    "passReceiverPlayerName",
    "passDistance",
    "passAngle",
    "shotDistance",
    "shotAngle",
    "shotTargetPointY",
    "shotTargetPointZ",
    "duelType",
    "duelPlayerId",
    "duelPlayerName",
    "fouledPlayerId",
    "fouledPlayerName"
  )

  # get list of kpi columns
  kpi_cols <- kpis$kpiName

  # create order
  order <- base::c(attribute_cols, kpi_cols)

  # reorder data
  events <- events[, order]

  # reorder rows
  events <- events %>%
    dplyr::arrange(matchId, eventNumber)

  return(events)
}
