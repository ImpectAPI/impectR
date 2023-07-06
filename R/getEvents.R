#' Return a dataframe that contains all events and kpi values for a set of given
#' match IDs
#'
#' @param matches IMPECT match ID or a list of match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all events and kpi values for a set of given
#' match IDs
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   events <- getEvents(matches = c(84248), token = token)
#' })
#' }
getEvents <- function (matches, token) {
  # check if match input is not a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # apply .eventAttributes function to list of matches
  events <-
    purrr::map_df(matches,
                  ~ .eventAttributes(match = ., token = token))

  # apply .eventScorings function to a set of matches
  scorings <-
    purrr::map_df(matches,
                  ~ .eventScorings(match = ., token = token))

  # apply .matchInfo function to a set of matches
  iterations <-
    purrr::map(matches, ~ .matchInfo(match = ., token = token)$iterationId) %>%
    base::unique()

  # apply .playerNames function to a set of iterations
  players <-
    purrr::map_df(iterations,
                  ~ .playerNames(iteration = ., token = token))

  # apply .squadNames function to a set of iterations
  squads <-
    purrr::map_df(iterations,
                  ~ .squadNames(iteration = ., token = token))

  # get kpi names
  kpis <- .kpis(token = token, scope = "event")

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations,
                  ~ getMatches(iteration = ., token = token))

  # get iterations
  iterations <- getIterations(token = token)

  # start merging dfs

  # merge events with squads
  events <- events %>%
    dplyr::left_join(dplyr::select(squads, squadId = id, squadName = name),
                     by = base::c("squadId" = "squadId")) %>%
    dplyr::left_join(
      dplyr::select(
        squads,
        squadId = id,
        currentAttackingSquadName = name
      ),
      by = base::c("currentAttackingSquadId" = "squadId")
    )

  # merge events with players
  events <- events %>%
    dplyr::left_join(
      dplyr::select(players, id, playerName = commonname),
      by = base::c("playerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, id, pressingPlayerName = commonname),
      by = base::c("pressingPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, id, fouledPlayerName = commonname),
      by = base::c("fouledPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, id, duelPlayerName = commonname),
      by = base::c("duelPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, id, passReceiverPlayerName = commonname),
      by = base::c("passReceiverPlayerId" = "id")
    )

  # merge with matchplan info
  events <- events %>%
    dplyr::left_join(matchplan, by = base::c("matchId" = "id"))

  # merge with competition info
  events <- events %>%
    dplyr::left_join(iterations,
                     by = base::c("iterationId" = "id"))

  # unnest scorings and full join with kpi list to ensure all kpis are present
  scorings <- scorings %>%
    dplyr::full_join(kpis, by = base::c("kpiId" = "id")) %>%
    dplyr::arrange("kpiId") %>%
    dplyr::select(-"kpiId") %>%
    tidyr::pivot_wider(
      names_from = "name",
      values_from = "value",
      values_fn = sum,
      values_fill = NA
    ) %>%
    dplyr::filter(!base::is.na("eventId"))

  # merge events and scorings
  events <- events %>%
    dplyr::left_join(
      scorings,
      by = c(
        "playerPosition" = "position",
        "playerId" = "playerId",
        "id" = "eventId"
      )
    )

  # rename some columns
  events <- events %>%
    dplyr::rename(
      attackingSquadId = "currentAttackingSquadId",
      attackingSquadName = "currentAttackingSquadName",
      duelType = "duelDuelType",
      dateTime = "scheduledDate",
      gameTime = "gameTimeGameTime",
      gameTimeInSec = "gameTimeGameTimeInSec",
      eventId = "id",
      eventNumber = "index"
    )

  # reorder columns
  # define desired column order
  attribute_cols <- base::c(
    "matchId",
    "dateTime",
    "competitionId",
    "competitionName",
    "competitionType",
    "iterationId",
    "season",
    "matchDayIndex",
    "matchDayName",
    "homeSquadId",
    "homeSquadName",
    "homeSquadCountryId",
    "homeSquadCountryName",
    "homeSquadType",
    "awaySquadId",
    "awaySquadName",
    "awaySquadCountryId",
    "awaySquadCountryName",
    "awaySquadType",
    "eventId",
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

  # create order
  order <- base::c(attribute_cols, kpis$name)

  # reorder data
  events <- events %>%
    dplyr::select(dplyr::all_of(order))

  # reorder rows
  events <- events %>%
    dplyr::arrange("matchId", "eventNumber")

  return(events)
}
