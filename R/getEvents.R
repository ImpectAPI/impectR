#' Return a dataframe that contains all events and kpi values for a set of given
#' match IDs
#'
#' @param matches 'IMPECT' match ID or a list of match IDs
#' @param token bearer token
#' @param include_kpis include KPIs in event data
#' @param include_set_pieces include additional set piece data in event data
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing all events and kpi values for a set of given
#' match IDs
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(events <- getEvents(
#'   matches = c(0, 1),
#'   token = "invalid",
#'   include_kpis = T,
#'   include_set_pieces = F
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' events <- getEvents(
#'   matches = c(84248, 158150),
#'   token = "yourToken",
#'   include_kpis = T,
#'   include_set_pieces = F
#' )
#' }
getEvents <- function (
    matches,
    token,
    include_kpis = TRUE,
    include_set_pieces = FALSE
    ) {

  # check if match input is not a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # get matchInfo from API
  matchInfo <-
    purrr::map_df(
      matches,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = "https://api.impect.com/v5/customerapi/matches/",
            id = .,
            token = token
            ),
          "text",
          encoding = "UTF-8"
          )
        )$data
      ) %>%
    dplyr::select(.data$id, .data$iterationId, .data$lastCalculationDate) %>%
    base::unique()

  # filter for fail matches
  fail_matches <- matchInfo %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == TRUE) %>%
    dplyr::pull(.data$id)

  # filter for avilable matches
  matches <- matchInfo %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == FALSE) %>%
    dplyr::pull(.data$id)

  # raise warnings
  if (base::length(fail_matches) > 0) {
    if (base::length(matches) == 0) {
      base::stop("All supplied matches are unavailable. Execution stopped.")
    }
    else {
      base::warning(
        sprintf(
          "The following matches are not available yet and were ignored:\n\t%s",
          paste(fail_matches, collapse = ", ")
        )
      )
    }
  }

  # get events from API
  events <-
    purrr::map_df(
      matches,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = "https://api.impect.com/v5/customerapi/matches/",
            id = .,
            suffix = "/events",
            token = token
          ),
          "text",
          encoding = "UTF-8"
          )
        )$data %>%
        dplyr::mutate(matchId = ..1) %>%
        jsonlite::flatten()
      )

  # fix column names using regex
  base::names(events) <-
    gsub("\\.(.)", "\\U\\1", base::names(events), perl = TRUE)

  if (include_kpis) {
    # get event kpis from API
    scorings <-
      purrr::map_df(
        matches,
        ~ jsonlite::fromJSON(
          httr::content(
            .callAPIlimited(
              base_url = "https://api.impect.com/v5/customerapi/matches/",
              id = .,
              suffix = "/event-kpis",
              token = token
            ),
            "text",
            encoding = "UTF-8"
          )
        )$data
      )

    # get kpi names from API
    kpis <- jsonlite::fromJSON(
      httr::content(
        .callAPIlimited(
          base_url = "https://api.impect.com/v5/customerapi/kpis/event",
          token = token
          ),
        "text",
        encoding = "UTF-8"
        )
      )$data %>%
      jsonlite::flatten() %>%
      dplyr::select(.data$id, .data$name)
  }

  if (include_set_pieces) {
    # get set piece data from API
    set_pieces <-
      purrr::map_df(
        matches,
        ~ jsonlite::fromJSON(
          httr::content(
            .callAPIlimited(
              base_url = "https://api.impect.com/v5/customerapi/matches/",
              id = .,
              suffix = "/set-pieces",
              token = token
            ),
            "text",
            encoding = "UTF-8"
          )
        )$data %>%
          dplyr::mutate(matchId = ..1) %>%
        jsonlite::flatten()
      ) %>%
      tidyr::unnest_longer(.data$setPieceSubPhase) %>%
      tidyr::unnest(.data$setPieceSubPhase, names_sep = ".") %>%
      dplyr::rename(setPiecePhaseIndex = "phaseIndex")

    # fix column names using regex
    base::names(set_pieces) <-
      gsub("\\.(.)", "\\U\\1", base::names(set_pieces), perl = TRUE)

    # merge events and set pieces
    events <- events %>%
      dplyr::left_join(
        dplyr::select(set_pieces, -.data$matchId, -.data$squadId),
        by = c(
          "setPieceId" = "id",
          "setPieceSubPhaseId" = "setPieceSubPhaseId"
        )
      )
  }

  # get unique iterationIds
  iterations <- matchInfo %>%
    dplyr::pull(.data$iterationId) %>%
    base::unique()


  # get player master data from API
  players <-
    purrr::map_df(
      iterations,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = "https://api.impect.com/v5/customerapi/iterations/",
            id = .,
            suffix = "/players",
            token = token
          ),
          "text",
          encoding = "UTF-8"
        )
      )$data
    ) %>%
    dplyr::select(.data$id, .data$commonname) %>%
    base::unique()

  # get squad master data from API
  squads <-
    purrr::map_df(
      iterations,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = "https://api.impect.com/v5/customerapi/iterations/",
            id = .,
            suffix = "/squads",
            token = token
          ),
          "text",
          encoding = "UTF-8"
        )
      )$data %>%
        jsonlite::flatten()
    ) %>%
    dplyr::select(.data$id, .data$name) %>%
    base::unique()

  # fix column names using regex
  base::names(squads) <-
    gsub("\\.(.)", "\\U\\1", base::names(squads), perl = TRUE)

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations, ~ getMatches(iteration = ., token = token))

  # get iterations
  iterations <- getIterations(token = token)

  # account for matches without dribbles, duels, or opponents tagged
  attributes <- c(
    "dribbleDistance",
    "dribbleType",
    "dribbleResult",
    "dribblePlayerId",
    "duelDuelType",
    "duelPlayerId",
    "opponentCoordinatesX",
    "opponentCoordinatesY",
    "opponentAdjCoordinatesX",
    "opponentAdjCoordinatesY"
  )

  # add attribute if it doesn't exist in df
  for (attribute in attributes) {
    if (!(attribute %in% colnames(events))) {
      events[[attribute]] <- NA
    }
  }

  # start merging dfs

  # merge events with squads
  events <- events %>%
    dplyr::left_join(
      dplyr::select(squads, squadId = .data$id, squadName = .data$name),
      by = base::c("squadId" = "squadId")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        squads,
        squadId = .data$id,
        currentAttackingSquadName = .data$name
      ),
      by = base::c("currentAttackingSquadId" = "squadId")
    )

  # merge events with players
  events <- events %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, playerName = .data$commonname),
      by = base::c("playerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, pressingPlayerName = .data$commonname),
      by = base::c("pressingPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, fouledPlayerName = .data$commonname),
      by = base::c("fouledPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, duelPlayerName = .data$commonname),
      by = base::c("duelPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, passReceiverPlayerName = .data$commonname),
      by = base::c("passReceiverPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, dribbleOpponentPlayerName = .data$commonname),
      by = base::c("dribblePlayerId" = "id")
    )

  if (include_set_pieces) {
    events <- events %>%
      dplyr::left_join(
        dplyr::select(
          players, .data$id, setPieceSubPhaseMainEventPlayerName = .data$commonname
          ),
        by = base::c("setPieceSubPhaseMainEventPlayerId" = "id")
      ) %>%
      dplyr::left_join(
        dplyr::select(
          players, .data$id, setPieceSubPhasePassReceiverName = .data$commonname
        ),
        by = base::c("setPieceSubPhasePassReceiverId" = "id")
      ) %>%
      dplyr::left_join(
        dplyr::select(
          players, .data$id, setPieceSubPhaseFirstTouchPlayerName = .data$commonname
        ),
        by = base::c("setPieceSubPhaseFirstTouchPlayerId" = "id")
      ) %>%
      dplyr::left_join(
        dplyr::select(
          players, .data$id, setPieceSubPhaseSecondTouchPlayerName = .data$commonname
        ),
        by = base::c("setPieceSubPhaseSecondTouchPlayerId" = "id")
      )
  }

  # merge with matchplan info
  events <- events %>%
    dplyr::left_join(matchplan, by = base::c("matchId" = "id"))

  # merge with competition info
  events <- events %>%
    dplyr::left_join(iterations,
                     by = base::c("iterationId" = "id"))

  if (include_kpis) {
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
  }

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
      eventNumber = "index",
      dribbleOpponentPlayerId = "dribblePlayerId",
      setPieceSubPhaseMainEvent = "setPieceMainEvent"
    )

  # reorder columns
  # define desired column order
  event_cols <- base::c(
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
    "sequenceIndex",
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
    "playerPositionSide",
    "actionType",
    "action",
    "bodyPart",
    "bodyPartExtended",
    "previousPassHeight",
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
    "opponentCoordinatesX",
    "opponentCoordinatesY",
    "opponentAdjCoordinatesX",
    "opponentAdjCoordinatesY",
    "passReceiverType",
    "passReceiverPlayerId",
    "passReceiverPlayerName",
    "passDistance",
    "passAngle",
    "dribbleDistance",
    "dribbleType",
    "dribbleResult",
    "dribbleOpponentPlayerId",
    "dribbleOpponentPlayerName",
    "shotDistance",
    "shotAngle",
    "shotTargetPointY",
    "shotTargetPointZ",
    "shotWoodwork",
    "shotGkCoordinatesX",
    "shotGkCoordinatesY",
    "shotGkAdjCoordinatesX",
    "shotGkAdjCoordinatesY",
    "shotGkDivePointY",
    "shotGkDivePointZ",
    "duelType",
    "duelPlayerId",
    "duelPlayerName",
    "fouledPlayerId",
    "fouledPlayerName",
    "formationTeam",
    "formationOpponent",
    "inferredSetPiece"
  )

  set_piece_cols = base::c(
    "setPieceId",
    "setPiecePhaseIndex",
    "setPieceCategory",
    "adjSetPieceCategory",
    "setPieceExecutionType",
    "setPieceSubPhaseId",
    "setPieceSubPhaseIndex",
    "setPieceSubPhaseStartZone",
    "setPieceSubPhaseCornerEndZone",
    "setPieceSubPhaseCornerType",
    "setPieceSubPhaseFreeKickEndZone",
    "setPieceSubPhaseFreeKickType",
    "setPieceSubPhaseMainEvent",
    "setPieceSubPhaseMainEventPlayerId",
    "setPieceSubPhaseMainEventPlayerName",
    "setPieceSubPhaseMainEventOutcome",
    "setPieceSubPhasePassReceiverId",
    "setPieceSubPhasePassReceiverName",
    "setPieceSubPhaseFirstTouchPlayerId",
    "setPieceSubPhaseFirstTouchPlayerName",
    "setPieceSubPhaseFirstTouchWon",
    "setPieceSubPhaseIndirectHeader",
    "setPieceSubPhaseSecondTouchPlayerId",
    "setPieceSubPhaseSecondTouchPlayerName",
    "setPieceSubPhaseSecondTouchWon"
  )

  # add columns that might not exist in previous data versions
  for (col in event_cols) {
    if (!(col %in% colnames(events))) {
      events[[col]] <- NA
    }
  }

  # create order
  order <- event_cols

  # add set piece cols if necessary
  if (include_set_pieces) {
    order <- base::c(order, set_piece_cols)
  }

  # add kpis if necessary
  if (include_kpis) {
    order <- base::c(order, kpis$name)
  }

  # reorder data
  events <- events %>%
    dplyr::select(dplyr::all_of(order))

  # reorder rows
  events <- events %>%
    dplyr::arrange("matchId", "eventNumber")

  return(events)
}
