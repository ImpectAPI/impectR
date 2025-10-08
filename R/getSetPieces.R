#' Return a dataframe that contains all set pieces and aggregated kpi values per
#' set piece sub phase for a set of given list of match IDs
#'
#' @param matches list fo 'IMPECT' match IDs
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing all set pieces and aggregated kpi values per
#' set piece sub phase for a set of given list of match IDs
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(set_pieces <- getSetPieces(
#'   matches = c(0, 1),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' set_pieces <- getSetPieces(
#'   matches = c(84248, 158150),
#'   token = "yourToken"
#' )
#' }
getSetPieces <- function (
    matches,
    token,
    host = "https://api.impect.com"
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
            host,
            base_url = "/v5/customerapi/matches/",
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

  # get set piece data from API
  set_pieces <-
    purrr::map_df(
      matches,
      ~ jsonlite::flatten(
        jsonlite::fromJSON(
          httr::content(
            .callAPIlimited(
              host,
              base_url = "/v5/customerapi/matches/",
              id = .,
              suffix = "/set-pieces",
              token = token
              ),
            "text",
            encoding = "UTF-8"
            )
          )$data %>%
          dplyr::mutate(matchId = ..1)
      )
    ) %>%
    tidyr::unnest_longer(.data$setPieceSubPhase) %>%
    tidyr::unnest(.data$setPieceSubPhase, names_sep = ".") %>%
    tidyr::unnest(.data$setPieceSubPhase.aggregates, names_sep = ".")

  # fix column names using regex
  base::names(set_pieces) <-
    gsub("\\.(.)", "\\U\\1", base::names(set_pieces), perl = TRUE)

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
            host,
            base_url = "/v5/customerapi/iterations/",
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
            host,
            base_url = "/v5/customerapi/iterations/",
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

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations,
                  ~ getMatches(iteration = ., token = token, host = host))

  # get iterations
  iterations <- getIterations(token = token, host = host)

  # start merging dfs

  # merge with matchplan info
  set_pieces <- set_pieces %>%
    dplyr::left_join(matchplan, by = base::c("matchId" = "id"))

  # merge with competition info
  set_pieces <- set_pieces %>%
    dplyr::left_join(iterations,
                     by = base::c("iterationId" = "id"))

  # determine defending squad
  set_pieces <- set_pieces %>%
    dplyr::mutate(
      defendingSquadId = ifelse(
        .data$squadId == .data$awaySquadId, .data$homeSquadId, .data$awaySquadId
        )
    )

  # merge set_pieces with squads
  set_pieces <- set_pieces %>%
    dplyr::left_join(
      dplyr::select(
        squads,
        attackingSquadId = .data$id,
        attackingSquadName = .data$name
      ),
      by = base::c("squadId" = "attackingSquadId")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        squads,
        defendingSquadId = .data$id,
        defendingSquadName = .data$name
      ),
      by = base::c("defendingSquadId" = "defendingSquadId")
    )

  # merge set_pieces with players
  set_pieces <- set_pieces %>%
    dplyr::left_join(
      dplyr::select(
        players,
        .data$id,
        setPieceSubPhaseMainEventPlayerName = .data$commonname
      ),
      by = base::c("setPieceSubPhaseMainEventPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players,
        .data$id,
        setPieceSubPhasePassReceiverName = .data$commonname
      ),
      by = base::c("setPieceSubPhasePassReceiverId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players,
        .data$id,
        setPieceSubPhaseFirstTouchPlayerName = .data$commonname
      ),
      by = base::c("setPieceSubPhaseFirstTouchPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players,
        .data$id,
        setPieceSubPhaseSecondTouchPlayerName = .data$commonname
      ),
      by = base::c("setPieceSubPhaseSecondTouchPlayerId" = "id")
    )

  # rename some columns
  set_pieces <- set_pieces %>%
    dplyr::rename(
      setPieceId = "id",
      dateTime = "scheduledDate",
      attackingSquadId = "squadId",
      setPiecePhaseIndex = "phaseIndex",
      setPieceSubPhase_SHOT_XG = "setPieceSubPhaseAggregatesSHOT_XG",
      setPieceSubPhase_PACKING_XG = "setPieceSubPhaseAggregatesPACKING_XG",
      setPieceSubPhase_POSTSHOT_XG = "setPieceSubPhaseAggregatesPOSTSHOT_XG",
      setPieceSubPhase_SHOT_AT_GOAL_NUMBER = "setPieceSubPhaseAggregatesSHOT_AT_GOAL_NUMBER",
      setPieceSubPhase_GOALS = "setPieceSubPhaseAggregatesGOALS",
      setPieceSubPhase_PXT_POSITIVE = "setPieceSubPhaseAggregatesPXT_POSITIVE",
      setPieceSubPhase_BYPASSED_OPPONENTS = "setPieceSubPhaseAggregatesBYPASSED_OPPONENTS",
      setPieceSubPhase_BYPASSED_DEFENDERS = "setPieceSubPhaseAggregatesBYPASSED_DEFENDERS"
    )

  # define desired column order
  order <- c(
    "matchId",
    "dateTime",
    "competitionName",
    "competitionId",
    "competitionType",
    "iterationId",
    "season",
    "attackingSquadId",
    "attackingSquadName",
    "defendingSquadId",
    "defendingSquadName",
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
    "setPieceSubPhaseSecondTouchWon",
    "setPieceSubPhase_SHOT_XG",
    "setPieceSubPhase_PACKING_XG",
    "setPieceSubPhase_POSTSHOT_XG",
    "setPieceSubPhase_SHOT_AT_GOAL_NUMBER",
    "setPieceSubPhase_GOALS",
    "setPieceSubPhase_PXT_POSITIVE",
    "setPieceSubPhase_BYPASSED_OPPONENTS",
    "setPieceSubPhase_BYPASSED_DEFENDERS"
  )

  # reorder data
  set_pieces <- set_pieces %>%
    dplyr::select(dplyr::all_of(order))

  # reorder rows
  set_pieces <- set_pieces %>%
    dplyr::arrange("matchId", "phaseIndex")

  return(set_pieces)
}
