#' Return a dataframe that contains all set pieces and aggregated kpi values per
#' set piece sub phase for a set of given list of match IDs
#'
#' @param matches list fo IMPECT match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all set pieces and aggregated kpi values per
#' set piece sub phase for a set of given list of match IDs
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   events <- getSetPieces(matches = c(84248), token = token)
#' })
#' }
getSetPieces <- function (
    matches,
    token
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
    dplyr::select(id, iterationId, lastCalculationDate) %>%
    base::unique()

  # filter for fail matches
  fail_matches <- matchInfo %>%
    dplyr::filter(base::is.na(lastCalculationDate) == TRUE) %>%
    dplyr::pull(id)

  # filter for avilable matches
  matches <- matchInfo %>%
    dplyr::filter(base::is.na(lastCalculationDate) == FALSE) %>%
    dplyr::pull(id)

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
              base_url = "https://api.impect.com/v5/customerapi/matches/",
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
    tidyr::unnest_longer(setPieceSubPhase) %>%
    tidyr::unnest(setPieceSubPhase, names_sep = ".") %>%
    tidyr::unnest(setPieceSubPhase.aggregates, names_sep = ".")

  # fix column names using regex
  base::names(set_pieces) <-
    gsub("\\.(.)", "\\U\\1", base::names(set_pieces), perl = TRUE)

  # get unique iterationIds
  iterations <- matchInfo %>%
    dplyr::pull(iterationId) %>%
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
    dplyr::select(id, commonname) %>%
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
    dplyr::select(id, name) %>%
    base::unique()

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations,
                  ~ getMatches(iteration = ., token = token))

  # get iterations
  iterations <- getIterations(token = token)

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
    mutate(
      defendingSquadId = ifelse(
        squadId == awaySquadId, homeSquadId, awaySquadId
        )
    )

  # merge set_pieces with squads
  set_pieces <- set_pieces %>%
    dplyr::left_join(
      dplyr::select(
        squads,
        attackingSquadId = id,
        attackingSquadName = name
      ),
      by = base::c("squadId" = "attackingSquadId")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        squads,
        defendingSquadId = id,
        defendingSquadName = name
      ),
      by = base::c("defendingSquadId" = "defendingSquadId")
    )

  # merge set_pieces with players
  set_pieces <- set_pieces %>%
    dplyr::left_join(
      dplyr::select(
        players, id, setPieceSubPhaseMainEventPlayerName = commonname
      ),
      by = base::c("setPieceSubPhaseMainEventPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players, id, setPieceSubPhasePassReceiverName = commonname
      ),
      by = base::c("setPieceSubPhasePassReceiverId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players, id, setPieceSubPhaseFirstTouchPlayerName = commonname
      ),
      by = base::c("setPieceSubPhaseFirstTouchPlayerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players, id, setPieceSubPhaseSecondTouchPlayerName = commonname
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
