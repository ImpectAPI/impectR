# Define the allowed positions
allowed_positions <- c(
  "GOALKEEPER",
  "LEFT_WINGBACK_DEFENDER",
  "RIGHT_WINGBACK_DEFENDER",
  "CENTRAL_DEFENDER",
  "DEFENSE_MIDFIELD",
  "CENTRAL_MIDFIELD",
  "ATTACKING_MIDFIELD",
  "LEFT_WINGER",
  "RIGHT_WINGER",
  "CENTER_FORWARD"
)


#' Return a dataframe that contains all player scores for a given match ID
#' and list of positions
#'
#' @param matches Impect match IDs
#' @param positions list of position names. Must be one of:   "GOALKEEPER",
#' "LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER", "CENTRAL_DEFENDER",
#' "DEFENSE_MIDFIELD", "CENTRAL_MIDFIELD", "ATTACKING_MIDFIELD", "LEFT_WINGER",
#' "RIGHT_WINGER", "CENTER_FORWARD"
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the scores aggregated per player and
#' position for the given match ID and list of positions
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   playerMatchScores <- getPlayerMatchsums(84248, token)
#' })
#' }
getPlayerMatchScores <- function (matches, positions, token) {
  # check if match input is a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # check if the input positions are valid
  invalid_positions <- positions[!positions %in% allowed_positions]
  if (length(invalid_positions) > 0) {
    stop("Invalid position(s): ", paste(invalid_positions, collapse = ", "),
         ".\nChoose one or more of: ", paste(allowed_positions, collapse = ", "))
  }

  # apply .matchInfo function to a set of matches
  matchInfo <-
    purrr::map_df(matches, ~ .matchInfo(match = ., token = token)) %>%
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

  # compile position string
  position_string <- paste(positions, collapse = ",")

  # apply .playerMatchScores function to a set of matches
  scores_raw <-
    purrr::map(matches, ~ .playerMatchScores(
      match = .,
      positions = position_string,
      token = token
      ))

  # get unique iterationIds
  iterations <- matchInfo %>%
    dplyr::pull(iterationId) %>%
    base::unique()

  # apply playerNames function to a set of iterations
  players <-
    purrr::map_df(iterations, ~ .playerNames(iteration = ., token = token)) %>%
    dplyr::select(
      id, playerName = commonname, firstname, lastname,
      birthdate, birthplace, leg, idMappings
    ) %>%
    base::unique()

  # clean data
  players <- .cleanData(players)

  # apply squadNames function to a set of iterations
  squads <-
    purrr::map_df(iterations, ~ .squadNames(iteration = ., token = token)) %>%
    dplyr::select(id, name) %>%
    base::unique()

  # get kpi names
  score_list <- .playerScores(token = token)

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations, ~ getMatches(iteration = ., token = token))

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate scores

  extract_scores <- function(dict, side) {
    # convert side data to df
    temp <-
      jsonlite::flatten(jsonlite::fromJSON(
        jsonlite::toJSON(dict[side][[1]]$players),
        simplifyDataFrame = TRUE
      ),
      recursive = FALSE)

    # unnest scores
    temp <- temp %>%
      tidyr::unnest("playerScores", keep_empty = TRUE) %>%
      dplyr::select(
        "playerId" = "id",
        "playerScoreId",
        "value",
        "matchShare",
        "playDuration") %>%
      # join with kpis to ensure all kpiIds are present and order by kpiId
      dplyr::full_join(score_list, by = c("playerScoreId" = "id")) %>%
      dplyr::arrange("playerScoreId", "playerId") %>%
      # drop kpiId column
      dplyr::select(-"playerScoreId") %>%
      # pivot data
      tidyr::pivot_wider(
        names_from = "name",
        values_from = "value",
        values_fill = 0,
        values_fn = base::sum
      ) %>%
      # filter for non NA columns that were created by full join
      dplyr::filter(base::is.na(playerId) == FALSE) %>%
      # remove the "NA" column if it exists
      dplyr::select(-dplyr::matches("^NA$")) %>%
      dplyr::mutate(
        # add matchId
        matchId = dict$matchId,
        # add squadId
        squadId = dict[[side]]$id,
        # add positions
        positions = position_string)

    return(temp)
  }

  #create emtpty df to store match scores
  scores <- data.frame()

  # iterate over list elements
  for (i in 1:length(scores_raw)) {
    temp <- scores_raw[[i]]
    # apply extract_matchsums function to home and away squad
    temp <-
      purrr::map_df(
        base::c("squadHome", "squadAway"),
        ~ extract_scores(temp, .)
      )

    # appénd to target df
    scores <- base::rbind(scores, temp)
  }

  # merge with other data
  scores <- scores %>%
    dplyr::left_join(
      dplyr::select(
        matchplan, id, scheduledDate, matchDayIndex, matchDayName, iterationId
      ),
      by = c("matchId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, id, competitionId, competitionName, competitionType, season
      ),
      by = c("iterationId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(squads, id, squadName = name),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players, id, wyscoutId, heimSpielId, skillCornerId,
        playerName, firstname, lastname, birthdate, birthplace, leg
      ),
      by = c("playerId" = "id")) %>%
    # fix some column names
    dplyr::rename(
      dateTime = scheduledDate
    )

  # define column order
  order <- c(
    "matchId",
    "dateTime",
    "competitionName",
    "competitionId",
    "competitionType",
    "iterationId",
    "season",
    "matchDayIndex",
    "matchDayName",
    "squadId",
    "squadName",
    "playerId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "playerName",
    "firstname",
    "lastname",
    "birthdate",
    "birthplace",
    "leg",
    "positions",
    "matchShare",
    "playDuration",
    score_list$name
  )

  # select columns
  scores <- scores %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(scores)
}
