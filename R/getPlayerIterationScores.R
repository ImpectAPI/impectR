#' Return a dataframe that contains all player scores for a given iteration ID
#'
#' @param iteration Impect iteration ID
#' @param positions list of position names
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the player scores aggregated per player for
#' the given iteration ID and list of positions
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   scores <- getPlayerIterationScores(518, token)
#' })
#' }
getPlayerIterationScores <- function (iteration, positions, token) {
  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # compile position string
  position_string <- paste(positions, collapse = ",")

  # get squads for given iterationId
  squads <- .squadNames(iteration = iteration, token = token)
  squadIds <- squads %>%
    dplyr::filter(access == TRUE) %>%
    dplyr::pull(id) %>%
    base::unique()

  # apply .playerIterationScores function to all squads
  scores_raw <-
    purrr::map_df(squadIds,
                  ~ .playerIterationScores(
                    iteration = iteration,
                    squad = .,
                    positions = position_string,
                    token = token
                  ))

  # apply .playerNames function to a set of iterations
  players <- .playerNames(iteration = iteration, token = token)

  # get kpi names
  score_list <- .playerScores(token = token)

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate averages

  # unnest scorings
  scores <- scores_raw %>%
    tidyr::unnest("playerScores", keep_empty = TRUE) %>%
    dplyr::select(
      iterationId,
      squadId,
      playerId,
      playDuration,
      matchShare,
      playerScoreId,
      value
    ) %>%
    # add column to store positions string
    dplyr::mutate(positions = position_string) %>%
    # join with kpis to ensure all scores are present and order by playerScoreId
    dplyr::full_join(score_list, by = c("playerScoreId" = "id")) %>%
    dplyr::arrange(playerScoreId, playerId) %>%
    # drop playerScoreId column
    dplyr::select(-playerScoreId) %>%
    # pivot data
    tidyr::pivot_wider(
      names_from = name,
      values_from = value,
      values_fill = 0,
      values_fn = base::sum
    ) %>%
    # filter for non NA columns that were created by full join
    dplyr::filter(base::is.na(playerId) == FALSE) %>%
    # remove the "NA" column if it exists
    dplyr::select(-dplyr::matches("^NA$"))

  # merge with other data
  scores <- scores %>%
    dplyr::left_join(dplyr::select(squads, id, squadName = name),
                     by = c("squadId" = "id")) %>%
    dplyr::left_join(dplyr::select(players, id, playerName = commonname, firstname, lastname, birthdate, birthplace, leg),
                     by = c("playerId" = "id")) %>%
    dplyr::left_join(dplyr::select(iterations, id, competitionName, season),
                     by = c("iterationId" = "id"))

  # define column order
  order <- c(
    "iterationId",
    "competitionName",
    "season",
    "squadId",
    "squadName",
    "playerId",
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

  # return scores
  return(scores)
}
