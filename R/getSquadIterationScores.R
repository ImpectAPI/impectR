#' Return a dataframe that contains all squads scores for a given iteration ID
#'
#' @param iteration Impect iteration ID
#' @param token bearer token

#' @export

#' @importFrom dplyr %>%
#' @return a dataframe containing the squad scores aggregated per squad for the
#' given iteration ID
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   scores <- getSquadIterationScores(518, token)
#' })
#' }
getSquadIterationScores <- function (iteration, token) {
  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # get squads for given iterationId
  squads <- .squadNames(iteration = iteration, token = token)
  squadIds <- squads %>%
    dplyr::pull(id)

  # apply .playerIterationScores function to all squads
  scores_raw <- .squadIterationScores(iteration = iteration, token = token)

  # get score names
  score_list <- .squadScores(token = token)

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate averages

  # unnest scores
  scores <- scores_raw %>%
    tidyr::unnest(squadScores) %>%
    dplyr::select(
      iterationId,
      squadId,
      matches,
      squadScoreId,
      value) %>%
    # join with kpis to ensure all kpiIds are present and order by kpiId
    dplyr::full_join(score_list, by = c("squadScoreId" = "id")) %>%
    dplyr::arrange(squadScoreId, squadId) %>%
    # drop kpiId column
    dplyr::select(-squadScoreId) %>%
    # pivot data
    tidyr::pivot_wider(
      names_from = name,
      values_from = value,
      values_fill = 0,
      values_fn = base::sum
    ) %>%
    # filter for non NA columns that were created by full join
    dplyr::filter(base::is.na(squadId) == FALSE)

  # merge with other data
  scores <- scores %>%
    dplyr::left_join(
      dplyr::select(squads, id, squadName = name),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(iterations, id, competitionName, season),
      by = c("iterationId" = "id")
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionName",
    "season",
    "squadId",
    "squadName",
    "matches",
    score_list$name
  )

  # select columns
  scores <- scores %>%
    dplyr::select(dplyr::all_of(order))

  # return averages
  return(scores)
}
