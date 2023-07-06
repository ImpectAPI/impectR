#' Return a dataframe that contains all player averages for a given iteration ID
#'
#' @param iteration Impect iteration ID
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the KPI averages aggregated per player and
#' position for the given iteration ID
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   matchsums <- getPlayerIterationAverages(518, token)
#' })
#' }
getPlayerIterationAverages <- function (iteration, token) {
  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # get squads for given iterationId
  squads <- .squadNames(iteration = iteration, token = token)
  squadIds <- squads %>%
    dplyr::pull(id) %>%
    base::unique()

  # apply .playerIterationAverages function to all squads
  averages_raw <-
    purrr::map_df(squadIds,
                  ~ .playerIterationAverages(
                    iteration = iteration,
                    squad = .,
                    token = token
                  ))

  # apply .playerNames function to a set of iterations
  players <- .playerNames(iteration = iteration, token = token)

  # get kpi names
  kpis <- .kpis(token = token)

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate averages

  # unnest scorings
  averages <- averages_raw %>%
    tidyr::unnest("kpis") %>%
    dplyr::select(
      iterationId,
      squadId,
      playerId,
      position,
      playDuration,
      matchShare,
      kpiId,
      value
    ) %>%
    # join with kpis to ensure all kpiIds are present and order by kpiId
    dplyr::full_join(kpis, by = c("kpiId" = "id")) %>%
    dplyr::arrange(kpiId, playerId) %>%
    # drop kpiId column
    dplyr::select(-kpiId) %>%
    # pivot data
    tidyr::pivot_wider(
      names_from = name,
      values_from = value,
      values_fill = 0,
      values_fn = base::sum
    ) %>%
    # filter for non NA columns that were created by full join
    dplyr::filter(base::is.na(playerId) == FALSE)

  # merge with other data
  averages <- averages %>%
    dplyr::left_join(dplyr::select(squads, id, squadName = name),
                     by = c("squadId" = "id")) %>%
    dplyr::left_join(dplyr::select(players, id, playerName = commonname),
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
    "position",
    "playDuration",
    "matchShare",
    kpis$name
  )

  # select columns
  averages <- averages %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(averages)
}
