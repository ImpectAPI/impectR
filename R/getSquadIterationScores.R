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
  squads <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/squads",
        token = token
        ),
      "text",
      encoding = "UTF-8"
      )
    )$data %>%
    jsonlite::flatten() %>%
    dplyr::select(id, name, idMappings) %>%
    base::unique()

  # get squad Ids
  squadIds <- squads %>%
    dplyr::pull(id)

  # clean data
  squads <- .cleanData(squads)

  # apply .playerIterationScores function to all squads
  scores_raw <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/squad-scores",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    dplyr::mutate(iterationId = iteration)

  # get score names
  score_list <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/squad-scores",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::select(id, name)

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
      dplyr::select(
        squads, id, wyscoutId, heimSpielId, skillCornerId, squadName = name
      ),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, id, competitionId, competitionName, competitionType, season
      ),
      by = c("iterationId" = "id")
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionName",
    "season",
    "squadId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
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
