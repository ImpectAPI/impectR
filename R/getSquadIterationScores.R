#' Return a dataframe that contains all squads scores for a given iteration ID
#'
#' @param iteration 'IMPCET' iteration ID
#' @param token bearer token

#' @export

#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the squad scores aggregated per squad for the
#' given iteration ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(squad_scores <- getSquadIterationScores(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' squad_scores <- getSquadIterationScores(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#' }
getSquadIterationScores <- function (iteration, token) {

  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # get squads master data from API
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
    dplyr::select(.data$id, .data$name, .data$idMappings) %>%
    base::unique()

  # get squad Ids
  squadIds <- squads %>%
    dplyr::pull(.data$id)

  # clean data
  squads <- .cleanData(squads)

  # squad iteration scores from API
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

  # get score names from API
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
    dplyr::select(.data$id, .data$name)

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate averages

  # unnest scores
  scores <- scores_raw %>%
    tidyr::unnest(.data$squadScores) %>%
    dplyr::select(
      .data$iterationId,
      .data$squadId,
      .data$matches,
      .data$squadScoreId,
      .data$value) %>%
    # join with kpis to ensure all kpiIds are present and order by kpiId
    dplyr::full_join(score_list, by = c("squadScoreId" = "id")) %>%
    dplyr::arrange(.data$squadScoreId, .data$squadId) %>%
    # drop kpiId column
    dplyr::select(-.data$squadScoreId) %>%
    # pivot data
    tidyr::pivot_wider(
      names_from = .data$name,
      values_from = .data$value,
      values_fill = 0,
      values_fn = base::sum
    ) %>%
    # filter for non NA columns that were created by full join
    dplyr::filter(base::is.na(.data$squadId) == FALSE)

  # merge with other data
  scores <- scores %>%
    dplyr::left_join(
      dplyr::select(
        squads, .data$id, .data$wyscoutId, .data$heimSpielId,
        .data$skillCornerId, squadName = .data$name
      ),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, .data$id, .data$competitionId, .data$competitionName,
        .data$competitionType, .data$season
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
