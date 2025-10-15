#' Return a dataframe that contains all squads averages for a given iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
#' @param host host environment

#' @export

#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the KPI averages aggregated per squad for the
#' given iteration ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(squad_avgs <- getSquadIterationAverages(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' squad_avgs <- getSquadIterationAverages(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#' }
getSquadIterationAverages <- function (
    iteration,
    token,
    host = "https://api.impect.com"
) {

  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # get squads master data from API
  squads <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/squads",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten()

  # get squadIds
  squadIds <- squads %>%
    dplyr::pull(.data$id)

  # clean data
  squads <- .cleanData(squads)

  # get player iteration averages for all squads from API
  averages_raw <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/squad-kpis",
        token = token
        ),
      "text",
      encoding = "UTF-8"
      )
    )$data %>%
    dplyr::mutate(iterationId = iteration)

  # get kpi names from API
  kpis <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/kpis",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::select(.data$id, .data$name)

  # get competitions from API
  iterations <- getIterations(token = token, host = host)

  # manipulate averages

  # unnest scorings
  averages <- averages_raw %>%
    tidyr::unnest(.data$kpis) %>%
    dplyr::select(
      .data$iterationId,
      .data$squadId,
      .data$matches,
      .data$kpiId,
      .data$value) %>%
    # join with kpis to ensure all kpiIds are present and order by kpiId
    dplyr::full_join(kpis, by = c("kpiId" = "id")) %>%
    dplyr::arrange(.data$kpiId, .data$squadId) %>%
    # drop kpiId column
    dplyr::select(-.data$kpiId) %>%
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
  averages <- averages %>%
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
    kpis$name
  )

  # select columns
  averages <- averages %>%
    dplyr::select(dplyr::all_of(order))

  # return averages
  return(averages)
}
