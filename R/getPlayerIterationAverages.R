#' Return a dataframe that contains all player averages for a given iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the KPI averages aggregated per player and
#' position for the given iteration ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(player_avgs <- getPlayerIterationAverages(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' player_avgs <- getPlayerIterationAverages(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#' }
getPlayerIterationAverages <- function (iteration, token) {

  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # get squad master data from API
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
      jsonlite::flatten()

  # get squadIds
  squadIds <- squads %>%
    dplyr::filter(.data$access == TRUE) %>%
    dplyr::pull(.data$id) %>%
    base::unique()

  # get player iteration averages for all squads from API
  averages_raw <-
    purrr::map_df(
      squadIds,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = paste0(
              "https://api.impect.com/v5/customerapi/iterations/",
              iteration,
              "/squads/",
              .,
              "/player-kpis"
              ),
            token = token
            ),
          "text",
          encoding = "UTF-8"
          )
        )$data %>%
        dplyr::mutate(squadId = ..1, iterationId = iteration)
      )

  # get player master data from API
  players <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/players",
        token = token
        ),
      "text",
      encoding = "UTF-8"
      )
    )$data %>%
    base::unique()

  # clean data
  players <- .cleanData(players)

  # get kpi names from API
  kpis <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/kpis",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::select(.data$id, .data$name)

  # get iterations from API
  iterations <- getIterations(token = token)

  # manipulate averages

  # unnest scorings
  averages <- averages_raw %>%
    tidyr::unnest("kpis", keep_empty = TRUE) %>%
    dplyr::select(
      .data$iterationId,
      .data$squadId,
      .data$playerId,
      .data$position,
      .data$playDuration,
      .data$matchShare,
      .data$kpiId,
      .data$value
    ) %>%
    # join with kpis to ensure all kpiIds are present and order by kpiId
    dplyr::full_join(kpis, by = c("kpiId" = "id")) %>%
    dplyr::arrange(.data$kpiId, .data$playerId) %>%
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
    dplyr::filter(base::is.na(.data$playerId) == FALSE) %>%
    # remove the "NA" column if it exists
    dplyr::select(-dplyr::matches("^NA$"))

  # merge with other data
  averages <- averages %>%
    dplyr::left_join(dplyr::select(squads, .data$id, squadName = .data$name),
                     by = c("squadId" = "id")) %>%
    dplyr::left_join(
      dplyr::select(
        players,
        .data$id,
        .data$wyscoutId,
        .data$heimSpielId,
        .data$skillCornerId,
        playerName = .data$commonname,
        .data$firstname,
        .data$lastname,
        .data$birthdate,
        .data$birthplace,
        .data$leg
      ),
      by = c("playerId" = "id")) %>%
    dplyr::left_join(dplyr::select(
      iterations, .data$id, .data$competitionName, .data$season),
      by = c("iterationId" = "id")
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionName",
    "season",
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
    "position",
    "matchShare",
    "playDuration",
    kpis$name
  )

  # select columns
  averages <- averages %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(averages)
}
