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
#'   playerIterationAverages <- getPlayerIterationAverages(518, token)
#' })
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
    dplyr::filter(access == TRUE) %>%
    dplyr::pull(id) %>%
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
    dplyr::select(id, name)

  # get iterations from API
  iterations <- getIterations(token = token)

  # manipulate averages

  # unnest scorings
  averages <- averages_raw %>%
    tidyr::unnest("kpis", keep_empty = TRUE) %>%
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
    dplyr::filter(base::is.na(playerId) == FALSE) %>%
    # remove the "NA" column if it exists
    dplyr::select(-dplyr::matches("^NA$"))

  # merge with other data
  averages <- averages %>%
    dplyr::left_join(dplyr::select(squads, id, squadName = name),
                     by = c("squadId" = "id")) %>%
    dplyr::left_join(
      dplyr::select(
        players, id, wyscoutId, heimSpielId, skillCornerId,
        playerName = commonname, firstname, lastname, birthdate, birthplace, leg
      ),
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
