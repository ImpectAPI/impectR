#' Return a dataframe that contains all player matchsums for a given match ID
#'
#' @param match Impect match ID
#' @param token bearer token
#'
#' @return a dataframe containing the matchsums aggregated per player and position for the given match ID
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   matchsums <- getMatchsums(84248, token)
#' })
#' }
getMatchsums <- function (match, token) {
  # get match info
  response <-
    httr::RETRY(
      "GET",
      base::paste0("https://api.impect.com/v4/customerapi/matches/", match),
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  data <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # define function to extract players
  extract_players <- function(dict, side) {
    # convert side data to df
    temp <-
      jsonlite::flatten(jsonlite::fromJSON(jsonlite::toJSON(data[side][[1]]$players),
                                           simplifyDataFrame = TRUE),
                        recursive = FALSE)

    # detect non-empty playTime column
    list <- base::sapply(temp$playTime, length) > 0

    # filter and add context data
    temp <- temp[list,] %>%
      dplyr::mutate(
        matchId = data$matchId,
        date = data$date,
        dateTime = data$dateTime,
        competition = data$competition$competition,
        competitionId = data$competition$competitionId,
        competitionType = data$competition$competitionType,
        competitionIterationId = data$competition$competitionIterationId,
        competitionIterationName = data$competition$competitionIterationName,
        competitionIterationStepId = data$competition$competitionIterationStepId,
        competitionIterationStepName = data$competition$competitionIterationStepName,
        squadId = data[side][[1]]$squadId,
        squadName = data[side][[1]]$name
      )

    # reorder columns
    temp <-
      dplyr::select(
        temp,
        "matchId",
        "date",
        "dateTime",
        "competition",
        "competitionId",
        "competitionType",
        "competitionIterationId",
        "competitionIterationName",
        "competitionIterationStepId",
        "competitionIterationStepName",
        "squadId",
        "squadName",
        "playerId",
        "commonname",
        "playTime"
      )

    return(temp)
  }

  # apply extract_players function to home and away squad
  players <-
    purrr::map_df(purrr::map(
      base::c("squadHome", "squadAway"),
      ~ extract_players(data, .)
    ),
    base::as.data.frame)

  # unnest playTime column
  players <- players %>%
    tidyr::unnest(.data$playTime)

  # get match sums
  response <-
    httr::RETRY(
      "GET",
      base::paste0(
        "https://api.impect.com/v4/customerapi/matches/",
        match,
        "/matchsums"
      ),
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  match_sums <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # get match sums
  response <-
    httr::RETRY(
      "GET",
      base::paste0("https://api.impect.com/v4/customerapi/kpis"),
      httr::add_headers(Authorization = base::paste("Bearer", token, sep = " "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  kpis <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # convert to df
  kpis <- base::data.frame(kpis)

  # define function to extract matchsums
  extract_matchsums <- function(dict, side) {
    # convert side data to df
    temp <-
      jsonlite::flatten(jsonlite::fromJSON(
        jsonlite::toJSON(match_sums[side][[1]]$players),
        simplifyDataFrame = TRUE
      ),
      recursive = FALSE)

    # detect non-empty scorings column
    list <- base::sapply(temp$scorings, length) > 0

    # filter and add context data
    temp <- temp[list,]

    # unnest scorings
    temp <- temp %>%
      tidyr::unnest(.data$scorings) %>%
      dplyr::select(.data$playerId, .data$detailedPosition, .data$kpiId, .data$totalValue) %>%
      # join with kpis to ensure all kpiIds are present and order by kpiId
      dplyr::full_join(kpis, by = "kpiId") %>%
      dplyr::arrange(.data$kpiId, .data$playerId) %>%
      # drop kpiId column
      dplyr::select(-.data$kpiId) %>%
      # pivot data
      tidyr::pivot_wider(
        names_from = .data$kpiName,
        values_from = .data$totalValue,
        values_fill = 0,
        values_fn = base::sum
      ) %>%
      # filter for non NA columns that were created by full join
      dplyr::filter(base::is.na(.data$playerId) == FALSE)

    return(temp)
  }

  # apply extract_matchsums function to home and away squad
  match_sums <-
    purrr::map_df(purrr::map(
      base::c("squadHome", "squadAway"),
      ~ extract_matchsums(data, .)
    ),
    base::as.data.frame)


  # merge matchsums and players data
  data <-
    dplyr::inner_join(players, match_sums, by = base::c("playerId", "detailedPosition"))

  return(data)
}
