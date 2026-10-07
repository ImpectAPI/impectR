#' Return a dataframe that contains match predictions for a given iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the match predictions for the given
#' iteration ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(match_predictions <- getMatchPredictions(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' match_predictions <- getMatchPredictions(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#' }
getMatchPredictions <- function (
    iteration,
    token,
    host = "https://api.impect.com"
) {

  # check if iteration input is a int
  if (!base::is.numeric(iteration)) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # get match predictions from API
  predictions <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/predictions/match-predictions",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data

  # check if any predictions are available
  if (base::length(predictions) == 0) {
    stop(base::paste0(
      "No match predictions available for iteration ", iteration, "."
    ))
  }

  # flatten data and add iteration ID
  predictions <- jsonlite::flatten(
    predictions %>%
      dplyr::mutate(iterationId = iteration)
  )

  # fix column names using regex
  base::names(predictions) <-
    gsub("\\.(.)", "\\U\\1", base::names(predictions), perl = TRUE)

  # get matches
  matchplan <- getMatches(iteration = iteration, token = token, host = host)

  # get competitions
  iterations <- getIterations(token = token, host = host)

  # merge with other data
  predictions <- predictions %>%
    dplyr::left_join(
      dplyr::select(
        matchplan, .data$id, .data$matchDayIndex, .data$matchDayName,
        .data$homeSquadId, .data$homeSquadName, .data$awaySquadId,
        .data$awaySquadName, .data$scheduledDate
      ),
      by = c("matchId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, .data$id, .data$competitionId, .data$competitionName,
        .data$competitionType, .data$season, .data$competitionGender
      ),
      by = c("iterationId" = "id")
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionId",
    "competitionName",
    "competitionType",
    "season",
    "competitionGender",
    "matchId",
    "matchDayIndex",
    "matchDayName",
    "scheduledDate",
    "homeSquadId",
    "homeSquadName",
    "awaySquadId",
    "awaySquadName",
    "predMarketHome",
    "predMarketAway",
    "predModelHome",
    "predModelAway",
    "predExpertHome",
    "predExpertAway"
  )

  # select columns
  predictions <- predictions %>%
    dplyr::select(dplyr::all_of(order))

  # fix column types
  predictions <- predictions %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("pred"), base::as.numeric))

  # reorder rows
  predictions <- predictions %>%
    dplyr::arrange(.data$matchDayIndex, .data$matchId)

  # return predictions
  return(predictions)
}
