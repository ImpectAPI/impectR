#' Return a dataframe that contains season simulations for a given iteration ID
#'
#' For every simulation date, a season simulation shows the actual table, the
#' expected final points and goals and the probability of every finishing
#' position of each squad. The rank probabilities are expanded into one column
#' per finishing position (\code{rank1Probability}, \code{rank2Probability},
#' ...) and refer to the squad's own group.
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
#' @param date optional date (\code{"YYYY-MM-DD"}) to only return the latest
#' simulation on or before that date
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the season simulations for the given
#' iteration ID, or an empty dataframe if no simulations are available
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(season_simulations <- getSeasonSimulations(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' season_simulations <- getSeasonSimulations(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#'
#' # get the current simulation only
#' current_simulation <- getSeasonSimulations(
#'   iteration = 1004,
#'   token = "yourToken",
#'   date = base::format(base::Sys.Date())
#' )
#' }
getSeasonSimulations <- function (
    iteration,
    token,
    date = NULL,
    host = "https://api.impect.com"
) {

  # check if iteration input is a int
  if (!base::is.numeric(iteration)) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # check if date input is a string in the format YYYY-MM-DD
  if (!base::is.null(date) && (
    !base::is.character(date) || base::length(date) != 1 ||
    !base::grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
  )) {
    stop("Unprocessable type for 'date' variable. Use the format 'YYYY-MM-DD'")
  }

  # construct suffix
  suffix <- "/predictions/season-simulations"
  if (!base::is.null(date)) {
    suffix <- base::paste0(suffix, "?date=", date)
  }

  # get season simulations from API
  predictions <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/iterations/",
        id = iteration,
        suffix = suffix,
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data$predictions

  # return empty dataframe if no simulations are available
  if (base::length(predictions) == 0) {
    return(tibble::tibble())
  }

  # unnest squad predictions and add iteration ID
  simulations <- predictions %>%
    dplyr::mutate(iterationId = iteration) %>%
    tidyr::unnest("squadPredictions")

  # expand rank probabilities into one column per finishing position (the
  # number of ranks may differ between groups, so missing ranks are NA)
  probabilities <- simulations$rankProbabilities
  if (base::is.matrix(probabilities)) {
    probabilities <- base::split(probabilities, base::row(probabilities))
  }
  rank_columns <- base::paste0(
    "rank", base::seq_len(base::max(base::lengths(probabilities))),
    "Probability"
  )
  for (rank in base::seq_along(rank_columns)) {
    simulations[[rank_columns[rank]]] <- purrr::map_dbl(
      probabilities,
      ~ if (base::length(.x) >= rank) base::as.numeric(.x[[rank]]) else NA_real_
    )
  }

  # get squad master data from API
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
    jsonlite::flatten() %>%
    dplyr::select(.data$id, .data$name, .data$idMappings) %>%
    base::unique()

  # clean data
  squads <- .cleanData(squads)

  # get competitions
  iterations <- getIterations(token = token, host = host)

  # merge with other data
  simulations <- simulations %>%
    dplyr::left_join(
      dplyr::select(
        squads, .data$id, .data$wyscoutId, .data$heimSpielId,
        .data$skillCornerId, .data$optaId, .data$statsPerformId,
        .data$transfermarktId, .data$soccerdonnaId, .data$dflId,
        squadName = .data$name
      ),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, .data$id, .data$competitionId, .data$competitionName,
        .data$competitionType, .data$season, .data$competitionGender
      ),
      by = c("iterationId" = "id")
    )

  # fix column types
  simulations <- simulations %>%
    dplyr::mutate(
      stageName = base::as.character(.data$stageName),
      groupName = base::as.character(.data$groupName),
      dplyr::across(
        c("matches", "points", "goalsFor", "goalsAgainst"),
        base::as.integer
      ),
      dplyr::across(
        c("pointsProjected", "goalsForProjected", "goalsAgainstProjected"),
        base::as.numeric
      )
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionId",
    "competitionName",
    "competitionType",
    "season",
    "competitionGender",
    "date",
    "stageName",
    "groupName",
    "squadId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "optaId",
    "statsPerformId",
    "transfermarktId",
    "soccerdonnaId",
    "dflId",
    "squadName",
    "matches",
    "points",
    "goalsFor",
    "goalsAgainst",
    "pointsProjected",
    "goalsForProjected",
    "goalsAgainstProjected",
    rank_columns
  )

  # select columns
  simulations <- simulations %>%
    dplyr::select(dplyr::all_of(order))

  # reorder rows (squads without a group first)
  simulations <- simulations %>%
    dplyr::arrange(
      .data$date, !base::is.na(.data$groupName), .data$groupName,
      .data$squadId
    )

  # return simulations
  return(simulations)
}
