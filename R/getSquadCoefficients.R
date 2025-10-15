#' Return a dataframe that contains squad ratings for a given iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the squad ratings for the given iteration ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(squad_ratings <- getSquadRatings(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' squad_ratings <- getSquadRatings(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#' }
getSquadCoefficients <- function (
    iteration,
    token,
    host = "https://api.impect.com"
) {

  # check if iteration input is a int
  if (!base::is.numeric(iteration)) {
    stop("Unprocessable type for 'iteration' variable")
  }
  # get squad amater data from API
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

  # get squad ratings from API
  coefficients <- jsonlite::flatten(
    jsonlite::fromJSON(
      httr::content(
        .callAPIlimited(
          host,
          base_url = "/v5/customerapi/iterations/",
          id = iteration,
          suffix = "/predictions/model-coefficients",
          token = token
        ),
        "text",
        encoding = "UTF-8"
      )
    )$data$entries %>%
      dplyr::mutate(iterationId = iteration)
  )

  # fix column names using regex
  base::names(coefficients) <-
    gsub("\\.(.)", "\\U\\1", base::names(coefficients), perl = TRUE)

  # get competitions
  iterations <- getIterations(token = token, host = host)

  # unnest squads column
  coefficients <- coefficients %>%
    tidyr::unnest(.data$squads)

    # merge with other data
  coefficients <- coefficients %>%
    dplyr::left_join(
      dplyr::select(
        squads, .data$id, .data$wyscoutId, .data$heimSpielId,
        .data$skillCornerId, squadName = .data$name
      ),
      by = c("id" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, .data$id, .data$competitionName,
        .data$competitionType, .data$season, .data$competitionGender
      ),
      by = c("iterationId" = "id")
    )

  # rename columns
  coefficients <- coefficients %>%
    dplyr::rename(
      squadId = .data$id,
      attackCoefficient = .data$att,
      defenseCoefficient = .data$def,
      interceptCoefficient = .data$competitionIntercept,
      homeCoefficient = .data$competitionHome,
      competitionCoefficient = .data$competitionComp
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionId",
    "competitionName",
    "competitionType",
    "season",
    "competitionGender",
    "interceptCoefficient",
    "homeCoefficient",
    "competitionCoefficient",
    "date",
    "squadId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "squadName",
    "attackCoefficient",
    "defenseCoefficient"
  )

  # select columns
  coefficients <- coefficients %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(coefficients)
}
