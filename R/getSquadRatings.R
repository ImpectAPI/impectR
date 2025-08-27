#' Return a dataframe that contains squad ratings for a given iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
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
getSquadRatings <- function (iteration, token) {

  # check if iteration input is a int
  if (!base::is.numeric(iteration)) {
    stop("Unprocessable type for 'iteration' variable")
  }
  # get squad amster data from API
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

  # clean data
  squads <- .cleanData(squads)

  # get squad ratings from API
  ratings <- jsonlite::flatten(
    jsonlite::fromJSON(
      httr::content(
        .callAPIlimited(
          base_url = "https://api.impect.com/v5/customerapi/iterations/",
          id = iteration,
          suffix = "/squads/ratings",
          token = token
        ),
        "text",
        encoding = "UTF-8"
        )
    )$data$squadRatingsEntries %>%
      dplyr::mutate(iterationId = iteration)
  )

  # fix column names using regex
  base::names(ratings) <-
    gsub("\\.(.)", "\\U\\1", base::names(ratings), perl = TRUE)

  # get competitions
  iterations <- getIterations(token = token)

  # merge with other data
  ratings <- ratings %>%
    tidyr::unnest(.data$squadRatings) %>%
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
    "date",
    "squadId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "squadName",
    "value"
  )

  # select columns
  ratings <- ratings %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(ratings)
}
