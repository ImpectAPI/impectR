#' Return a dataframe that contains squad ratings for a given iteration ID
#'
#' @param iteration Impect iteration ID
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the squad ratings for the given iteration ID
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   ratings <- getSquadRatings(1005, token)
#' })
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
    dplyr::select(id, name, idMappings) %>%
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
    tidyr::unnest(squadRatings) %>%
    dplyr::left_join(
      dplyr::select(
        squads, id, wyscoutId, heimSpielId, skillCornerId, squadName = name
      ),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, id, competitionId, competitionName,
        competitionType, season, competitionGender
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
