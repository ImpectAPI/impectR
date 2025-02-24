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
  if (!base::is.numeric(matches)) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # apply squadNames function to an iteration
  squads <- .squadNames(iteration = iteration, token = token) %>%
    dplyr::select(id, name, idMappings) %>%
    base::unique()

  # clean data
  squads <- .cleanData(squads)

  # apply squadRatings function to an iteration
  ratings <- .squadRatings(iteration = iteration, token = token)

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
