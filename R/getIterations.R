#' Return a dataframe containing all iterations available to the user
#'
#' @param token bearer token
#'
#' @export

#' @importFrom dplyr %>%
#' @return a dataframe containing all iterations available to the user
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   competitionIterations <- getCompetitions(token)
#' })
#' }
getIterations <- function(token) {
  # get iteration data
  iterations <- .iterations(token = token)

  # clean data
  iterations <- .cleanData(iterations)

  # return dataframe
  return(iterations)
}
