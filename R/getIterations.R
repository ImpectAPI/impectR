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
  iterations <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/iterations",
        token = token
        ),
      "text",
      encoding = "UTF-8"
      )
    )$data


  # clean data
  iterations <- .cleanData(iterations)

  # return dataframe
  return(iterations)
}
