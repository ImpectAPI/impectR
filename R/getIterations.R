#' Return a dataframe containing all iterations available to the user
#'
#' @param token bearer token
#'
#' @export

#' @importFrom dplyr %>%
#' @return a dataframe containing all iterations available to the user
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(events <- getIterations(
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' iterations <- getIterations(
#'   token = "yourToken"
#' )
#' }
getIterations <- function(token) {
  # get iteration data from API
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
