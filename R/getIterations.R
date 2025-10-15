#' Return a dataframe containing all iterations available to the user
#'
#' @param token bearer token
#' @param host host environment
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
getIterations <- function(token, host = "https://api.impect.com") {
  # get iteration data from API
  iterations <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/iterations",
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
