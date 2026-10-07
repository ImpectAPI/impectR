#' Return a dataframe containing all iterations available to the user
#'
#' @param token bearer token
#' @param host host environment
#'
#' @export

#' @importFrom dplyr %>%
#' @importFrom rlang .data
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

  # get countries
  countries <- .getCountries(token = token, host = host)

  # merge with countries
  iterations <- iterations %>%
    dplyr::left_join(
      dplyr::select(
        countries, .data$id, competitionCountryName = .data$fifaName
      ),
      by = c("competitionCountryId" = "id")
    )

  # define column order
  order <- c(
    "id",
    "competitionId",
    "competitionName",
    "season",
    "competitionType",
    "competitionCountryId",
    "competitionCountryName",
    "competitionGender",
    "competitionAgeGroup",
    "dataVersion",
    "lastChangeTimestamp",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "optaId",
    "statsPerformId",
    "transfermarktId",
    "soccerdonnaId",
    "dflId"
  )

  # select columns
  iterations <- iterations %>%
    dplyr::select(dplyr::all_of(order))

  # return dataframe
  return(iterations)
}
