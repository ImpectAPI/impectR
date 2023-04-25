#' Return a dataframe with basic information for all matches for a given competitionIteration
#'
#' @param competitionIterationId Impect competition iteration ID
#' @param token bearer token
#'
#' @return a dataframe containing all matches for a given competition iteration ID
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   matchplan <- getMatchplan(518, token)
#' })
#' }
getMatchplan <- function(competitionIterationId, token) {
  # get competition iteration data
  response <-
    httr::RETRY(
      "GET",
      url = base::paste0(
        "https://api.impect.com/v4/customerapi/scouting/competitionIterations/",
        competitionIterationId
      ),
      httr::add_headers(
        Authorization = base::paste("Bearer", token, sep =" "))
    )

  # check response status
  httr::stop_for_status(response)

  # get data from response
  data <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # create empty df
  matches <- base::data.frame()

  # iterate over competitionIterationSteps
  for (i in 1:base::length(data$competitionIterationSteps$matches)) {
    # get matches for iteration
    temp <- data$competitionIterationSteps$matches[[i]]
    # append to matches dataframe
    matches <- dplyr::bind_rows(matches, temp)
  }

  # convert to dataframe
  matches <- jsonlite::flatten(matches)

  # fix column names using regex
  base::names(matches) <-
    gsub("\\.(.)", "\\U\\1", base::names(matches), perl = TRUE)
  base::names(matches) <-
    gsub("Competition", "", base::names(matches), perl = TRUE)

  # return matches
  return(matches)
}
