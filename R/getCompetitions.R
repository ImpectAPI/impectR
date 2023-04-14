#' Return a dataframe containing all competitionIterations available to the user
#'
#' @param token bearer token
#'
#' @return a dataframe containing all competition iterations available to the user
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   competitionIterations <- getCompetitions(token)
#' })
#' }
getCompetitions <- function(token) {
  # request competition iteration information from API
  response <-
    httr::GET(url = "https://api.impect.com/v4/customerapi/scouting/competitionIterations/",
              httr::add_headers(Authorization = base::paste("Bearer", token, sep = " ")))

  # check response status
  httr::stop_for_status(response)

  # get data from response
  data <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # convert to dataframe
  df <- jsonlite::flatten(base::as.data.frame(data))

  # fix column names using regex
  base::names(df) <-
    base::gsub("\\.(.)", "\\U\\1", base::names(df), perl = TRUE)

  # return dataframe
  return(df)
}
