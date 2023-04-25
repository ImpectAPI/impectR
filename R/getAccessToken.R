#' Get an access token from the Impect Customer API
#'
#' @param username your impect username
#' @param password your impect password
#'
#' @return a string containing a bearer token
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   username <- "yourUsername"
#'   password <- "youPassword"
#'   token <- getAccessToken(username, password)
#' })
#' }
getAccessToken <- function(username, password) {
  # create tokenURL
  token_url <-
    "https://login.impect.com/auth/realms/production/protocol/openid-connect/token"

  # compose login link
  login <- base::paste0(
    "client_id=api&grant_type=password&username=",
    utils::URLencode(username),
    "&password=",
    utils::URLencode(password)
  )

  # define request headers
  headers <-
    base::c("Content-Type" = "application/x-www-form-urlencoded")

  # request access token
  response <-
    httr::RETRY("POST",
                url = token_url,
                body = login,
                httr::add_headers(.headers = headers))

  # check response status
  httr::stop_for_status(response)

  # get access token from response
  token <- httr::content(response)$access_token

  # return token
  return(token)
}
