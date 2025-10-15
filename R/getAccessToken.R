#' Get an access token from the 'Impect' Customer API
#'
#' @param username your 'IMPECT' username
#' @param password your 'IMPECT' password
#' @param token_url host specific token url
#'
#' @return a string containing a bearer token
#' @export
#'
#' @examples
#' # Toy example: this will error quickly (no credentials)
#' try(getAccessToken(username = "invalidUser", password = "invalidPassword"))
#'
#' # Real usage: requires valid credentials
#' \dontrun{
#'   token <- getAccessToken(username = "yourUsername", password = "yourPassword")
#' }
getAccessToken <- function(
    username,
    password,
    token_url = "https://login.impect.com/auth/realms/production/protocol/openid-connect/token"
  ) {
  # validate input parameters
  if (missing(username) || missing(password) || username == "" || password == "") {
    stop("Username and password are required.")
  }

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

  # Check response status
  if (httr::http_type(response) != "application/json" ||
      httr::status_code(response) != 200) {
    stop("Failed to retrieve access token.")
  }

  # get access token from response
  token <- httr::content(response)$access_token

  # return token
  return(token)
}
