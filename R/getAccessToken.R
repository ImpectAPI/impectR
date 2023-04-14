######
#
# This function returns an access token for the external API
#
######

getAccessToken <- function(username, password) {
  # require packages
  require(httr)
  require(utils)
  
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
    httr::POST(url = token_url,
               body = login,
               httr::add_headers(.headers = headers))
  
  # check response status
  httr::stop_for_status(response)
  
  # get access token from response
  token <- httr::content(response)$access_token
  
  # return token
  return(token)
}