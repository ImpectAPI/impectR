#' Return the response from the given API endpoint with optional retries
#' in case of non-200 status code. Raises an error after three unsuccessful
#' tries.
#'
#' @noRd
#'
#' @param base_url API endpoint URL
#' @param id The id of the object to be retrieved
#' @param suffix The suffix of the endpoint URL that comes after the id
#' @param token Bearer token
#'
#' @return Response content of the API endpoint
.callAPI <- function(base_url, id = "", suffix = "", token,
                     max_retries = 3, retry_delay = 1) {

  # try API call
  for (i in 1:max_retries) {
    # get API response
    response <-
      httr::GET(
        url = base::paste0(base_url, id, suffix),
        httr::add_headers(
          Authorization = base::paste("Bearer", token, sep = " "))
        )

    # return the response if status code is 200
    if (httr::status_code(response) == 200) {
      return(response)
    } else if (httr::status_code(response) == 429) {
      # handle rate limiting (429 status code)
      message <- httr::content(response, "parsed")$message
      base::cat(base::paste("Received status code 429 (", message,
                "), retrying in", retry_delay, "seconds...\n"))
      Sys.sleep(retry_delay)
    } else if (httr::status_code(response) %in% c(401, 403)) {
      # handle unauthorized or forbidden (401 or 403 status codes)
      message <- dplyr::coalesce(
        httr::content(response, "parsed")$message,
        "Unauthorized"
      )
      stop(base::paste("Received status code", httr::status_code(response),
                 "(", message, ")"))
    } else {
      # handle other errors
      message <- dplyr::coalesce(
        httr::content(response, "parsed")$message,
        "Unknown error"
      )
      stop(base::paste("Received status code", httr::status_code(response),
                 "(", message, ")"))
    }
  }
}

#' Applies the rate limit policy to the .callAPI function
#'
#' @noRd
#'
#' @param base_url Impect API endpoint URL
#' @param id the id of the object to be retrieved
#' @param suffix suffix of the endpoint URL that comes after the id
#' @param token bearer token
#'
#' @return a dataframe containing the response of an API endpoint
.callAPIlimited <- function(base_url, id = "", suffix = "", token) {

  # check if Token bucket exist and create it if not
  if (!exists("bucket")) {

    # get response from API
    response <- .callAPI(base_url, id, suffix, token)

    # get rate limit policy
    policy <- response[["all_headers"]][[1]][["headers"]][["ratelimit-policy"]]

    # extract maximum requests using regex
    capacity <- as.numeric(gsub(";.*", "", policy))

    # extract time window using regex
    intervall <- as.numeric(gsub(".*w=(\\d+).*", "\\1", policy))


    # create TokenBucket
    bucket <<- TokenBucket(
      # set default rate, capacity, available tokens and time
      capacity = capacity,
      intervall = intervall,
      tokens = as.numeric(
        response[["all_headers"]][[1]][["headers"]][["ratelimit-remaining"]]
        ),
      last_update = as.numeric(Sys.time())
    )

    return(response)
  }

  # check if a token is available
  if (bucket$isTokenAvailable()) {
    # get API response
    response <- .callAPI(base_url, id, suffix, token)

    # consume a token
    bucket$consumeToken()
  } else {
    # wait for bucket intervall
    Sys.sleep(bucket$intervall)

    # call function again
    response <- .callAPIlimited(base_url, id, suffix, token)
  }

  # return response
  return(response)
}


#' Processes a dataframe and fixes column names and extracts the first mapping
#' ID for SkillCOrner and Heimspiel
#'
#' @noRd
#'
#' @param data a data frame
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing clean columns names and SkillCorner/HeimSpiel
#' columns
.cleanData <- function (data) {

  # unnest mappings column
  data <- data %>%
    jsonlite::flatten() %>%
    tidyr::unnest_wider("idMappings")

  # fix column names using regex
  base::names(data) <-
    base::gsub("[\\.\\_](.)", "\\U\\1", base::names(data), perl = TRUE)

  # iterate over the skillCorner, heimSpiel and wyscout columns and keep first
  # value only
  for (provider in c("skillCorner", "heimSpiel", "wyscout")) {
    # drop null values in lists und keep first list entry
    data[[provider]] <-
      purrr::map(data[[provider]], ~ purrr::discard(.x, is.null)[[1]])
    # replace empty lists with NAs
    data[[provider]] <-
      purrr::map(
        data[[provider]],
        ~ base::ifelse(length(.x) == 0, NA, .x)
      )
  }

  # edit column names
  data <- data %>%
    dplyr::rename(skillCornerId = "skillCorner",
                  heimSpielId = "heimSpiel",
                  wyscoutId = "wyscout")

  # edit column types
  data <- data %>%
    mutate(
      skillCornerId = as.integer(skillCornerId),
      heimSpielId = as.integer(heimSpielId),
      wyscoutId = as.integer(wyscoutId)
    )

  # return squads
  return(data)
}


#' TokenBucket Class
#'
#' This class represents a token bucket, which is a rate limiting mechanism used
#' to control the frequency of certain actions or requests based on available tokens.
#' Tokens are added to the bucket at a specified rate and can be consumed when needed.
#'
#' @name TokenBucket
#'
#' @slot capacity the maximum amount of tokens in the bucket
#' @slot tokens the amount of token currently available in the bucket
#' @slot intervall the duration in seconds after which the bucket gets refilled
#' @slot last_update the timestamp from the liast time tokens were added or consumed
#'
#' @importFrom methods new
#'
#' @export
#'
#' @keywords classes
#'
#' @examples
#' \donttest{
#' try({
#' # create token bucket
#' bucket <- TokenBucket(
#'   capacity = 10,
#'   tokens = 10,
#'   intervall = 1,
#'   last_update = Sys.Time()
#' )
#'
#' # add tokens to bucket
#' bucket$addTokens()
#'
#' # check if a token is available
#' bucket$isTokenAvailable
#'
#' # consume token
#' bucket$consumeToken
#' })
#' }
TokenBucket <- setRefClass(
  "TokenBucket",
  fields = list(
    capacity = "numeric",     #' Capacity of the token bucket.
    tokens = "numeric",       #' Number of tokens currently available.
    intervall = "numeric",    #' Time interval for token replenishment.
    last_update = "numeric"   #' Last time the token bucket was updated.
  ),
  methods = list(

    addTokens = function() {
      current_time <- as.numeric(Sys.time())
      elapsed_time <- current_time - last_update
      if (elapsed_time >= 1) {
        tokens <<- capacity
        last_update <<- current_time
      }
    },

    isTokenAvailable = function() {
      addTokens()
      return(tokens >= 1)
    },

    consumeToken = function() {
      if (!isTokenAvailable()) {
        return(FALSE)
      }
      tokens <<- tokens - 1
      return(TRUE)
    }
  )
)
