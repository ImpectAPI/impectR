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
.callAPI <- function(base_url, id = "", suffix = "", token) {
  # get API response
  response <-
    httr::RETRY(
      "GET",
      url = paste0(base_url, id, suffix),
      httr::add_headers(
        Authorization = base::paste("Bearer", token, sep = " "))
      )

  # break if status code != 200
  if (httr::http_error(response)) {
    base::stop(
      base::paste0(
        "Error: HTTP ",
        httr::content(response)$status,
        ": ",
        dplyr::coalesce(
          httr::content(response)$message,
          "No specific error message."
        )
      )
    )
  }

  # return response
  return(response)
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


#' Return a dataframe that contains all events for a given match ID
#'
#' @noRd
#'
#' @param match Impect match ID
#' @param token bearer token
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all events for the given match ID
.eventAttributes <- function(match, token) {
  # get match events
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/matches/",
    id = match,
    suffix = "/events",
    token = token
  )

  # convert to dataframe and add column with matchId
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data %>%
    dplyr::mutate(matchId = match)

  # convert to dataframe
  temp <- jsonlite::flatten(temp)

  # fix column names using regex
  base::names(temp) <-
    gsub("\\.(.)", "\\U\\1", base::names(temp), perl = TRUE)

  # return event data
  return(temp)
}


#' Return a dataframe that contains all player matchsums for a given match ID
#'
#' @noRd
#'
#' @param match Impect match ID
#' @param token bearer token
#'
#' @return a dataframe containing all player matchsums for the given match ID
.playerMatchsums <- function(match, token) {
  # get player matchsums
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/matches/",
    id = match,
    suffix = "/player-kpis",
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # return player matchsums
  return(temp)
}


#' Return a dataframe that contains all squad matchsums for a given match ID
#'
#' @noRd
#'
#' @param match Impect match ID
#' @param token bearer token
#'
#' @return a dataframe containing all squad matchsums for the given match ID
.squadMatchsums <- function(match, token) {
  # get squad matchsums
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/matches/",
    id = match,
    suffix = "/squad-kpis",
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # return squad matchsums
  return(temp)
}


#' Return a dataframe that contains all KPI values for each event for a given match ID
#'
#' @noRd
#'
#' @param match Impect match ID
#' @param token bearer token
#'
#' @return a dataframe containing all KPI values for each event for a given match ID
.eventScorings <- function(match, token) {
  # get event kpi values
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/matches/",
    id = match,
    suffix = "/event-kpis",
    token = token
    )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # return event kpi values
  return(temp)
}


#' Returns the matchInfo for a given match ID
#'
#' @noRd
#'
#' @param match Impect match ID
#' @param token bearer token
#'
#' @return the iterationId for a given match ID
.matchInfo <- function (match, token) {
  # get iterations
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/matches/",
    id = match,
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # return matchInfo
  return(temp)
}


#' Returns all iterations the user has access to
#'
#' @noRd
#'
#' @param token bearer token
#'
#' @return the iterationId for a given match ID
.iterations <- function (token) {
  # get iterations
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/iterations",
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # return iterations
  return(temp)
}


#' Return a dataframe that contains all players for a given iteration ID
#'
#' @noRd
#'
#' @param iteration Impect iteration ID
#' @param token bearer token
#'
#' @return a dataframe containing all players for a given iteration ID
.playerNames <- function (iteration, token) {
  # get players
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/iterations/",
    id = iteration,
    suffix = "/players",
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data

  # return players
  return(temp)
}


#' Return a dataframe that contains all squads for a given iteration ID
#'
#' @noRd
#'
#' @param iteration Impect iteration ID
#' @param token bearer token
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all squads for a given iteration ID
.squadNames <- function (iteration, token) {
  # get squads
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/iterations/",
    id = iteration,
    suffix = "/squads",
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data %>%
    jsonlite::flatten()

  # fix column names using regex
  base::names(temp) <-
    gsub("\\.(.)", "\\U\\1", base::names(temp), perl = TRUE)

  # return squads
  return(temp)
}


#' Return a dataframe that contains all matches for a given iteration ID
#'
#' @noRd
#'
#' @param iteration Impect iteration ID
#' @param token bearer token
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all matches for a given iteration ID
.matches <- function (iteration, token) {
  # get matches
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/iterations/",
    id = iteration,
    suffix = "/matches",
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data %>%
    jsonlite::flatten()

  # fix column names using regex
  base::names(temp) <-
    base::gsub("\\.(.)", "\\U\\1", base::names(temp), perl = TRUE)

  # return matches
  return(temp)
}


#' Return a dataframe that contains all iteration averages for a given iteration
#'  ID for a given squad ID
#'
#' @noRd
#'
#' @param iteration Impect iteration ID
#' @param match Impect match ID
#' @param token bearer token
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all iteration averages for a given iteration
#' ID for a given squad ID
.playerIterationAverages <- function(iteration, squad, token) {
  # get .player iteration averages
  response <- .callAPIlimited(
    base_url = paste0(
      "https://api.impect.com/v5/customerapi/iterations/",
      iteration,
      "/squads/",
      squad,
      "/player-kpis"
    ),
    token = token
  )

  # convert to dataframe and add columns with matchId and iterationId
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data %>%
    dplyr::mutate(squadId = squad,
                  iterationId = iteration)

  # return averages data
  return(temp)
}


#' Return a dataframe that contains all iteration averages for a given iteration
#'  ID for all squads
#'
#' @noRd
#'
#' @param iteration Impect iteration ID
#' @param token bearer token
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all events for the given match ID
.squadIterationAverages <- function(iteration, token) {
  # get squad iteration averages
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/iterations/",
    id = iteration,
    suffix = "/squad-kpis",
    token = token
  )

  # convert to dataframe and add column with iterationId
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data %>%
    dplyr::mutate(iterationId = iteration)

  # return averages data
  return(temp)
}


#' Return a dataframe that contains all countries
#'
#' @noRd
#'
#' @param token bearer token
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all countries
.countryNames <- function (token) {
  # get squads
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/countries/",
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data %>%
    jsonlite::flatten()

  # return squads
  return(temp)
}


#' Return a dataframe that contains kpi IDs and names
#'
#' @noRd
#'
#' @param token bearer token
#' @param scope scope of kpis to be returned: all or event level only
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing kpis IDs and names
.kpis <- function (token, scope = "") {
  # get squads
  response <- .callAPIlimited(
    base_url = "https://api.impect.com/v5/customerapi/kpis/",
    suffix = scope,
    token = token
  )

  # convert to dataframe
  temp <-
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data %>%
    jsonlite::flatten()

  # return squads
  return(temp)
}


#' Processes a dataframe and fixes column names and extracts the first mapping
#' ID for SkillCOrner and Heimspiel
#'
#' @noRd
#'
#' @param data a data fram
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
    base::gsub("\\.(.)", "\\U\\1", base::names(data), perl = TRUE)
  base::names(data) <-
    base::gsub("\\_(.)", "\\U\\1", base::names(data), perl = TRUE)


  # iterate over the skillCorner and heimSpiel column and keep first value
  data$skillCorner <-
    purrr::map(data$skillCorner, ~ ifelse(length(.x) > 0, .x[[1]], NA))
  data$skillCorner <- base::sapply(data$skillCorner, function(x)
    x[[1]])
  data$skillCorner <-
    base::sapply(data$skillCorner, function(x)
      ifelse(length(x) > 0, x[[1]], NA))
  data$heimSpiel <-
    purrr::map(data$heimSpiel, ~ .x[!sapply(.x, is.null)])
  data$heimSpiel <- base::sapply(data$heimSpiel, function(x)
    x[[1]])
  data$heimSpiel <-
    base::sapply(data$heimSpiel, function(x)
      ifelse(length(x) > 0, x[[1]], NA))

  # edit column names
  data <- data %>%
    dplyr::rename(skillCornerId = "skillCorner",
                  heimSpielId = "heimSpiel")

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
