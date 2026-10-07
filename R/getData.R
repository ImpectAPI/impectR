#' Send a request to any 'IMPECT' API endpoint and return the response as a
#' flattened dataframe
#'
#' Use this function to access endpoints that are not wrapped by a dedicated
#' function of this package. The request respects the same rate limits as all
#' other functions. Nested fields of the response are flattened into separate
#' columns whose names are converted to camel case (e.g. \code{squadHome.id}
#' becomes \code{squadHomeId}).
#'
#' @param url endpoint path (e.g. \code{"/v5/customerapi/countries"}), which is
#' prefixed with \code{host}, or full URL, which must start with \code{host}
#' @param token bearer token
#' @param method HTTP method
#' @param data optional request body as a list, sent as JSON
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the flattened \code{data} of the response,
#' or an empty dataframe if the response has no content
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(countries <- getData(
#'   url = "/v5/customerapi/countries",
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' # get the list of countries, which is not wrapped by a dedicated function
#' countries <- getData(
#'   url = "/v5/customerapi/countries",
#'   token = "yourToken"
#' )
#' }
getData <- function (
    url,
    token,
    method = "GET",
    data = NULL,
    host = "https://api.impect.com"
) {

  # check input for url argument
  if (!base::is.character(url) || base::length(url) != 1) {
    stop("Unprocessable type for 'url' variable")
  }

  # remove trailing slash from host
  host <- base::sub("/+$", "", host)

  if (base::grepl("^[A-Za-z][A-Za-z0-9+.-]*://", url)) {
    # only send the token to full URLs on the given host
    if (!base::startsWith(url, base::paste0(host, "/"))) {
      stop(
        base::sprintf(
          "Full URLs must start with the host '%s'. Pass an endpoint path instead.",
          host
        )
      )
    }
  } else {
    # prefix relative paths with the host
    if (!base::startsWith(url, "/")) {
      url <- base::paste0("/", url)
    }
    url <- base::paste0(host, url)
  }

  # get response from API
  response <- .callAPIlimited(
    host = "",
    base_url = url,
    token = token,
    method = base::toupper(method),
    body = data
  )

  # get response content
  content <- httr::content(response, "text", encoding = "UTF-8")

  # return empty dataframe if response has no content (e.g. 204 No Content)
  if (httr::status_code(response) == 204 || base::is.na(content) ||
      !base::nzchar(base::trimws(content))) {
    return(tibble::tibble())
  }

  # parse response content
  response <- jsonlite::fromJSON(content)$data

  # check if response contains data
  if (base::length(response) == 0) {
    stop(
      base::sprintf(
        "The %s endpoint returned no data/ an empty list.", url
      )
    )
  }

  # convert to dataframe
  if (base::is.data.frame(response)) {
    # list of objects
    response <- jsonlite::flatten(response)
  } else if (base::is.list(response) && !base::is.null(base::names(response))) {
    # single object -> one row
    response <- .flattenObject(response)
  } else {
    # list of scalar values
    response <- tibble::tibble(value = response)
  }

  # fix column names using regex
  base::names(response) <-
    gsub("\\.(.)", "\\U\\1", base::names(response), perl = TRUE)

  # return dataframe
  return(tibble::as_tibble(response))
}


#' Flatten a single JSON object into a one-row dataframe, joining the names of
#' nested objects with a dot
#'
#' @noRd
#'
#' @param x named list as returned by jsonlite::fromJSON() for a JSON object
#' @param prefix prefix for the column names
#'
#' @return a one-row tibble
.flattenObject <- function (x, prefix = "") {

  columns <- base::list()

  for (name in base::names(x)) {
    value <- x[[name]]
    column <- base::paste0(prefix, name)

    if (base::is.list(value) && !base::is.data.frame(value) &&
        !base::is.null(base::names(value))) {
      # nested object -> flatten recursively
      columns <- base::c(
        columns,
        base::as.list(.flattenObject(value, base::paste0(column, ".")))
      )
    } else if (base::is.atomic(value) && base::length(value) == 1) {
      # scalar value
      columns[[column]] <- value
    } else {
      # arrays, lists of objects and nulls -> keep as list column
      columns[[column]] <- base::list(value)
    }
  }

  tibble::as_tibble(columns)
}
