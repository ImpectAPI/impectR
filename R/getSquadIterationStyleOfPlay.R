#' Return a dataframe that contains the squad style of play values for a given
#' iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the style of play values per squad for the
#' given iteration ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(squad_style_of_play <- getSquadIterationStyleOfPlay(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' squad_style_of_play <- getSquadIterationStyleOfPlay(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#' }
getSquadIterationStyleOfPlay <- function (
    iteration,
    token,
    host = "https://api.impect.com"
) {

  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # get squads master data from API
  squads <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/squads",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::select(.data$id, .data$name, .data$idMappings) %>%
    base::unique()

  # clean data
  squads <- .cleanData(squads)

  # get squad iteration style of play from API
  styles_raw <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/squad-style-of-play",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    dplyr::mutate(iterationId = iteration)

  # get style of play definitions from API
  styles_definitions <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/squad-style-of-play",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::pull(.data$styleOfPlayName)

  # get competitions
  iterations <- getIterations(token = token, host = host)

  # unnest style of play values into long format
  styles <- dplyr::bind_rows(
    tibble::tibble(
      squadId = base::integer(),
      styleOfPlayName = base::character(),
      value = base::numeric()
    ),
    purrr::map2_dfr(
      styles_raw$squadId,
      styles_raw$styleOfPlay,
      ~ if (base::is.data.frame(.y) && base::nrow(.y) > 0) {
        tibble::tibble(
          squadId = .x,
          styleOfPlayName = .y$styleOfPlayName,
          value = .y$value
        )
      }
    )
  )

  # pivot style of play values
  styles <- styles %>%
    tidyr::pivot_wider(
      id_cols = "squadId",
      names_from = "styleOfPlayName",
      values_from = "value",
      values_fn = base::sum
    )

  # ensure all styles are present
  for (style in base::setdiff(styles_definitions, base::names(styles))) {
    styles[[style]] <- NA_real_
  }

  # merge with squads and matches played (keeps squads without style of play
  # values)
  styles <- styles_raw %>%
    dplyr::select(.data$iterationId, .data$squadId, .data$matches) %>%
    dplyr::left_join(styles, by = c("squadId" = "squadId"))

  # merge with other data
  styles <- styles %>%
    dplyr::left_join(
      dplyr::select(
        iterations, .data$id, .data$competitionId, .data$competitionName,
        .data$competitionType, .data$season
      ),
      by = c("iterationId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        squads, .data$id, .data$wyscoutId, .data$heimSpielId,
        .data$skillCornerId, .data$optaId, .data$statsPerformId,
        .data$transfermarktId, .data$soccerdonnaId, .data$dflId,
        squadName = .data$name
      ),
      by = c("squadId" = "id")
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionName",
    "season",
    "squadId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "optaId",
    "statsPerformId",
    "transfermarktId",
    "soccerdonnaId",
    "dflId",
    "squadName",
    "matches",
    styles_definitions
  )

  # select columns
  styles <- styles %>%
    dplyr::select(dplyr::all_of(order))

  # return style of play
  return(styles)
}
