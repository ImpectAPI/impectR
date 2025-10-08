#' Return a dataframe that contains all starting formations for a set of given
#' match IDs
#'
#' @param matches 'IMPECT' match ID or a list of match IDs
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing all starting formations for a set of given
#' match IDs
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(events <- getFormations(
#'   matches = c(0, 1),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' formations <- getFormations(
#'   matches = c(84248, 158150),
#'   token = "yourToken"
#' )
#' }
getFormations <- function (
    matches,
    token,
    host = "https://api.impect.com"
) {

  # check if match input is not a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # get matchInfo from API
  match_info <- purrr::map_df(
    matches,
    ~ {
      response <- jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            host,
            base_url = "/v5/customerapi/matches/",
            id = .,
            token = token
          ),
          "text",
          encoding = "UTF-8"
        )
      )$data

      # extract top-level fields and convert to a single-row tibble
      tibble::tibble(
        id = response$id,
        dateTime = response$dateTime,
        lastCalculationDate = response$lastCalculationDate,
        iterationId = response$iterationId,

        squadHomeId = response$squadHome$id,
        squadHomeFormations = list(response$squadHome$formations),

        squadAwayId = response$squadAway$id,
        squadAwayFormations = list(response$squadAway$formations),
      )
    }
  ) %>%
    base::unique()

  # filter for fail matches
  fail_matches <- match_info %>%
    dplyr::select(.data$id, .data$lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == TRUE) %>%
    dplyr::pull(.data$id)

  # filter for available matches
  matches <- match_info %>%
    dplyr::select(.data$id, .data$lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == FALSE) %>%
    dplyr::pull(.data$id)

  # raise warnings
  if (base::length(fail_matches) > 0) {
    if (base::length(matches) == 0) {
      base::stop("All supplied matches are unavailable. Execution stopped.")
    }
    else {
      base::warning(
        base::sprintf(
          "The following matches are not available yet and were ignored:\n\t%s",
          base::paste(fail_matches, collapse = ", ")
        )
      )
    }
  }

  # get unique iterationIds
  iterations <- match_info %>%
    dplyr::pull(.data$iterationId) %>%
    base::unique()

  # get squad master data from API
  squads <-
    purrr::map_df(
      iterations,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            host,
            base_url = "/v5/customerapi/iterations/",
            id = .,
            suffix = "/squads",
            token = token
          ),
          "text",
          encoding = "UTF-8"
        )
      )$data %>%
        jsonlite::flatten()
    ) %>%
    dplyr::select(.data$id, .data$name) %>%
    base::unique()

  # fix column names using regex
  base::names(squads) <-
    gsub("\\.(.)", "\\U\\1", base::names(squads), perl = TRUE)

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations, ~ getMatches(
      iteration = .,
      token = token,
      host = host)
    )

  # get iterations
  iterations <- getIterations(token = token, host = host)

  # extract formations
  formations_home <- match_info %>%
    dplyr::select(
      .data$id,
      squadId = .data$squadHomeId,
      squadFormations = .data$squadHomeFormations
    )

  formations_away <- match_info %>%
    dplyr::select(
      .data$id,
      squadId = .data$squadAwayId,
      squadFormations = .data$squadAwayFormations
    )

  # combine data frames
  formations <- dplyr::bind_rows(formations_home, formations_away)

  # unnest formations column
  formations <- formations %>%
    tidyr::unnest_longer(.data$squadFormations)

  # normalize the JSON structure into separate columns
  formations <- formations %>%
    tidyr::unnest(.data$squadFormations)

  # start merging dfs

  # merge formations with squads
  formations <- formations %>%
    dplyr::left_join(
      dplyr::select(squads, squadId = .data$id, squadName = .data$name),
      by = base::c("squadId" = "squadId")
    )

  # merge with matches info
  formations <- formations %>%
    dplyr::left_join(
      dplyr::select(matchplan, .data$id, .data$skillCornerId, .data$heimSpielId,
                    .data$wyscoutId, .data$matchDayIndex, .data$matchDayName,
                    .data$scheduledDate, .data$lastCalculationDate,
                    .data$iterationId),
      by = base::c("id" = "id")
    )

  # merge with competition info
  formations <- formations %>%
    dplyr::left_join(
      dplyr::select(iterations, .data$id, .data$competitionName,
                    .data$competitionId, .data$competitionType, .data$season),
      by = base::c("iterationId" = "id")
      )

  # rename some columns
  formations <- formations %>%
    dplyr::rename(
      matchId = "id",
      dateTime = "scheduledDate"
    )

  # define desired column order
  cols <- base::c(
    "matchId",
    "dateTime",
    "competitionId",
    "competitionName",
    "competitionType",
    "iterationId",
    "season",
    "matchDayIndex",
    "matchDayName",
    "squadId",
    "squadName",
    "gameTime",
    "gameTimeInSec",
    "formation"
  )

  # reorder data
  formations <- formations %>%
    dplyr::select(dplyr::all_of(cols))

  # reorder rows
  formations <- formations %>%
    dplyr::arrange("matchId", "squadId")

  return(formations)
}
