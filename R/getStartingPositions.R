#' Return a dataframe that contains all starting positions for a set of given
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
#' @return a dataframe containing all starting positions for a set of given
#' match IDs
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(starting_pos <- getStartingPositions(
#'   matches = c(0, 1),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' starting_pos <- getStartingPositions(
#'   matches = c(84248, 158150),
#'   token = "yourToken"
#' )
#' }
getStartingPositions <- function (
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
        matchId = response$id,
        dateTime = response$dateTime,
        lastCalculationDate = response$lastCalculationDate,
        iterationId = response$iterationId,

        squadHomeId = response$squadHome$id,
        squadHomeStartingPositions = list(response$squadHome$startingPositions),
        squadHomePlayers = list(response$squadHome$players),

        squadAwayId = response$squadAway$id,
        squadAwayStartingPositions = list(response$squadAway$startingPositions),
        squadAwayPlayers = list(response$squadAway$players),
      )
    }
  ) %>%
    base::unique()

  # filter for fail matches
  fail_matches <- match_info %>%
    dplyr::select(.data$matchId, .data$lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == TRUE) %>%
    dplyr::pull(.data$matchId)

  # filter for available matches
  matches <- match_info %>%
    dplyr::select(.data$matchId, .data$lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == FALSE) %>%
    dplyr::pull(.data$matchId)

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


  # get player master data from API
  players <-
    purrr::map_df(
      iterations,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            host,
            base_url = "/v5/customerapi/iterations/",
            id = .,
            suffix = "/players",
            token = token
          ),
          "text",
          encoding = "UTF-8"
        )
      )$data
    ) %>%
    base::unique()

  # clean data
  players <- .cleanData(players)

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
      host = host
    ))

  # get iterations
  iterations <- getIterations(token = token, host = host)

  # extract shirt numbers
  shirt_numbers_home <- match_info %>%
    dplyr::select(
      .data$matchId, squadId = .data$squadHomeId,
      squadPlayers = .data$squadHomePlayers
    )

  shirt_numbers_away <- match_info %>%
    dplyr::select(
      .data$matchId, squadId = .data$squadAwayId,
      squadPlayers = .data$squadAwayPlayers
    )

  # combine data frames
  shirt_numbers <- dplyr::bind_rows(shirt_numbers_home, shirt_numbers_away)

  # unnest players column
  shirt_numbers <- shirt_numbers %>%
    tidyr::unnest_longer(.data$squadPlayers)

  # normalize the JSON structure into separate columns
  shirt_numbers <- shirt_numbers %>%
    tidyr::unnest(.data$squadPlayers) %>%
    dplyr::rename("playerId" = "id")

  # extract starting positions
  starting_positions_home <- match_info %>%
    dplyr::select(
      .data$matchId,
      squadId = .data$squadHomeId,
      squadStartingPositions = .data$squadHomeStartingPositions
    )

  starting_positions_away <- match_info %>%
    dplyr::select(
      .data$matchId,
      squadId = .data$squadAwayId,
      squadStartingPositions = .data$squadAwayStartingPositions
    )

  # combine data frames
  starting_positions <- dplyr::bind_rows(
    starting_positions_home, starting_positions_away
    )

  # unnest starting_positions column
  starting_positions <- starting_positions %>%
    tidyr::unnest_longer(.data$squadStartingPositions)

  # normalize the JSON structure into separate columns
  starting_positions <- starting_positions %>%
    tidyr::unnest(.data$squadStartingPositions)

  # start merging dfs

  # merge starting_positions with squads
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      dplyr::select(squads, squadId = .data$id, squadName = .data$name),
      by = base::c("squadId" = "squadId")
    )

  # merge with shirt numbers
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      shirt_numbers,
      by = base::c(
        "playerId" = "playerId", "matchId" = "matchId", "squadId" = "squadId"
        )
    )

  # merge with players
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, playerName = .data$commonname),
      by = base::c("playerId" = "id")
    )

  # merge with matches info
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      dplyr::select(matchplan, .data$id, .data$matchDayIndex, .data$matchDayName,
                    dateTime = .data$scheduledDate, .data$lastCalculationDate,
                    .data$iterationId),
      by = base::c("matchId" = "id")
    )

  # merge with competition info
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      dplyr::select(iterations, .data$id, .data$competitionName,
                    .data$competitionId, .data$competitionType, .data$season),
      by = base::c("iterationId" = "id")
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
    "playerId",
    "playerName",
    "shirtNumber",
    "position",
    "positionSide"
  )

  # reorder data
  starting_positions <- starting_positions %>%
    dplyr::select(dplyr::all_of(cols))

  # reorder rows
  starting_positions <- starting_positions %>%
    dplyr::arrange("matchId", "squadId", "playerId")

  return(starting_positions)
}
