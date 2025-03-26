#' Return a dataframe that contains all starting positions for a set of given
#' match IDs
#'
#' @param matches IMPECT match ID or a list of match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all starting positions for a set of given
#' match IDs
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   startingPositions <- getStartingPositions(matches = c(84248), token = token)
#' })
#' }
getStartingPositions <- function (
    matches,
    token
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
            base_url = "https://api.impect.com/v5/customerapi/matches/",
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
    dplyr::select(matchId, lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(lastCalculationDate) == TRUE) %>%
    dplyr::pull(matchId)

  # filter for available matches
  matches <- match_info %>%
    dplyr::select(matchId, lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(lastCalculationDate) == FALSE) %>%
    dplyr::pull(matchId)

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
    dplyr::pull(iterationId) %>%
    base::unique()


  # get player master data from API
  players <-
    purrr::map_df(
      iterations,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = "https://api.impect.com/v5/customerapi/iterations/",
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
            base_url = "https://api.impect.com/v5/customerapi/iterations/",
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
    dplyr::select(id, name) %>%
    base::unique()

  # fix column names using regex
  base::names(squads) <-
    gsub("\\.(.)", "\\U\\1", base::names(squads), perl = TRUE)

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations, ~ getMatches(iteration = ., token = token))

  # get iterations
  iterations <- getIterations(token = token)

  # extract shirt numbers
  shirt_numbers_home <- match_info %>%
    dplyr::select(
      matchId, squadId = squadHomeId, squadPlayers = squadHomePlayers
    )

  shirt_numbers_away <- match_info %>%
    dplyr::select(
      matchId, squadId = squadAwayId, squadPlayers = squadAwayPlayers
    )

  # combine data frames
  shirt_numbers <- dplyr::bind_rows(shirt_numbers_home, shirt_numbers_away)

  # unnest players column
  shirt_numbers <- shirt_numbers %>%
    tidyr::unnest_longer(squadPlayers)

  # normalize the JSON structure into separate columns
  shirt_numbers <- shirt_numbers %>%
    tidyr::unnest(squadPlayers) %>%
    dplyr::rename("playerId" = "id")

  # extract starting positions
  starting_positions_home <- match_info %>%
    dplyr::select(
      matchId,
      squadId = squadHomeId,
      squadStartingPositions = squadHomeStartingPositions
    )

  starting_positions_away <- match_info %>%
    dplyr::select(
      matchId,
      squadId = squadAwayId,
      squadStartingPositions = squadAwayStartingPositions
    )

  # combine data frames
  starting_positions <- dplyr::bind_rows(
    starting_positions_home, starting_positions_away
    )

  # unnest starting_positions column
  starting_positions <- starting_positions %>%
    tidyr::unnest_longer(squadStartingPositions)

  # normalize the JSON structure into separate columns
  starting_positions <- starting_positions %>%
    tidyr::unnest(squadStartingPositions)

  # start merging dfs

  # merge starting_positions with squads
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      dplyr::select(squads, squadId = id, squadName = name),
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
      dplyr::select(players, id, playerName = commonname),
      by = base::c("playerId" = "id")
    )

  # merge with matches info
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      dplyr::select(matchplan, id, matchDayIndex, matchDayName,
                    dateTime = scheduledDate,lastCalculationDate, iterationId),
      by = base::c("matchId" = "id")
    )

  # merge with competition info
  starting_positions <- starting_positions %>%
    dplyr::left_join(
      dplyr::select(iterations, id, competitionName, competitionId,
                    competitionType, season),
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
