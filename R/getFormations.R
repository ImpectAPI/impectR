#' Return a dataframe that contains all starting formations for a set of given
#' match IDs
#'
#' @param matches IMPECT match ID or a list of match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all starting formations for a set of given
#' match IDs
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   formations <- getFormations(matches = c(84248), token = token)
#' })
#' }
getFormations <- function (
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
    dplyr::select(id, lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(lastCalculationDate) == TRUE) %>%
    dplyr::pull(id)

  # filter for available matches
  matches <- match_info %>%
    dplyr::select(id, lastCalculationDate) %>%
    base::unique() %>%
    dplyr::filter(base::is.na(lastCalculationDate) == FALSE) %>%
    dplyr::pull(id)

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

  # extract formations
  formations_home <- match_info %>%
    dplyr::select(
      id, squadId = squadHomeId, squadFormations = squadHomeFormations
      )

  formations_away <- match_info %>%
    dplyr::select(
      id, squadId = squadAwayId, squadFormations = squadAwayFormations
      )

  # combine data frames
  formations <- dplyr::bind_rows(formations_home, formations_away)

  # unnest formations column
  formations <- formations %>%
    tidyr::unnest_longer(squadFormations)

  # normalize the JSON structure into separate columns
  formations <- formations %>%
    tidyr::unnest(squadFormations)

  # start merging dfs

  # merge formations with squads
  formations <- formations %>%
    dplyr::left_join(
      dplyr::select(squads, squadId = id, squadName = name),
      by = base::c("squadId" = "squadId")
    )

  # merge with matches info
  formations <- formations %>%
    dplyr::left_join(
      dplyr::select(matchplan, id, skillCornerId, heimSpielId, wyscoutId,
                    matchDayIndex, matchDayName, scheduledDate,
                    lastCalculationDate, iterationId),
      by = base::c("id" = "id")
    )

  # merge with competition info
  formations <- formations %>%
    dplyr::left_join(
      dplyr::select(iterations, id, competitionName, competitionId,
                    competitionType, season),
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
