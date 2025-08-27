#' Return a dataframe that contains squad level scores and ratios for a given match ID
#'
#' @param matches 'IMPECT' match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the scores and rations aggregated per squad for the given match ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(squad_match_scores <- getSquadMatchScores(
#'   matches = c(0, 1),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' squad_match_scores <- getSquadMatchScores(
#'   matches = c(84248, 158150),
#'   token = "yourToken"
#' )
#' }
getSquadMatchScores <- function (matches, token) {

  # check if match input is a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # get match info from API
  matchInfo <-
    purrr::map_df(
      matches,
      ~ jsonlite::fromJSON(
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
    ) %>%
    dplyr::select(.data$id, .data$iterationId, .data$lastCalculationDate) %>%
    base::unique()

  # filter for fail matches
  fail_matches <- matchInfo %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == TRUE) %>%
    dplyr::pull(.data$id)

  # filter for avilable matches
  matches <- matchInfo %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == FALSE) %>%
    dplyr::pull(.data$id)

  # raise warnings
  if (base::length(fail_matches) > 0) {
    if (base::length(matches) == 0) {
      base::stop("All supplied matches are unavailable. Execution stopped.")
    }
    else {
      base::warning(
        sprintf(
          "The following matches are not available yet and were ignored:\n\t%s",
          paste(fail_matches, collapse = ", ")
        )
      )
    }
  }

  # get squad match scores from API
  scores_raw <-
    purrr::map(
      matches,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = "https://api.impect.com/v5/customerapi/matches/",
            id = .,
            suffix = "/squad-scores",
            token = token
          ),
          "text",
          encoding = "UTF-8"
        )
      )$data
    )

  # get unique iterationIds
  iterations <- matchInfo %>%
    dplyr::pull(.data$iterationId) %>%
    base::unique()

  # get squads master data from API
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
    dplyr::select(.data$id, .data$name, .data$idMappings) %>%
    base::unique()

  # clean data
  squads <- .cleanData(squads)

  # get score names from API
  scores_list <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/squad-scores",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::select(.data$id, .data$name)

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations, ~ getMatches(iteration = ., token = token))

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate matchsums

  # define function to extract match scores
  extract_scores <- function(dict, side) {
    # convert side data to df
    temp <-
      base::data.frame(dict[[side]]) %>%
      dplyr::rename(
        squadId = .data$id,
        squadScoreId = .data$squadScores.squadScoreId,
        value = .data$squadScores.value
        )

    # unnest scores
    temp <- temp %>%
      # join with scores to ensure all squadScoreId are present and order by squadScoreId
      dplyr::full_join(scores_list, by = c("squadScoreId" = "id")) %>%
      dplyr::arrange(.data$squadScoreId, .data$squadId) %>%
      # drop squadScoreId column
      dplyr::select(-.data$squadScoreId) %>%
      # pivot data
      tidyr::pivot_wider(
        names_from = .data$name,
        values_from = .data$value,
        values_fill = 0,
        values_fn = base::sum
      ) %>%
      # filter for non NA columns that were created by full join
      dplyr::filter(base::is.na(.data$squadId) == FALSE) %>%
      dplyr::mutate(
        # add matchId
        matchId = dict$matchId)

    return(temp)
  }

  #create emtpty df to store matchsums
  scores <- data.frame()

  # iterate over list elements
  for (i in 1:length(scores_raw)) {
    temp <- scores_raw[[i]]
    # apply extract_matchsums function to home and away squad
    temp <-
      purrr::map_df(base::c("squadHome", "squadAway"),
                    ~ extract_scores(temp, .))

    # appénd to target df
    scores <- base::rbind(scores, temp)
  }

  # merge with other data
  scores <- scores %>%
    dplyr::left_join(
      dplyr::select(
        matchplan, .data$id, .data$scheduledDate, .data$matchDayIndex,
        .data$matchDayName, .data$iterationId
      ),
      by = c("matchId" = "id")
    ) %>%
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
        .data$skillCornerId, squadName = .data$name
      ),
      by = c("squadId" = "id")
    ) %>%
    # fix some column names
    dplyr::rename(
      dateTime = .data$scheduledDate
    )

  # define column order
  order <- c(
    "matchId",
    "dateTime",
    "competitionName",
    "competitionId",
    "competitionType",
    "iterationId",
    "season",
    "matchDayIndex",
    "matchDayName",
    "squadId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "squadName",
    scores_list$name
  )

  # select columns
  scores <- scores %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(scores)
}
