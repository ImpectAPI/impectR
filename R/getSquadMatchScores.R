#' Return a dataframe that contains squad level scores and ratios for a given match ID
#'
#' @param matches Impect match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the scores and rations aggregated per squad for the given match ID
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   matchsums <- getSquadMatchsums(84248, token)
#' })
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

  # apply .matchInfo function to a set of matches
  matchInfo <-
    purrr::map_df(matches, ~ .matchInfo(match = ., token = token)) %>%
    dplyr::select(id, iterationId, lastCalculationDate) %>%
    base::unique()

  # filter for fail matches
  fail_matches <- matchInfo %>%
    dplyr::filter(base::is.na(lastCalculationDate) == TRUE) %>%
    dplyr::pull(id)

  # filter for avilable matches
  matches <- matchInfo %>%
    dplyr::filter(base::is.na(lastCalculationDate) == FALSE) %>%
    dplyr::pull(id)

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

  # apply _eventAttributes function to a set of matches
  scores_raw <-
    purrr::map(matches, ~ .squadMatchScores(match = ., token = token))

  # get unique iterationIds
  iterations <- matchInfo %>%
    dplyr::pull(iterationId) %>%
    base::unique()

  # apply squadNames function to a set of iterations
  squads <-
    purrr::map_df(iterations, ~ .squadNames(iteration = ., token = token)) %>%
    dplyr::select(id, name, idMappings) %>%
    base::unique()

  # clean data
  squads <- .cleanData(squads)

  # get score names
  scores_list <- .squadScores(token = token)

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
        squadId = id,
        squadScoreId = squadScores.squadScoreId,
        value = squadScores.value
        )

    # unnest scores
    temp <- temp %>%
      # join with scores to ensure all squadScoreId are present and order by squadScoreId
      dplyr::full_join(scores_list, by = c("squadScoreId" = "id")) %>%
      dplyr::arrange(squadScoreId, squadId) %>%
      # drop squadScoreId column
      dplyr::select(-squadScoreId) %>%
      # pivot data
      tidyr::pivot_wider(
        names_from = name,
        values_from = value,
        values_fill = 0,
        values_fn = base::sum
      ) %>%
      # filter for non NA columns that were created by full join
      dplyr::filter(base::is.na(squadId) == FALSE) %>%
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
        matchplan, id, scheduledDate, matchDayIndex, matchDayName, iterationId
      ),
      by = c("matchId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        iterations, id, competitionId, competitionName, competitionType, season
      ),
      by = c("iterationId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        squads, id, wyscoutId, heimSpielId, skillCornerId, squadName = name
      ),
      by = c("squadId" = "id")
    ) %>%
    # fix some column names
    dplyr::rename(
      dateTime = scheduledDate
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
