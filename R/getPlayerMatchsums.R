#' Return a dataframe that contains all player matchsums for a given match ID
#'
#' @param matches Impect match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the matchsums aggregated per player and position for the given match ID
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   matchsums <- getPlayerMatchsums(84248, token)
#' })
#' }
getPlayerMatchsums <- function (matches, token) {
  # check if match input is a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # apply .eventAttributes function to a set of matches
  matchsums_raw <-
    purrr::map(matches, ~ .playerMatchsums(match = ., token = token))

  # apply .matchInfo function to a set of matches
  iterations <-
    purrr::map(matches, ~ .matchInfo(match = ., token = token)$iterationId) %>%
    base::unique()

  # apply playerNames function to a set of iterations
  players <-
    purrr::map_df(iterations, ~ .playerNames(iteration = ., token = token))

  # apply squadNames function to a set of iterations
  squads <-
    purrr::map_df(iterations, ~ .squadNames(iteration = ., token = token))

  # get kpi names
  kpis <- .kpis(token = token)

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations, ~ getMatches(iteration = ., token = token))

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate matchsums

  extract_matchsums <- function(dict, side) {
    # convert side data to df
    temp <-
      jsonlite::flatten(jsonlite::fromJSON(
        jsonlite::toJSON(dict[side][[1]]$players),
        simplifyDataFrame = TRUE
      ),
      recursive = FALSE)

    # detect non-empty scorings column
    list <- base::sapply(temp$kpis, length) > 0

    # filter and add context data
    temp <- temp[list,]

    # unnest scorings
    temp <- temp %>%
      tidyr::unnest("kpis") %>%
      dplyr::select(
        "playerId" = "id",
        "position",
        "kpiId",
        "value",
        "matchShare") %>%
      # join with kpis to ensure all kpiIds are present and order by kpiId
      dplyr::full_join(kpis, by = c("kpiId" = "id")) %>%
      dplyr::arrange("kpiId", "playerId") %>%
      # drop kpiId column
      dplyr::select(-"kpiId") %>%
      # pivot data
      tidyr::pivot_wider(
        names_from = "name",
        values_from = "value",
        values_fill = 0,
        values_fn = base::sum
      ) %>%
      # filter for non NA columns that were created by full join
      dplyr::filter(base::is.na(playerId) == FALSE) %>%
      dplyr::mutate(
        # add matchId
        matchId = dict$matchId,
        # add squadId
        squadId = dict[[side]]$id)

    return(temp)
  }

  #create emtpty df to store matchsums
  matchsums <- data.frame()

  # iterate over list elements
  for (i in 1:length(matchsums_raw)) {
    temp <- matchsums_raw[[i]]
    # apply extract_matchsums function to home and away squad
    temp <-
      purrr::map_df(
        base::c("squadHome", "squadAway"),
        ~ extract_matchsums(temp, .)
      )

    # appénd to target df
    matchsums <- base::rbind(matchsums, temp)
  }

  # merge with other data
  matchsums <- matchsums %>%
    dplyr::left_join(
      matchplan,
      by = c("matchId" = "id")
    ) %>%
    dplyr::left_join(
      iterations,
      by = c("iterationId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(squads, id, squadName = name),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, id, playerName = commonname),
      by = c("playerId" = "id")
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
    "squadName",
    "playerId",
    "playerName",
    "position",
    "matchShare",
    kpis$name
  )

  # select columns
  matchsums <- matchsums %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(matchsums)
}
