#' Return a dataframe that contains squad level matchsums for a given match ID
#'
#' @param matches Impect match IDs
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing the matchsums aggregated per squad for the given match ID
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   matchsums <- getSquadMatchsums(84248, token)
#' })
#' }
getSquadMatchsums <- function (matches, token) {
  # check if match input is a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # apply _eventAttributes function to a set of matches
  matchsums_raw <-
    purrr::map(matches, ~ .squadMatchsums(match = ., token = token))

  # apply .matchInfo function to a set of matches
  iterations <-
    purrr::map(matches, ~ .matchInfo(match = ., token = token)$iterationId) %>%
    base::unique()

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

  # define function to extract matchsums
  extract_matchsums <- function(dict, side) {
    # convert side data to df
    temp <-
      base::data.frame(dict[[side]]) %>%
      dplyr::rename(kpiId = kpis.kpiId,
                    value = kpis.value)

    # unnest scorings
    temp <- temp %>%
      # join with kpis to ensure all kpiIds are present and order by kpiId
      dplyr::full_join(kpis, by = c("kpiId" = "id")) %>%
      dplyr::arrange(kpiId, id) %>%
      # drop kpiId column
      dplyr::select(-kpiId) %>%
      # pivot data
      tidyr::pivot_wider(
        names_from = name,
        values_from = value,
        values_fill = 0,
        values_fn = base::sum
      ) %>%
      # filter for non NA columns that were created by full join
      dplyr::filter(base::is.na(id) == FALSE) %>%
      dplyr::mutate(
        # add matchId
        matchId = dict$matchId)

    return(temp)
  }

  #create emtpty df to store matchsums
  matchsums <- data.frame()

  # iterate over list elements
  for (i in 1:length(matchsums_raw)) {
    temp <- matchsums_raw[[i]]
    # apply extract_matchsums function to home and away squad
    temp <-
      purrr::map_df(base::c("squadHome", "squadAway"),
                    ~ extract_matchsums(temp, .))

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
      by = c("id" = "id")
    ) %>%
    # fix some column names
    dplyr::rename(
      dateTime = scheduledDate,
      squadId = id
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
    kpis$name
  )

  # select columns
  matchsums <- matchsums %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(matchsums)
}
