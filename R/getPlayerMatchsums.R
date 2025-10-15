#' Return a dataframe that contains all player matchsums for a given match ID
#'
#' @param matches 'IMPECT' match IDs
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the matchsums aggregated per player and position for the given match ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(player_match_sums <- getPlayerMatchsums(
#'   matches = c(0, 1),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' player_match_sums <- getPlayerMatchsums(
#'   matches = c(84248, 158150),
#'   token = "yourToken"
#' )
#' }
getPlayerMatchsums <- function (
    matches,
    token,
    host = "https://api.impect.com"
) {

  # check if match input is a list and convert to list if required
  if (!base::is.list(matches)) {
    if (base::is.numeric(matches) || base::is.character(matches)) {
      matches <- base::c(matches)
    } else {
      stop("Unprocessable type for 'matches' variable")
    }
  }

  # get matchInfo from API
  matchInfo <-
    purrr::map_df(
      matches,
      ~ {
          temp <- jsonlite::fromJSON(
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

          response <- dplyr::tibble(
            id = temp$id,
            dateTime = temp$dateTime,
            iterationId = temp$iterationId,
            lastCalculationDate = temp$lastCalculationDate,
            squadHomeId = temp$squadHome$id,
            squadAwayId = temp$squadAway$id,
            homeCoachId = purrr::pluck(temp, "squadHome", "coachId", .default = NA),
            awayCoachId = purrr::pluck(temp, "squadAway", "coachId", .default = NA),
            formationHome = temp$squadHome$startingFormation,
            formationAway = temp$squadAway$startingFormation
          )
        }
    )

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

  # get player matchsums from API
  matchsums_raw <-
    purrr::map(
      matches,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            host,
            base_url = "/v5/customerapi/matches/",
            id = .,
            suffix = "/player-kpis",
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
    dplyr::select(
      .data$id, playerName = .data$commonname, .data$firstname,
      .data$lastname, .data$birthdate, .data$birthplace, .data$leg,
      .data$idMappings
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

  # get coach master data from API
  coaches <-
    purrr::map_df(
      iterations,
      ~ {
          response <- jsonlite::fromJSON(
          httr::content(
            .callAPIlimited(
              host,
              base_url = "/v5/customerapi/iterations/",
              id = .,
              suffix = "/coaches",
              token = token
            ),
            "text",
            encoding = "UTF-8"
          )
        )$data

        if (base::length(response) > 0) {
          response <- response %>%
            jsonlite::flatten()
        } else {
          response <- base::data.frame(
            id = -1,
            name = "",
            stringsAsFactors = FALSE
          )
        }
      }
    ) %>%
    dplyr::select(.data$id, .data$name) %>%
    base::unique()


  # get kpi names from API
  kpis <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/kpis",
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
    purrr::map_df(iterations, ~ getMatches(iteration = ., token = token, host = host))

  # get competitions
  iterations <- getIterations(token = token, host = host)

  # manipulate matchsums

  extract_matchsums <- function(dict, side) {
    # convert side data to df
    temp <-
      jsonlite::flatten(jsonlite::fromJSON(
        jsonlite::toJSON(dict[side][[1]]$players),
        simplifyDataFrame = TRUE
      ),
      recursive = FALSE)

    # unnest scorings
    temp <- temp %>%
      tidyr::unnest("kpis", keep_empty = TRUE) %>%
      dplyr::select(
        "playerId" = "id",
        "position",
        "kpiId",
        "value",
        "matchShare",
        "playDuration") %>%
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
      dplyr::filter(base::is.na(.data$playerId) == FALSE) %>%
      # remove the "NA" column if it exists
      dplyr::select(-dplyr::matches("^NA$")) %>%
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
      dplyr::select(squads, .data$id, squadName = .data$name),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(players, .data$id, .data$playerName, .data$firstname,
                    .data$lastname, .data$birthdate, .data$birthplace,
                    .data$leg, .data$wyscoutId, .data$heimSpielId,
                    .data$skillCornerId),
      by = c("playerId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::bind_rows(
        dplyr::select(
          matchInfo,
          matchId = .data$id,
          squadId = .data$squadHomeId,
          coachId = .data$homeCoachId
        ),
        dplyr::select(
          matchInfo,
          matchId = .data$id,
          squadId = .data$squadAwayId,
          coachId = .data$awayCoachId
        )
      ),
      by = base::c("matchId" = "matchId", "squadId" = "squadId")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        coaches,
        coachId = .data$id,
        coachName = .data$name
      ),
      by = base::c("coachId" = "coachId")
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
    "squadName",
    "coachId",
    "coachName",
    "playerId",
    "wyscoutId",
    "heimSpielId",
    "skillCornerId",
    "playerName",
    "firstname",
    "lastname",
    "birthdate",
    "birthplace",
    "leg",
    "position",
    "matchShare",
    "playDuration",
    kpis$name
  )

  # select columns
  matchsums <- matchsums %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(matchsums)
}
