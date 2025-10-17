#' Return a dataframe that contains squad level matchsums for a given match ID
#'
#' @param matches 'IMPECT' match IDs
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the matchsums aggregated per squad for the given match ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(squad_match_sums <- getSquadMatchsums(
#'   matches = c(0, 1),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' squad_match_sums <- getSquadMatchsums(
#'   matches = c(84248, 158150),
#'   token = "yourToken"
#' )
#' }
getSquadMatchsums <- function (
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

  # get squad matchsums from API
  matchsums_raw <-
    purrr::map(
      matches,
      ~ jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            host,
            base_url = "/v5/customerapi/matches/",
            id = .,
            suffix = "/squad-kpis",
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
    dplyr::select(.data$id, .data$name, .data$idMappings) %>%
    base::unique()

  # clean data
  squads <- .cleanData(squads)

  # get coach master data from API
  coaches_blacklisted = FALSE
  coaches <-
    purrr::map_df(
      iterations,
      ~ {
        response <- .callAPIlimited(
          host,
          base_url = "/v5/customerapi/iterations/",
          id = .,
          suffix = "/coaches",
          token = token,
          ignore_403 = TRUE
        )

        # check status
        status <- httr::status_code(response)

        if (status == 403) {
          coaches_blacklisted <<- TRUE

          # insert empty df as response
          response <- base::data.frame(
            id = -1,
            name = "",
            stringsAsFactors = FALSE
          )
        } else {
          response <- jsonlite::fromJSON(
            httr::content(response, "text", encoding = "UTF-8")
          )$data

          # flatten response
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
    purrr::map_df(iterations, ~ getMatches(
      iteration = .,
      token = token,
      host = host
    ))

  # get competitions
  iterations <- getIterations(token = token, host = host)

  # manipulate matchsums

  # define function to extract matchsums
  extract_matchsums <- function(dict, side) {
    # convert side data to df
    temp <-
      base::data.frame(dict[[side]]) %>%
      dplyr::rename(
        squadId = .data$id,
        kpiId = .data$kpis.kpiId,
        value = .data$kpis.value
      )

    # unnest scorings
    temp <- temp %>%
      # join with kpis to ensure all kpiIds are present and order by kpiId
      dplyr::full_join(kpis, by = c("kpiId" = "id")) %>%
      dplyr::arrange(.data$kpiId, .data$squadId) %>%
      # drop kpiId column
      dplyr::select(-.data$kpiId) %>%
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
        squads, squadId = .data$id, .data$wyscoutId, .data$heimSpielId,
        .data$skillCornerId, squadName = .data$name
      ),
      by = c("squadId" = "squadId")
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
    # fix some column names
    dplyr::rename(
      dateTime = .data$scheduledDate,
    )

  # merge events with coaches
  if (coaches_blacklisted == FALSE) {
    matchsums <- matchsums %>%
      dplyr::left_join(
        dplyr::select(
          coaches,
          coachId = .data$id,
          coachName = .data$name
        ),
        by = base::c("coachId" = "coachId")
      )
  }

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
    "coachId",
    "coachName",
    kpis$name
  )

  # check if coaches are blacklisted
  if (coaches_blacklisted) {
    order <- order[!order %in% c("coachId", "coachName")]
  }

  # select columns
  matchsums <- matchsums %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(matchsums)
}
