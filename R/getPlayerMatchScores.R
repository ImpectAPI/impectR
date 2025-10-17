# Define the allowed positions
allowed_positions <- c(
  "GOALKEEPER",
  "LEFT_WINGBACK_DEFENDER",
  "RIGHT_WINGBACK_DEFENDER",
  "CENTRAL_DEFENDER",
  "DEFENSE_MIDFIELD",
  "CENTRAL_MIDFIELD",
  "ATTACKING_MIDFIELD",
  "LEFT_WINGER",
  "RIGHT_WINGER",
  "CENTER_FORWARD"
)


#' Return a dataframe that contains all player scores for a given match ID
#' and list of positions
#'
#' @param matches 'IMPECT' match IDs
#' @param token bearer token
#' @param positions optional list of position names. Must be one of:   "GOALKEEPER",
#' "LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER", "CENTRAL_DEFENDER",
#' "DEFENSE_MIDFIELD", "CENTRAL_MIDFIELD", "ATTACKING_MIDFIELD", "LEFT_WINGER",
#' "RIGHT_WINGER", "CENTER_FORWARD". If not submitted, function will return all
#' positions individually.
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the scores aggregated per player and
#' position for the given match ID and list of positions
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(player_match_scores <- getPlayerMatchScores(
#'   matches = c(0, 1),
#'   positions = c("INVALID_POSITION_1", "INVALID_POSITION_2"),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' player_match_scores <- getPlayerMatchScores(
#'   matches = c(84248, 158150),
#'   positions = c("CENTRAL_DEFENDER", "DEFENSE_MIDFIELD"),
#'   token = "yourToken"
#' )
#' }
getPlayerMatchScores <- function (
    matches,
    token,
    positions = NULL,
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

  if (!base::is.null(positions)) {
    # check if the input positions are valid
    invalid_positions <- positions[!positions %in% allowed_positions]
    if (length(invalid_positions) > 0) {
      stop("Invalid position(s): ", paste(invalid_positions, collapse = ", "),
           ".\nChoose one or more of: ", paste(allowed_positions, collapse = ", "))
    }

    # compile position string
    position_string <- paste(positions, collapse = ",")
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

        # define functino to convert NULL to NA
        safe_extract <- function(x) if (is.null(x)) NA else x

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
          base::paste(fail_matches, collapse = ", ")
        )
      )
    }
  }

  # get player match scores from API
  if (!base::is.null(positions)) {
    scores_raw <-
      purrr::map(
        matches,
        ~ jsonlite::fromJSON(
          httr::content(
            .callAPIlimited(
              host,
              base_url = base::paste0(
                "/v5/customerapi/matches/",
                .,
                "/positions/",
                position_string,
                "/player-scores"
              ),
              token = token
            ),
            "text",
            encoding = "UTF-8"
          )
        )$data
      )
  } else {
    scores_raw <-
      purrr::map(
        matches,
        ~ jsonlite::fromJSON(
          httr::content(
            .callAPIlimited(
              host,
              base_url = base::paste0(
                "/v5/customerapi/matches/",
                .,
                "/player-scores"
              ),
              token = token
            ),
            "text",
            encoding = "UTF-8"
          )
        )$data
      )
  }

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
  score_list <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/player-scores",
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

  # manipulate scores

  extract_scores <- function(dict, side) {
    # check if side has players listed
    if (base::length(dict[side][[1]]$players) > 0) {

      # convert side data to df
      temp <-
        jsonlite::flatten(jsonlite::fromJSON(
          jsonlite::toJSON(dict[side][[1]]$players),
          simplifyDataFrame = TRUE
        ),
        recursive = FALSE) %>%
        dplyr::mutate(
          matchId = dict$matchId,
          squadId = dict[side][[1]]$id,
        )

      if (!base::is.null(positions)) {
        # unnest scores
        temp <- temp %>%
          tidyr::unnest("playerScores", keep_empty = TRUE) %>%
          dplyr::select(
            "matchId",
            "squadId",
            "playerId" = "id",
            "playerScoreId",
            "value",
            "matchShare",
            "playDuration") %>%
          # add positions
          dplyr::mutate(positions = position_string)
      } else {
        # unnest scores
        temp <- temp %>%
          tidyr::unnest("playerScores", keep_empty = TRUE) %>%
          dplyr::select(
            "matchId",
            "squadId",
            "playerId" = "id",
            "position",
            "playerScoreId",
            "value",
            "matchShare",
            "playDuration")

      }

      temp <- temp %>%
        # join with kpis to ensure all kpiIds are present and order by kpiId
        dplyr::full_join(score_list, by = c("playerScoreId" = "id")) %>%
        dplyr::arrange("playerScoreId", "playerId") %>%
        # drop kpiId column
        dplyr::select(-"playerScoreId") %>%
        # pivot data
        tidyr::pivot_wider(
          names_from = "name",
          values_from = "value",
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
          squadId = dict[[side]]$id
        )

      return(temp)
    }
  }

  #create emtpty df to store match scores
  scores <- data.frame()

  # iterate over list elements
  for (i in 1:length(scores_raw)) {
    temp <- scores_raw[[i]]
    # apply extract_matchsums function to home and away squad
    temp <-
      purrr::map_df(
        base::c("squadHome", "squadAway"),
        ~ extract_scores(temp, .)
      )

    # appénd to target df
    scores <- base::rbind(scores, temp)
  }

  # raise exception if no player played at given positions in matches
  if (base::length(scores) == 0) {
    base::stop(
      base::paste0(
        "No players played at position(s) ",
        position_string,
        " in match(es) ",
        base::paste(matches, collapse = ", "),
        "."
      )
    )
  }

  # print matches without players at given position
  error_list <- base::as.character(
    matches[!matches %in% scores$matchId]
  )
  if (base::length(error_list) > 0) {
    base::message(
      base::sprintf(
        "No players played at position(s) %s for following matches:\n\t%s",
        positions, paste(error_list, collapse = ", ")
      )
    )
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
      dplyr::select(squads, .data$id, squadName = .data$name),
      by = c("squadId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        players, .data$id, .data$wyscoutId, .data$heimSpielId,
        .data$skillCornerId, .data$playerName, .data$firstname, .data$lastname,
        .data$birthdate, .data$birthplace, .data$leg
      ),
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
    # fix some column names
    dplyr::rename(
      dateTime = .data$scheduledDate
    )

  # merge events with coaches
  if (coaches_blacklisted == FALSE) {
    scores <- scores %>%
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
    "positions",
    "matchShare",
    "playDuration",
    score_list$name
  )

  if (base::is.null(positions)) {
    order[order == "positions"] <- "position"
  }

  # check if coaches are blacklisted
  if (coaches_blacklisted) {
    order <- order[!order %in% c("coachId", "coachName")]
  }

  # select columns
  scores <- scores %>%
    dplyr::select(dplyr::all_of(order))

  # return matchsums
  return(scores)
}
