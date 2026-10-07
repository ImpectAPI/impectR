#' Return a dataframe that contains the squad style of play values for the
#' given match IDs
#'
#' @param matches 'IMPECT' match IDs
#' @param token bearer token
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the style of play values per squad for the
#' given match IDs
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(squad_match_style_of_play <- getSquadMatchStyleOfPlay(
#'   matches = c(0, 1),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' squad_match_style_of_play <- getSquadMatchStyleOfPlay(
#'   matches = c(84248, 158150),
#'   token = "yourToken"
#' )
#' }
getSquadMatchStyleOfPlay <- function (
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

  # create vector to store matches that are forbidden (HTTP 403)
  forbidden_matches <- base::c()

  # get matchInfo from API
  matchInfo <-
    purrr::map_df(
      matches,
      ~ {
        response <- .callAPIlimited(
          host,
          base_url = "/v5/customerapi/matches/",
          id = .,
          token = token,
          ignore_403 = TRUE
        )

        # skip forbidden matches
        if (httr::status_code(response) == 403) {
          forbidden_matches <<- base::c(forbidden_matches, .)
          return(NULL)
        }

        temp <- jsonlite::fromJSON(
          httr::content(response, "text", encoding = "UTF-8")
        )$data

        response <- dplyr::tibble(
          id = temp$id,
          dateTime = temp$dateTime,
          iterationId = temp$iterationId,
          lastCalculationDate = temp$lastCalculationDate,
          squadHomeId = temp$squadHome$id,
          squadAwayId = temp$squadAway$id,
          homeCoachId = purrr::pluck(temp, "squadHome", "coachId", .default = NA),
          awayCoachId = purrr::pluck(temp, "squadAway", "coachId", .default = NA)
        )
      }
    )

  # raise exception if all matches are forbidden
  if (base::nrow(matchInfo) == 0) {
    base::stop(
      "All supplied matches are unavailable or forbidden. Execution stopped."
    )
  }

  # filter for fail matches
  fail_matches <- matchInfo %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == TRUE) %>%
    dplyr::pull(.data$id)

  # filter for avilable matches
  matches <- matchInfo %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == FALSE) %>%
    dplyr::pull(.data$id)

  # raise exception if no matches remaining or report removed matches
  if (base::length(matches) == 0) {
    base::stop(
      "All supplied matches are unavailable or forbidden. Execution stopped."
    )
  }
  if (base::length(forbidden_matches) > 0) {
    base::warning(
      sprintf(
        "The following matches are forbidden for the user and were ignored:\n\t%s",
        paste(forbidden_matches, collapse = ", ")
      )
    )
  }
  if (base::length(fail_matches) > 0) {
    base::warning(
      sprintf(
        "The following matches are not available yet and were ignored:\n\t%s",
        paste(fail_matches, collapse = ", ")
      )
    )
  }

  # get squad match style of play from API (forbidden matches return NULL)
  forbidden_styles <- base::c()
  styles_raw <-
    purrr::map(
      matches,
      ~ {
        response <- .callAPIlimited(
          host,
          base_url = "/v5/customerapi/matches/",
          id = .,
          suffix = "/squad-style-of-play",
          token = token,
          ignore_403 = TRUE
        )

        # skip forbidden matches
        if (httr::status_code(response) == 403) {
          forbidden_styles <<- base::c(forbidden_styles, .)
          return(NULL)
        }

        jsonlite::fromJSON(
          httr::content(response, "text", encoding = "UTF-8")
        )$data
      }
    )

  # report matches with forbidden style of play data
  if (base::length(forbidden_styles) > 0) {
    base::warning(
      sprintf(
        "Style of play data is forbidden for the user for the following matches, which were ignored:\n\t%s",
        paste(forbidden_styles, collapse = ", ")
      )
    )
  }

  # get unique iterationIds of available matches
  iterations <- matchInfo %>%
    dplyr::filter(base::is.na(.data$lastCalculationDate) == FALSE) %>%
    dplyr::pull(.data$iterationId) %>%
    base::unique()

  # get squads master data from API
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

  # get style of play definitions from API
  styles_definitions <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        host,
        base_url = "/v5/customerapi/squad-style-of-play",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::pull(.data$styleOfPlayName)

  # get matchplan data
  matchplan <-
    purrr::map_df(iterations, ~ getMatches(
      iteration = .,
      token = token,
      host = host
    ))

  # get competitions
  iterations <- getIterations(token = token, host = host)

  # manipulate style of play

  # define function to extract one row per match and side
  extract_sides <- function(dict, match_id, side) {
    squad_id <- dict[[side]]$id
    if (base::is.null(squad_id) || base::is.na(squad_id)) {
      return(NULL)
    }
    tibble::tibble(matchId = match_id, squadId = squad_id)
  }

  # define function to extract style of play values in long format
  extract_styles <- function(dict, match_id, side) {
    squad_id <- dict[[side]]$id
    side_styles <- dict[[side]]$styleOfPlay
    if (base::is.null(squad_id) || base::is.na(squad_id) ||
        !base::is.data.frame(side_styles) || base::nrow(side_styles) == 0) {
      return(NULL)
    }
    tibble::tibble(
      matchId = match_id,
      squadId = squad_id,
      styleOfPlayName = side_styles$styleOfPlayName,
      value = side_styles$value
    )
  }

  # create one row per match and side, keeping sides without style of play
  # values
  sides <- dplyr::bind_rows(
    tibble::tibble(matchId = base::integer(), squadId = base::integer()),
    purrr::map2_dfr(
      styles_raw,
      matches,
      function(dict, match_id) {
        purrr::map_dfr(
          base::c("squadHome", "squadAway"),
          ~ extract_sides(dict, match_id, .)
        )
      }
    )
  )

  # unnest style of play values into long format
  styles <- dplyr::bind_rows(
    tibble::tibble(
      matchId = base::integer(),
      squadId = base::integer(),
      styleOfPlayName = base::character(),
      value = base::numeric()
    ),
    purrr::map2_dfr(
      styles_raw,
      matches,
      function(dict, match_id) {
        purrr::map_dfr(
          base::c("squadHome", "squadAway"),
          ~ extract_styles(dict, match_id, .)
        )
      }
    )
  )

  # pivot style of play values
  styles <- styles %>%
    tidyr::pivot_wider(
      id_cols = base::c("matchId", "squadId"),
      names_from = "styleOfPlayName",
      values_from = "value",
      values_fn = base::sum
    )

  # ensure all styles are present
  for (style in base::setdiff(styles_definitions, base::names(styles))) {
    styles[[style]] <- NA_real_
  }

  # merge style of play values onto all match sides
  styles <- sides %>%
    dplyr::left_join(
      styles,
      by = base::c("matchId" = "matchId", "squadId" = "squadId")
    )

  # merge with other data
  styles <- styles %>%
    dplyr::left_join(
      dplyr::select(
        matchplan, .data$id, .data$scheduledDate, .data$matchDayIndex,
        .data$matchDayName, .data$iterationId
      ),
      by = c("matchId" = "id")
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
        iterations, .data$id, .data$competitionId, .data$competitionName,
        .data$competitionType, .data$season
      ),
      by = c("iterationId" = "id")
    ) %>%
    dplyr::left_join(
      dplyr::select(
        squads, .data$id, .data$wyscoutId, .data$heimSpielId,
        .data$skillCornerId, .data$optaId, .data$statsPerformId,
        .data$transfermarktId, .data$soccerdonnaId, .data$dflId,
        squadName = .data$name
      ),
      by = c("squadId" = "id")
    ) %>%
    # fix some column names
    dplyr::rename(
      dateTime = .data$scheduledDate
    )

  # merge with coaches
  if (coaches_blacklisted == FALSE) {
    styles <- styles %>%
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
    "optaId",
    "statsPerformId",
    "transfermarktId",
    "soccerdonnaId",
    "dflId",
    "squadName",
    "coachId",
    "coachName"
  )

  # check if coaches are blacklisted
  if (coaches_blacklisted) {
    order <- order[!order %in% c("coachId", "coachName")]
  }

  # add style of play names to order
  order <- c(order, styles_definitions)

  # select columns
  styles <- styles %>%
    dplyr::select(dplyr::all_of(order))

  # return style of play
  return(styles)
}
