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


#' Return a dataframe that contains all player scores for a given iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param positions list of position names. Must be one of:   "GOALKEEPER",
#' "LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER", "CENTRAL_DEFENDER",
#' "DEFENSE_MIDFIELD", "CENTRAL_MIDFIELD", "ATTACKING_MIDFIELD", "LEFT_WINGER",
#' "RIGHT_WINGER", "CENTER_FORWARD"
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing the player scores aggregated per player for
#' the given iteration ID and list of positions
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(player_scores <- getPlayerIterationScores(
#'   iteration = 0,
#'   positions = c("INVALID_POSITION_1", "INVALID_POSITION_2"),
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' player_scores <- getPlayerIterationScores(
#'   iteration = 1004,
#'   positions = c("CENTRAL_DEFENDER", "DEFENSE_MIDFIELD"),
#'   token = "yourToken"
#' )
#' }
getPlayerIterationScores <- function (iteration, positions, token) {

  # check if iteration input is a string or integer
  if (!(base::is.numeric(iteration) ||
        base::is.character(iteration))) {
    stop("Unprocessable type for 'iteration' variable")
  }

  # check if the input positions are valid
  invalid_positions <- positions[!positions %in% allowed_positions]
  if (length(invalid_positions) > 0) {
    stop("Invalid position(s): ", paste(invalid_positions, collapse = ", "),
         ".\nChoose one or more of: ", paste(allowed_positions, collapse = ", "))
  }

  # compile position string
  position_string <- paste(positions, collapse = ",")

  # get squads master data from API
  squads <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/squads",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten()

  # get squadIds
  squadIds <- squads %>%
    dplyr::filter(.data$access == TRUE) %>%
    dplyr::pull(.data$id) %>%
    base::unique()

  # get player iteration scores from API
  scores_raw <-
    purrr::map_df(
      squadIds,
      ~ {
        response <- jsonlite::fromJSON(
        httr::content(
          .callAPIlimited(
            base_url = paste0(
              "https://api.impect.com/v5/customerapi/iterations/",
              iteration,
              "/squads/",
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

        if (base::length(response) > 0) {
          response <- response %>%
            dplyr::mutate(squadId = ..1, iterationId = iteration)
        }
      }
    )

  # raise exception if no player played at given positions in matches
  if (base::length(scores_raw) == 0) {
    base::stop(
      base::paste0(
        "No players played at position(s) ",
        position_string,
        " in iteration ",
        iteration,
        "."
      )
    )
  }

  # print matches without players at given position
  error_list <- base::as.character(
    squadIds[!squadIds %in% scores_raw$squadId]
  )
  if (base::length(error_list) > 0) {
    base::message(
      base::sprintf(
        "No players played at position(s) %s for following squads:\n\t%s",
        positions, paste(error_list, collapse = ", ")
      )
    )
  }

  # get player master data from API
  players <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/players",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    base::unique()

  # clean data
  players <- .cleanData(players)

  # get score names from API
  score_list <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/player-scores",
        token = token
      ),
      "text",
      encoding = "UTF-8"
    )
  )$data %>%
    jsonlite::flatten() %>%
    dplyr::select(.data$id, .data$name)

  # get competitions
  iterations <- getIterations(token = token)

  # manipulate averages

  # unnest scorings
  scores <- scores_raw %>%
    tidyr::unnest("playerScores", keep_empty = TRUE) %>%
    dplyr::select(
      .data$iterationId,
      .data$squadId,
      .data$playerId,
      .data$playDuration,
      .data$matchShare,
      .data$playerScoreId,
      .data$value
    ) %>%
    # add column to store positions string
    dplyr::mutate(positions = position_string) %>%
    # join with kpis to ensure all scores are present and order by playerScoreId
    dplyr::full_join(score_list, by = c("playerScoreId" = "id")) %>%
    dplyr::arrange(.data$playerScoreId, .data$playerId) %>%
    # drop playerScoreId column
    dplyr::select(-.data$playerScoreId) %>%
    # pivot data
    tidyr::pivot_wider(
      names_from = .data$name,
      values_from = .data$value,
      values_fill = 0,
      values_fn = base::sum
    ) %>%
    # filter for non NA columns that were created by full join
    dplyr::filter(base::is.na(.data$playerId) == FALSE) %>%
    # remove the "NA" column if it exists
    dplyr::select(-dplyr::matches("^NA$"))

  # merge with other data
  scores <- scores %>%
    dplyr::left_join(dplyr::select(squads, .data$id, squadName = .data$name),
                     by = c("squadId" = "id")) %>%
    dplyr::left_join(
      dplyr::select(
        players,
        .data$id,
        .data$wyscoutId,
        .data$heimSpielId,
        .data$skillCornerId,
        playerName = .data$commonname,
        .data$firstname,
        .data$lastname,
        .data$birthdate,
        .data$birthplace,
        .data$leg
      ),
      by = c("playerId" = "id")) %>%
    dplyr::left_join(dplyr::select(
      iterations, .data$id, .data$competitionName, .data$season),
      by = c("iterationId" = "id")
    )

  # define column order
  order <- c(
    "iterationId",
    "competitionName",
    "season",
    "squadId",
    "squadName",
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

  # select columns
  scores <- scores %>%
    dplyr::select(dplyr::all_of(order))

  # return scores
  return(scores)
}
