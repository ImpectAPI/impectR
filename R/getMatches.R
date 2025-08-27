#' Return a dataframe with basic information for all matches for a given iteration ID
#'
#' @param iteration 'IMPECT' iteration ID
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return a dataframe containing all matches for a given iteration ID
#'
#' @examples
#' # Toy example: this will error quickly (no API token)
#' try(matchplan <- getMatches(
#'   iteration = 0,
#'   token = "invalid"
#' ))
#'
#' # Real usage: requires valid Bearer Token from `getAccessToken()`
#' \dontrun{
#' matchplan <- getMatches(
#'   iteration = 1004,
#'   token = "yourToken"
#' )
#' }
getMatches <- function(iteration, token) {

  # get matches from API
  matches <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/iterations/",
        id = iteration,
        suffix = "/matches",
        token = token
      ),
      "text",
      encoding = "UTF-8"
      )
    )$data %>%
    jsonlite::flatten()

  # clean data
  matches <- .cleanData(matches)

  # get squads from API
  squads <-jsonlite::fromJSON(
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

  # clean data
  squads <- .cleanData(squads)

  # merge matches with squads
  matches <- matches %>%
    dplyr::left_join(squads,
                     by = c("homeSquadId" = "id"),
                     suffix = c("", "_home")) %>%
    dplyr::rename(
      homeSquadName = .data$name,
      homeSquadType = .data$type,
      homeSquadSkillCornerId = .data$skillCornerId_home,
      homeSquadHeimSpielId = .data$heimSpielId_home,
      homeSquadWyscoutId = .data$wyscoutId_home,
      homeSquadCountryId = .data$countryId
    ) %>%
    dplyr::left_join(squads,
                     by = c("awaySquadId" = "id"),
                     suffix = c("", "_away")) %>%
    dplyr::rename(
      awaySquadName = .data$name,
      awaySquadType = .data$type,
      awaySquadSkillCornerId = .data$skillCornerId_away,
      awaySquadHeimSpielId = .data$heimSpielId_away,
      awaySquadWyscoutId = .data$wyscoutId_away,
      awaySquadCountryId = .data$countryId
    )

  # get countries data from API
  countries <- jsonlite::fromJSON(
    httr::content(
      .callAPIlimited(
        base_url = "https://api.impect.com/v5/customerapi/countries/",
        token = token
        ),
      "text",
      encoding = "UTF-8"
      )
    )$data %>%
    jsonlite::flatten()

  # merge matches with countries
  matches <- matches %>%
    dplyr::left_join(countries, by = c("homeSquadCountryId" = "id")) %>%
    dplyr::rename(homeSquadCountryName = .data$fifaName) %>%
    dplyr::left_join(countries, by = c("awaySquadCountryId" = "id")) %>%
    dplyr::rename(awaySquadCountryName = .data$fifaName)

  # reorder columns
  matches <- matches %>%
    dplyr::select(
      .data$id,
      .data$skillCornerId,
      .data$heimSpielId,
      .data$wyscoutId,
      .data$iterationId,
      .data$matchDayIndex,
      .data$matchDayName,
      .data$homeSquadId,
      .data$homeSquadName,
      .data$homeSquadType,
      .data$homeSquadCountryId,
      .data$homeSquadCountryName,
      .data$homeSquadSkillCornerId,
      .data$homeSquadHeimSpielId,
      .data$homeSquadWyscoutId,
      .data$awaySquadId,
      .data$awaySquadName,
      .data$awaySquadType,
      .data$awaySquadCountryId,
      .data$awaySquadCountryName,
      .data$awaySquadSkillCornerId,
      .data$awaySquadHeimSpielId,
      .data$awaySquadWyscoutId,
      .data$scheduledDate,
      .data$lastCalculationDate,
      .data$available
    )

  # return matches
  return(matches)
}
