#' Return a dataframe with basic information for all matches for a given iteration ID
#'
#' @param iteration IMPECT iteration ID
#' @param token bearer token
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @return a dataframe containing all matches for a given iteration ID
#'
#'
#' @examples
#' \donttest{
#' try({ # prevent cran errors
#'   matches <- getMatches(518, token)
#' })
#' }
getMatches <- function(iteration, token) {
  # get matches
  matches <- .matches(iteration = iteration, token = token)

  # clean data
  matches <- .cleanData(matches)

  # get squads
  squads <- .squadNames(iteration = iteration, token = token)

  # clean data
  squads <- .cleanData(squads)

  # merge matches with squads
  matches <- matches %>%
    dplyr::left_join(squads,
                     by = c("homeSquadId" = "id"),
                     suffix = c("", "_home")) %>%
    dplyr::rename(
      homeSquadName = name,
      homeSquadType = type,
      homeSquadSkillCornerId = skillCornerId_home,
      homeSquadHeimSpielId = heimSpielId_home,
      homeSquadWyscoutId = wyscoutId_home,
      homeSquadCountryId = countryId
    ) %>%
    dplyr::left_join(squads,
                     by = c("awaySquadId" = "id"),
                     suffix = c("", "_away")) %>%
    dplyr::rename(
      awaySquadName = name,
      awaySquadType = type,
      awaySquadSkillCornerId = skillCornerId_away,
      awaySquadHeimSpielId = heimSpielId_away,
      awaySquadWyscoutId = wyscoutId_away,
      awaySquadCountryId = countryId
    )

  # get countries data
  countries <- .countryNames(token)

  # merge matches with countries
  matches <- matches %>%
    dplyr::left_join(countries, by = c("homeSquadCountryId" = "id")) %>%
    dplyr::rename(homeSquadCountryName = fifaName) %>%
    dplyr::left_join(countries, by = c("awaySquadCountryId" = "id")) %>%
    dplyr::rename(awaySquadCountryName = fifaName)

  # reorder columns
  matches <- matches %>%
    dplyr::select(
      id,
      skillCornerId,
      heimSpielId,
      wyscoutId,
      iterationId,
      matchDayIndex,
      matchDayName,
      homeSquadId,
      homeSquadName,
      homeSquadType,
      homeSquadCountryId,
      homeSquadCountryName,
      homeSquadSkillCornerId,
      homeSquadHeimSpielId,
      homeSquadWyscoutId,
      awaySquadId,
      awaySquadName,
      awaySquadType,
      awaySquadCountryId,
      awaySquadCountryName,
      awaySquadSkillCornerId,
      awaySquadHeimSpielId,
      awaySquadWyscoutId,
      scheduledDate,
      lastCalculationDate,
      available
    )

  # return matches
  return(matches)
}
