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
#'   matchplan <- getMatchplan(518, token)
#' })
#' }
getMatchplan <- function(iteration, token) {
  # get matches
  matches <- .matches(iteration = iteration, token = token)

  # clean data
  matches <- .cleanData(matches)

  # get squads
  squads <- .squadNames(iteration = iteration, token = token)

  # clean data
  squads <- .cleanData(squads)

  # merge matchplan with squads
  matches <- matches %>%
    dplyr::left_join(squads,
                     by = c("homeSquadId" = "id"),
                     suffix = c("", "_home")) %>%
    dplyr::rename(
      homeSquadName = name,
      homeType = type,
      homeSquadSkillCornerId = skillCornerId_home,
      homeSquadHeimSpielId = heimSpielId_home,
      homeSquadCountryId = countryId
    ) %>%
    dplyr::left_join(squads,
                     by = c("awaySquadId" = "id"),
                     suffix = c("", "_away")) %>%
    dplyr::rename(
      awaySquadName = name,
      awayType = type,
      awaySquadSkillCornerId = skillCornerId_away,
      awaySquadHeimSpielId = heimSpielId_away,
      awaySquadCountryId = countryId
    )

  # get countries data
  countries <- .countryNames(token)

  # merge matchplan with countries
  matches <- matches %>%
    dplyr::left_join(countries, by = c("homeSquadCountryId" = "id")) %>%
    dplyr::rename(homeSquadCountryName = name) %>%
    dplyr::left_join(countries, by = c("awaySquadCountryId" = "id")) %>%
    dplyr::rename(awaySquadCountryName = name)

  # reorder columns
  matches <- matches %>%
    dplyr::select(
      iterationId,
      matchId = id,
      skillCornerId,
      heimSpielId,
      matchDayIndex,
      matchDayName,
      homeSquadId,
      homeSquadName,
      homeType,
      homeSquadCountryId,
      homeSquadCountryName,
      homeSquadSkillCornerId,
      homeSquadHeimSpielId,
      awaySquadId,
      awaySquadName,
      awayType,
      awaySquadCountryId,
      awaySquadCountryName,
      awaySquadSkillCornerId,
      awaySquadHeimSpielId,
      scheduledDate,
      lastCalculationDate,
      available
    )

  # return matches
  return(matches)
}
