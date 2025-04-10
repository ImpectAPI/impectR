% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPlayerIterationScores.R
\name{getPlayerIterationScores}
\alias{getPlayerIterationScores}
\title{Return a dataframe that contains all player scores for a given iteration ID}
\usage{
getPlayerIterationScores(iteration, positions, token)
}
\arguments{
\item{iteration}{Impect iteration ID}

\item{positions}{list of position names. Must be one of:   "GOALKEEPER",
"LEFT_WINGBACK_DEFENDER", "RIGHT_WINGBACK_DEFENDER", "CENTRAL_DEFENDER",
"DEFENSE_MIDFIELD", "CENTRAL_MIDFIELD", "ATTACKING_MIDFIELD", "LEFT_WINGER",
"RIGHT_WINGER", "CENTER_FORWARD"}

\item{token}{bearer token}
}
\value{
a dataframe containing the player scores aggregated per player for
the given iteration ID and list of positions
}
\description{
Return a dataframe that contains all player scores for a given iteration ID
}
\examples{
\donttest{
try({ # prevent cran errors
  scores <- getPlayerIterationScores(518, token)
})
}
}
