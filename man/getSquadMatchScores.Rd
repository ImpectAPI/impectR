% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSquadMatchScores.R
\name{getSquadMatchScores}
\alias{getSquadMatchScores}
\title{Return a dataframe that contains squad level scores and ratios for a given match ID}
\usage{
getSquadMatchScores(matches, token)
}
\arguments{
\item{matches}{Impect match IDs}

\item{token}{bearer token}
}
\value{
a dataframe containing the scores and rations aggregated per squad for the given match ID
}
\description{
Return a dataframe that contains squad level scores and ratios for a given match ID
}
\examples{
\donttest{
try({ # prevent cran errors
  squadMatchScores <- getSquadMatchsums(84248, token)
})
}
}
