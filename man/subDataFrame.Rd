\name{subDataFrame}
\alias{subDataFrame}
\title{Select a continuous subframe of a data frame...}
\usage{subDataFrame(x, size=1, pos="START")
}
\description{Select a continuous subframe of a data frame}
\details{Return a continuous subframe of the data frame \code{x} containing \code{size} * \code{nrow(x)}
rows from the start, center, or end.}
\value{A subframe of \code{x}.}
\arguments{\item{x}{The data frame to get a subframe from.}
\item{size}{The size ratio of the subframe. Must be between 0 and 1.}
\item{pos}{The position to take the subframe from. Must be \code{"START"}, \code{"CENTER"},
or \code{"END"}.}
}

