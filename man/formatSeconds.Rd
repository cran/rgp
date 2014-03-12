\name{formatSeconds}
\alias{formatSeconds}
\title{Format time and data values into human-readable character vectors}
\usage{
formatSeconds(seconds, secondDecimals = 2)
}
\arguments{
  \item{seconds}{A numeric vector denoting seconds.}

  \item{secondDecimals}{The number of decimal places to
  show for seconds.  Defaults to 2.}
}
\value{
A character vector containg a human-readable representation
of the given date/time.
}
\description{
These functions convert date and time values into
human-readable character vectors. \code{formatSeconds}
formats time values given as a numerical vector denoting
seconds into human-readable character vectors, i.e.
\code{formatSeconds(70)} results in the string \code{"1
minute, 10 seconds"}.
}

