\name{formatSeconds}
\alias{formatSeconds}
\title{Format time and data values into human-readable character vectors...}
\usage{formatSeconds(seconds, secondDecimals=2)
}
\description{Format time and data values into human-readable character vectors}
\details{These functions convert date and time values into human-readable character
vectors.
\code{formatSeconds} formats time values given as a numerical vector denoting
seconds into human-readable character vectors, i.e. \code{formatSeconds(70)}
results in the string \code{"1 minute, 10 seconds"}.}
\value{A character vector containg a human-readable representation of the
given date/time.}
\arguments{\item{seconds}{A numeric vector denoting seconds.}
\item{secondDecimals}{The number of decimal places to show for seconds.
Defaults to 2.}
}

