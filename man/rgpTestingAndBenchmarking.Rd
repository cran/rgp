\name{rgpTestingAndBenchmarking}
\alias{rgpBenchmark}
\title{Utility functions for testing and benchmarking the RGP system...}
\usage{rgpBenchmark(fitnessFunction=function(ind) 0, samples=1, time=10, ...)
evaluationsPerSecondBenchmark(f, samples=1, time=10, ...)
}
\description{Utility functions for testing and benchmarking the RGP system}
\details{\code{rgpBenchmark}: \code{rgpBenchmark} measures the number of fitness evaluations per second
performed by \code{\link{geneticProgramming}}. A number of \code{samples}
experiments are performed.

\code{evaluationsPerSecondBenchmark} measures the number of times a function
can be called per second in a tight loop.

}
\value{\code{rgpBenchmark}: The number of fitness evaluations per second performed by RGP.

}
\arguments{\item{f}{The function under test.}
\item{fitnessFunction}{The fitness function to pass to the call to
\code{\link{geneticProgramming}}.}
\item{samples}{The number of indpendent measurements to perform, defaults to 1.}
\item{time}{The time in seconds a sample lasts, defaults to 10 seconds.}
\item{...}{Options as passed to the function under test.}
}
\alias{evaluationsPerSecondBenchmark}

