\name{evolutionStopConditions}
\alias{makeStepsStopCondition}
\title{Evolution stop conditions...}
\usage{makeStepsStopCondition(stepLimit)
makeTimeStopCondition(timeLimit)
`&.stopCondition`(e1, e2)
`|.stopCondition`(e1, e2)
`!.stopCondition`(e1)
}
\description{Evolution stop conditions}
\details{\code{makeStepsStopCondition}: Evolution stop conditions are predicates (functions that return a single logical value)
of the signature \code{function(population, stepNumber, timeElapsed)}. They are used
to decide when to finish a GP evolution run. Stop conditions must be members of the
S3 class \code{c("stopCondition", "function")}. They can be combined using the generic
\emph{and} (\code{|}), \emph{or} (\code{|}) and \emph{not} (\code{!}) functions.

\code{makeStepsStopCondition} creates a stop condition that is fulfilled if the number
of evolution steps exceeds a given limit.
\code{makeTimeStopCondition} creates a stop condition that is fulfilled if the run time
(in seconds) of an evolution run exceeds a given limit.

}
\arguments{\item{stepLimit}{The maximum number of evolution steps for \code{makeStepsStopCondition}.}
\item{timeLimit}{The maximum runtime in seconds for \code{makeTimeStopCondition}.}
\item{e1}{A stop condition.}
\item{e2}{A stop condition.}
}
\alias{makeTimeStopCondition}
\alias{&.stopCondition}
\alias{|.stopCondition}
\alias{!.stopCondition}

