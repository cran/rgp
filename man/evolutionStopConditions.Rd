\name{evolutionStopConditions}
\alias{makeStepsStopCondition}
\title{Evolution stop conditions...}
\usage{makeStepsStopCondition(stepLimit)
makeEvaluationsStopCondition(evaluationLimit)
makeFitnessStopCondition(fitnessLimit)
makeTimeStopCondition(timeLimit)
`&.stopCondition`(e1, e2)
`|.stopCondition`(e1, e2)
`!.stopCondition`(e1)
}
\description{Evolution stop conditions}
\details{\code{makeStepsStopCondition}: Evolution stop conditions are predicates (functions that return a single logical value)
of the signature \code{function(population, stepNumber, evaluationNumber, bestFitness,
timeElapsed)}.
They are used to decide when to finish a GP evolution run. Stop conditions must be members
of the S3 class \code{c("stopCondition", "function")}. They can be combined using the
generic \emph{and} (\code{|}), \emph{or} (\code{|}) and \emph{not} (\code{!}) functions.

\code{makeStepsStopCondition} creates a stop condition that is fulfilled if the number
of evolution steps exceeds a given limit.
\code{makeEvaluationsStopCondition} creates a stop condition that is fulfilled if the
number of fitness function evaluations exceeds a given limit.
\code{makeFitnessStopCondition} creates a stop condition that is fulfilled if the
number best fitness seen in an evaluation run undercuts a certain limit.
\code{makeTimeStopCondition} creates a stop condition that is fulfilled if the run time
(in seconds) of an evolution run exceeds a given limit.

}
\arguments{\item{stepLimit}{The maximum number of evolution steps for \code{makeStepsStopCondition}.}
\item{evaluationLimit}{The maximum number of fitness function evaluations for
\code{makeEvaluationsStopCondition}.}
\item{fitnessLimit}{The minimum fitness for \code{makeFitnessStopCondition}.}
\item{timeLimit}{The maximum runtime in seconds for \code{makeTimeStopCondition}.}
\item{e1}{A stop condition.}
\item{e2}{A stop condition.}
}
\alias{makeEvaluationsStopCondition}
\alias{makeFitnessStopCondition}
\alias{makeTimeStopCondition}
\alias{&.stopCondition}
\alias{|.stopCondition}
\alias{!.stopCondition}

