\name{makeRegressionFitnessFunction}
\alias{makeRegressionFitnessFunction}
\title{Create a fitness function for symbolic regression...}
\usage{makeRegressionFitnessFunction(formula, data, errormeasure=rmse, indsizelimit=NA,
    penalizeGenotypeConstantIndividuals=FALSE)
}
\description{Create a fitness function for symbolic regression}
\details{Creates a fitness function that calculates an error measure with
respect to a given set of data variables. A simplified version of
the formula syntax is used to describe the regression task. When
an \code{indsizelimit} is given, individuals exceeding this limit
will receive a fitness of \code{Inf}.}
\value{A fitness function to be used in symbolic regression.}
\arguments{\item{formula}{A formula object describing the regression task.}
\item{data}{An optional data frame containing the variables in the
model.}
\item{errormeasure}{A function to use as an error measure.}
\item{indsizelimit}{Individuals exceeding this size limit will get
a fitness of \code{Inf}.}
\item{penalizeGenotypeConstantIndividuals}{Individuals that do not
contain any input variables will get a fitness of \code{Inf}.}
}

