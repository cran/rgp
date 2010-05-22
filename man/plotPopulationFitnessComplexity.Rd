\name{plotPopulationFitnessComplexity}
\alias{plotPopulationFitnessComplexity}
\title{Fitness/Complexity plot for populations...}
\usage{plotPopulationFitnessComplexity(pop, fitnessFunction, complexityFunction=funcVisitationLength,
    showIndices=TRUE, showParetoFront=TRUE, hideOutliers=0, ...)
}
\description{Fitness/Complexity plot for populations}
\details{Plots the fitness against the complexity of each individual in a population.}
\arguments{\item{pop}{A population to plot.}
\item{fitnessFunction}{The function to calculate an individual's fitness with.}
\item{complexityFunction}{The function to calculate an individual's complexity with.}
\item{showIndices}{Whether to show the population index of each individual.}
\item{showParetoFront}{Whether to highlight the pareto front in the plot.}
\item{hideOutliers}{If \code{N = hideOutliers > 0}, hide outliers from the plot using
a "N * IQR" criterion.}
\item{...}{Additional parameters for the underlying call to \code{\link{plot}}.}
}

