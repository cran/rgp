\name{orderByParetoMeasure}
\alias{orderByParetoMeasure}
\title{Rearrange points via an arbitrary Pareto-based ranking...}
\usage{orderByParetoMeasure(values, measure=crowding_distance)
}
\description{Rearrange points via an arbitrary Pareto-based ranking}
\details{Returns a permutation that rearranges points, given as columns in a value matrix, via
Pareto-based ranking. Points are ranked by their Pareto front number, ties are broken
by the values of \code{measure}.}
\value{A permutation to rearrange \code{values} based on a Pareto based ranking.}
\arguments{\item{values}{The value matrix to return the ordering permutation for. Each column
represents a point, each row a dimension.}
\item{measure}{The measure used for ranking points that lie on the same Pareto front,
defaults to \code{crowding_distance}.}
}

