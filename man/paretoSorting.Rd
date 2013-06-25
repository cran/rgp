\name{orderByParetoCrowdingDistance}
\alias{orderByParetoCrowdingDistance}
\alias{orderByParetoHypervolumeContribution}
\title{Rearrange points via Pareto-based rankings}
\usage{
  orderByParetoCrowdingDistance(values)

  orderByParetoHypervolumeContribution(values)
}
\arguments{
  \item{values}{The value matrix to return the ordering
  permutation for. Each column represents a point, each row
  a dimension.}
}
\value{
  A permutation to rearrange \code{values} based on a
  Pareto based ranking.
}
\description{
  Returns a permutation that rearranges points, given as
  columns in a value matrix, via Pareto-based ranking.
  Points are ranked by their Pareto front number. In
  \code{orderByParetoCrowdingDistance}, ties are then
  broken by crowding distance, in
  \code{orderByParetoHypervolumeContribution}, ties are
  broken by hypervolume contribution.
}

