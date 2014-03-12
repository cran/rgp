\name{makeHierarchicalClusterFunction}
\alias{makeHierarchicalClusterFunction}
\title{Clustering Populations for Niching}
\usage{
makeHierarchicalClusterFunction(distanceMeasure = NULL, minNicheSize = 1)
}
\arguments{
  \item{distanceMeasure}{A distance measure, used for
  calculating distances between individuals in a
  population.}

  \item{minNicheSize}{The minimum number of individuals in
  each niche.}
}
\value{
A \code{clusterFunction} for clustering populations.
}
\description{
These functions create \code{clusterFunction}s for
\code{\link{multiNicheGeneticProgramming}} and
\code{\link{multiNicheSymbolicRegression}}.
\code{makeHierarchicalClusterFunction} returns a clustering
function that uses Ward's agglomerative hierarchical
clustering algorithm \code{\link{hclust}}.
}
\seealso{
\code{\link{multiNicheGeneticProgramming}},
\code{\link{multiNicheSymbolicRegression}}
}

