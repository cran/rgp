\name{sortingAlgorithms}
\alias{insertionSort}
\title{Sorting algorithms for vectors and lists...}
\usage{insertionSort(xs, orderRelation)
}
\description{Sorting algorithms for vectors and lists}
\details{These algorithms sort a list or vector by a given order relation (which
defaults to \code{<=}).
\code{insertionSort} is a stable O(n^2) sorting algorithm that is quite efficient
for very small sets (less than around 20 elements). Use an O(n*log(n)) algorithm
for larger sets.}
\value{The vector or list \code{xs} sorted by the order relation \code{orderRelation}.}
\arguments{\item{xs}{The vector or list to sort.}
\item{orderRelation}{The orderRelation to sort \code{xs} by (defaults to \code{`<=`}).
This relation by should reflexive, antisymetric, and transitive.}
}

