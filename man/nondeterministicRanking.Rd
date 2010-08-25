\name{nondeterministicRanking}
\alias{nondeterministicRanking}
\title{Create a nondeterministic ranking...}
\usage{nondeterministicRanking(l, p=1)
}
\description{Create a nondeterministic ranking}
\details{Create a permutation of the sequence \code{s} = \code{1:l} representing a ranking.
If \code{p} = 1, the ranking will be completely deterministic, i.e. equal to
\code{1:l}. If \code{p} = 0, the ranking will be completely random. If
0 < \code{p} < 1, the places in the ranking will be determined by iterative
weighted sampling without replacement from the sequence \code{s} := \code{1:l}.
At each step of this iterated weighted sampling, the first remaining element of
\code{s} will be selected with probability \code{p}, the second element with
probability \code{p * (1 - p)}, the third element with probability
\code{p * (1 - p) ^ 2}, and so forth.}
\value{A ranking permutation of the values \code{1:l}.}
\arguments{\item{l}{The numer of elements in the ranking.}
\item{p}{The "degree of determinism" of the ranking to create.}
}

