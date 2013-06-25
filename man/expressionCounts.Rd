\name{exprShapesOfDepth}
\alias{exprShapesOfDepth}
\alias{exprShapesOfMaxDepth}
\alias{exprShapesOfMaxSize}
\alias{exprShapesOfSize}
\alias{exprsOfDepth}
\alias{exprsOfMaxDepth}
\alias{exprsOfMaxSize}
\alias{exprsOfSize}
\title{Upper bounds for expression tree search space sizes}
\usage{
  exprShapesOfDepth(funcset, n)

  exprShapesOfMaxDepth(funcset, n)

  exprsOfDepth(funcset, inset, n)

  exprsOfMaxDepth(funcset, inset, n)

  exprShapesOfSize(funcset, n)

  exprShapesOfMaxSize(funcset, n)

  exprsOfSize(funcset, inset, n)

  exprsOfMaxSize(funcset, inset, n)
}
\arguments{
  \item{funcset}{The function set.}

  \item{inset}{The set of input variables.}

  \item{n}{The fixed size or depth.}
}
\description{
  These functions return the number of structurally
  different expressions or expression shapes of a given
  depth or size that can be build from a fixed function-
  and input-variable set. Here, "expression shape" means
  the shape of an expression tree, not taking any node
  labels into account. \code{exprShapesOfDepth} returns the
  number of structurally different expression shapes of a
  depth exactly equal to \code{n}.
  \code{exprShapesOfMaxDepth} returns the number of
  structurally different expression shapes of a depth less
  or equal to \code{n}. \code{exprsOfDepth} returns the
  number of structurally different expressions of a depth
  exactly equal to \code{n}. Note that constants are
  handled by conceptually substiting them with a fresh
  input variable. \code{exprShapesOfMaxDepth} returns the
  number of structurally different expressions of a depth
  less or equal to \code{n}. Note that constants are
  handled by conceptually substiting them with a fresh
  input variable. \code{exprShapesOfSize},
  \code{exprShapesOfMaxSize}, \code{exprsOfSize},
  \code{exprsOfMaxSize} are equivalents that regard
  expression tree size (number of nodes) instead of
  expression tree depth.
}

