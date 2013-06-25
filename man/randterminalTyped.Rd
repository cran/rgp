\name{randterminalTyped}
\alias{randterminalTyped}
\title{Create a random terminal node}
\usage{
  randterminalTyped(typeString, inset, conset, constprob)
}
\arguments{
  \item{typeString}{The string label of the type of the
  random terminal node to create.}

  \item{inset}{The set of input variables.}

  \item{conset}{The set of constant factories.}

  \item{constprob}{The probability of creating a constant
  versus an input variable.}
}
\value{
  A random terminal node, i.e. an input variable or a
  constant.
}
\description{
  Create a random terminal node
}

