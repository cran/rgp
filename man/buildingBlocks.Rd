\name{buildingBlocks}
\alias{buildingBlock}
\title{Support for GP buidling blocks...}
\usage{buildingBlock(expr, hardness=1)
buildingBlockq(expr, hardness=1)
}
\description{Support for GP buidling blocks}
\details{\code{buildingBlock}: Building blocks are a means for protecting expression subtrees
from modification through variation operators.
Often, certain functional units, represented as expression
subtrees in GP individuals, should stay intact during evolutionary
search. Building blocks at the leafs of expressions can be introduced
by adding them to the input variable set. Support for building blocks
is planned for a future release of RGP.

\code{buildingBlock} transforms an R expression to a building block
to be used as an element of the input variable (or function) set. The
parameter \code{hardness} (a numerical value in the interval [0.0 , 1.0])
determines the protection strength against variation inside the building
building block. When \code{hardness} is set to \code{1.0} (the default),
the building block will never be subject to variaton through mutation
or crossover.
\code{buildingBlockq} is equivaltent to \code{buildingBlock}, but
quotes it's argument \code{expr} first.

}
\value{\code{buildingBlock}: A building block.

}
\arguments{\item{expr}{The expresion to transform to a building block.}
\item{hardness}{The strength of the protection against varition inside
the building block. Must be a numeric in the interval [0.0, 1.0].
A \code{hardness} of \code{1.0} (the default) means that the building
block will never be subject to variation.}
}
\alias{buildingBlockq}

