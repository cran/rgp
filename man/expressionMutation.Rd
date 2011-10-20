\name{expressionMutation}
\alias{mutateFunc}
\title{Random mutation of functions and expressions...}
\usage{mutateFunc(func, funcset, mutatefuncprob=0.1, breedingFitness=function(individual)
    TRUE, breedingTries=50)
mutateSubtree(func, funcset, inset, conset, mutatesubtreeprob=0.1, maxsubtreedepth=5,
    breedingFitness=function(individual) TRUE, breedingTries=50)
mutateNumericConst(func, mutateconstprob=0.1, breedingFitness=function(individual) TRUE,
    breedingTries=50)
mutateFuncTyped(func, funcset, mutatefuncprob=0.1, breedingFitness=function(individual)
    TRUE, breedingTries=50)
mutateSubtreeTyped(func, funcset, inset, conset, mutatesubtreeprob=0.1, maxsubtreedepth=5,
    breedingFitness=function(individual) TRUE, breedingTries=50)
mutateNumericConstTyped(func, mutateconstprob=0.1, breedingFitness=function(individual) TRUE,
    breedingTries=50)
mutateChangeLabel(func, funcset, inset, conset, strength=1,
    breedingFitness=function(individual) TRUE, breedingTries=50)
mutateInsertSubtree(func, funcset, inset, conset, strength=1, subtreeDepth=2,
    breedingFitness=function(individual) TRUE, breedingTries=50)
mutateDeleteSubtree(func, funcset, inset, conset, strength=1, subtreeDepth=2,
    constprob=0.2, breedingFitness=function(individual) TRUE,
    breedingTries=50)
mutateChangeDeleteInsert(func, funcset, inset, conset, strength=1, subtreeDepth=2,
    constprob=0.2, iterations=1, changeProbability=1/3,
    deleteProbability=1/3, insertProbability=1/3,
    breedingFitness=function(individual) TRUE, breedingTries=50)
mutateDeleteInsert(func, funcset, inset, conset, strength=1, subtreeDepth=2,
    constprob=0.2, iterations=1, deleteProbability=0.5,
    insertProbability=0.5, breedingFitness=function(individual) TRUE,
    breedingTries=50)
}
\description{Random mutation of functions and expressions}
\details{\code{mutateFunc}: RGP implements two sets of mutation operators. The first set is inspired by classical
GP systems. Mutation strength is controlled by giving mutation probabilities:
\code{mutateFunc} mutates a function \eqn{f} by recursively replacing inner function labels in
\eqn{f} with probability \code{mutatefuncprob}.
\code{mutateSubtree} mutates a function by recursively replacing inner nodes with
newly grown subtrees of maximum depth \code{maxsubtreedepth}.
\code{mutateNumericConst} mutates a function by perturbing each numeric (double) constant \eqn{c}
with probability \code{mutateconstprob} by setting \eqn{c := c + rnorm(1)}. Note that constants
of other typed than \code{double} (e.g \code{integer}s) are not affected.
\code{mutateFuncTyped}, \code{mutateSubtreeTyped}, and \code{mutateNumericConstTyped} are
variants of the above functions that only create well-typed result expressions.

The second set of mutation operators features a more orthogonal design, with each individual
operator having a only a small effect on the genotype. Mutation strength is controlled by
the integral \code{strength} parameter.
\code{mutateChangeLabel} Selects a node (inner node or leaf) by uniform random sampling and replaces
the label of this node by a new label of matching type.
\code{mutateInsertSubtree} Selects a leaf by uniform random sampling and replaces it with a matching
subtree of the exact depth of \code{subtreeDepth}.
\code{mutateDeleteSubtree} Selects a subree of the exact depth of \code{subtreeDepth} by uniform random
sampling and replaces it with a matching leaf.
\code{mutateChangeDeleteInsert} Either applies \code{mutateChangeLabel}, \code{mutateDeleteSubtree},
or \code{mutateDeleteSubtree}. The probability weights for selecting an operator can be supplied
via the ...Probability arguments (probability weights are normalized to a sum of 1). 
\code{mutateDeleteInsert} Either applies \code{mutateDeleteSubtree} or \code{mutateDeleteSubtree}. The
probability weights for selecting an operator can be supplied via the ...Probability arguments
(probability weights are normalized to a sum of 1).
The above functions automatically create well-typed result expressions when used in a strongly
typed GP run.

All RGP mutation operators have the S3 class \code{c("mutationOperator", "function")}.

}
\value{\code{mutateFunc}: The randomly mutated function.

}
\arguments{\item{func}{The function to mutate randomly.}
\item{funcset}{The function set.}
\item{inset}{The set of input variables.}
\item{conset}{The set of constant factories.}
\item{mutatefuncprob}{The probability of trying to replace an inner function at each node.}
\item{mutatesubtreeprob}{The probability of replacing a subtree with a newly grown subtree
at each node.}
\item{maxsubtreedepth}{The maximum depth of newly grown subtrees.}
\item{mutateconstprob}{The probability of mutating a constant by adding \code{rnorm(1)} to it.}
\item{strength}{The number of individual point mutations (changes, insertions, deletions) to
perform.}
\item{subtreeDepth}{The depth of the subtrees to insert or delete.}
\item{constprob}{The probability of creating a constant versus an input variable when inserting
a new subtree.}
\item{iterations}{The number of times to apply a mutation operator to a GP individual. This
can be used as a generic way of controling the strength of the genotypic effect of mutation.}
\item{changeProbability}{The probability for selecting the \code{mutateChangeLabel} operator.}
\item{deleteProbability}{The probability for selecting the \code{mutateDeleteSubtree} operator.}
\item{insertProbability}{The probability for selecting the \code{mutateInsertSubtree} operator.}
\item{breedingFitness}{A breeding function. See the documentation for
\code{\link{geneticProgramming}} for details.}
\item{breedingTries}{The number of breeding steps.}
}
\alias{mutateSubtree}
\alias{mutateNumericConst}
\alias{mutateFuncTyped}
\alias{mutateSubtreeTyped}
\alias{mutateNumericConstTyped}
\alias{mutateChangeLabel}
\alias{mutateInsertSubtree}
\alias{mutateDeleteSubtree}
\alias{mutateChangeDeleteInsert}
\alias{mutateDeleteInsert}

