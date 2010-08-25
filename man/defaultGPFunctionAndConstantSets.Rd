\name{defaultGPFunctionAndConstantSets}
\alias{arithmeticFunctionSet}
\title{Default function- and constant factory sets for Genetic Programming...}
\description{Default function- and constant factory sets for Genetic Programming}
\details{\code{arithmeticFunctionSet}: \code{arithmeticFunctionSet} is an untyped function set containing the functions
"+", "-", "*", and "/".
\code{expLogFunctionSet} is an untyped function set containing the functions
"sqrt", "exp", and "ln".
\code{trigonometricFunctionSet} is an untyped function set containing the functions
"sin", "cos", and "tan".
\code{mathFunctionSet} is an untyped function set containing all of the above functions.

\code{numericConstantSet} is an untyped constant factory set containing a single
constant factory that creates numeric constants via calls to \code{runif(1, -1, 1)}.

\code{typedArithmeticFuncset} is a typed function set containing the functions
"+", "-", "*", and "/".
\code{typedExpLogFuncset} is a typed function set containing the functions
"sqrt", "exp", and "ln".
\code{typedTrigonometricFuncset} is a typed function set containing the functions
"sin", "cos", and "tan".
\code{typedMathFuncset} is a typed function set containing all of the typed functions above.

\code{typedLogicalFuncset} is a typed function set containing the functions
"<", ">", "==", "ifThenElse", "&", "|", and "!".
\code{typedMathLogicalFuncset} is a typed function set containing all functions of
\code{typedMathFuncset} and \code{typedLogicalFuncset}.

\code{typedHigherOrderVectorFuncset} is a typed function set containing the functions
"sapply" and "mean".

}
\alias{expLogFunctionSet}
\alias{trigonometricFunctionSet}
\alias{mathFunctionSet}
\alias{numericConstantSet}
\alias{typedArithmeticFuncset}
\alias{typedExpLogFuncset}
\alias{typedTrigonometricFuncset}
\alias{typedMathFuncset}
\alias{typedLogicalFuncset}
\alias{typedMathLogicalFuncset}
\alias{typedHigherOrderVectorFuncset}

