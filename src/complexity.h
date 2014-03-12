/* complexity.h
 *
 */

#ifndef COMPLEXITY_H
#define COMPLEXITY_H

#include <R.h>
#include <Rinternals.h>


/* sexp_visitation_length
 *
 */
SEXP sexp_visitation_length(SEXP sexp, int keepIntermediateResults);
SEXP sexp_visitation_length_R(SEXP sexp, SEXP keepIntermediateResults);

/* func_visitation_length
 *
 */
SEXP func_visitation_length_R(SEXP func, SEXP keepIntermediateResults);

#endif

