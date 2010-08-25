/* sexp_utils.h
 *
 */

#ifndef SEXP_UTILS_H
#define SEXP_UTILS_H

#include <R.h>
#include <Rinternals.h>


/* map_sexp
 *
 */
SEXP map_sexp(const SEXP sexp, SEXP (*const f)(SEXP));

/* map_sexp_leafs
 *
 */
SEXP map_sexp_leafs(const SEXP sexp, SEXP (*const f)(SEXP));

/* map_sexp_inner_nodes
 *
 */
SEXP map_sexp_inner_nodes(const SEXP sexp, SEXP (*const f)(SEXP));

// TODO

#endif
