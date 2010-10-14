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
SEXP map_sexp(SEXP (*const f)(SEXP), const SEXP sexp);

/* map_sexp_leafs
 *
 */
SEXP map_sexp_leafs(SEXP (*const f)(SEXP), const SEXP sexp);

/* map_sexp_inner_nodes
 *
 */
SEXP map_sexp_inner_nodes(SEXP (*const f)(SEXP), const SEXP sexp);

// TODO

#endif
