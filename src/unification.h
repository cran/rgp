/* unification.h
 *
 */

#ifndef UNIFICATION_H
#define UNIFICATION_H

#include <R.h>
#include <Rinternals.h>


/* unify_match
 *
 */
SEXP unify_match(SEXP a, SEXP b, SEXP sigma,
                 Rboolean (*const is_variable)(SEXP), Rboolean contains_check);

#endif

