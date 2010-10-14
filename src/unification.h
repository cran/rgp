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
SEXP unify_match(const SEXP a, const SEXP b, const SEXP sigma,
                 Rboolean (*const is_variable)(SEXP), const Rboolean contains_check);

#endif
