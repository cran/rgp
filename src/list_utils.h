/* list_utils.h
 *
 */

#ifndef LIST_UTILS_H
#define LIST_UTILS_H

#include <R.h>
#include <Rinternals.h>


/* map_list
 *
 */
SEXP map_list(SEXP (*const f)(SEXP), const SEXP list);

/* make_alist
 *
 */
SEXP make_alist();

/* add_alist
 *
 */
SEXP add_alist(const SEXP key, const SEXP value, const SEXP alist);

/* get_alist
 *
 */
SEXP get_alist(const SEXP key, const SEXP alist);

/* contains_alist
 *
 */
Rboolean contains_alist(const SEXP key, const SEXP alist);

// TODO

#endif
