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
SEXP map_list(SEXP (*const f)(SEXP), SEXP list);

/* make_alist
 *
 */
SEXP make_alist();

/* add_alist
 *
 */
SEXP add_alist(SEXP key, SEXP value, SEXP alist);

/* get_alist
 *
 */
SEXP get_alist(SEXP key, SEXP alist);

/* contains_alist
 *
 */
Rboolean contains_alist(SEXP key, SEXP alist);

// TODO

#endif

