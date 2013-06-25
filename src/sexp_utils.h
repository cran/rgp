/* sexp_utils.h
 *
 */

#ifndef SEXP_UTILS_H
#define SEXP_UTILS_H

#include <R.h>
#include <Rinternals.h>


/* deep_copy_closxp
 *
 */
SEXP deep_copy_closxp(SEXP sexp);

/* map_sexp
 *
 */
SEXP map_sexp(SEXP (*const f)(SEXP), SEXP sexp);

/* map_sexp_leafs
 *
 */
SEXP map_sexp_leafs(SEXP (*const f)(SEXP), SEXP sexp);

/* map_sexp_inner_nodes
 *
 */
SEXP map_sexp_inner_nodes(SEXP (*const f)(SEXP), SEXP sexp);

/* modify_sexp_shortcut
 *
 */
void modify_sexp_shortcut(void (*const f)(SEXP), SEXP sexp);

/* sexp_size
 *
 */
int sexp_size(SEXP sexp);
SEXP sexp_size_R(SEXP sexp);

/* get_sexp_subtree
 *
 */
SEXP get_sexp_subtree(SEXP sexp, int index);
SEXP get_sexp_subtree_R(SEXP sexp, SEXP index);

/* replace_sexp_subtree
 *
 */
SEXP replace_sexp_subtree(SEXP sexp, int index, SEXP replacement);
SEXP replace_sexp_subtree_R(SEXP sexp, SEXP index, SEXP replacement);

/* make_closure
 *
 */
SEXP make_closure(SEXP body, SEXP formal_parameter_list, SEXP envir);

#endif

