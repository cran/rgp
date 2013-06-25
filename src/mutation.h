/* mutation.h
 *
 */

#ifndef MUTATION_H
#define MUTATION_H

#include <R.h>
#include <Rinternals.h>


/* mutate_constants_normal
 *
 */
SEXP mutate_constants_normal(SEXP sexp, double p, double mu, double sigma);
SEXP mutate_constants_normal_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma);

/* mutate_subtrees
 *
 */
SEXP mutate_subtrees(SEXP sexp,
                     double p_insert, double p_delete,
                     SEXP function_symbol_list,
                     SEXP function_arities,
                     SEXP input_variable_list,
                     double constant_min, double constant_max,
                     double p_subtree, double p_constant,
                     int depth_max);
SEXP mutate_subtrees_R(SEXP sexp,
                       SEXP p_insert, SEXP p_delete,
                       SEXP function_symbol_list,
                       SEXP function_arities,
                       SEXP input_variable_list,
                       SEXP constant_min, SEXP constant_max,
                       SEXP p_subtree, SEXP p_constant,
                       SEXP depth_max); 

/* mutate_functions
 *
 */
SEXP mutate_functions(SEXP sexp, double p, SEXP function_symbol_list, SEXP function_arities);
SEXP mutate_functions_R(SEXP sexp, SEXP p, SEXP function_symbol_list, SEXP function_arities);

#endif

