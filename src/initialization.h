/* initialization.h
 *
 */

#ifndef INITIALIZATION_H
#define INITIALIZATION_H

#include <R.h>
#include <Rinternals.h>


/* initialize_expression_grow
 *
 */
SEXP initialize_expression_grow(SEXP function_symbol_list,
                                SEXP function_arities,
                                SEXP input_variable_list,
                                double constant_min, double constant_max,
                                double p_subtree, double p_constant,
                                int depth_max);
SEXP initialize_expression_grow_R(SEXP function_symbol_list,
                                  SEXP function_arities,
                                  SEXP input_variable_list,
                                  SEXP constant_min, SEXP constant_max,
                                  SEXP p_subtree, SEXP p_constant,
                                  SEXP depth_max);

#endif

