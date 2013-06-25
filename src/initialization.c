/* initialization.c
 *
 */

#include "initialization.h"
#include "random_utils.h"
#include <Rmath.h>


SEXP initialize_expression_grow_recursive(int current_depth,
                                          SEXP function_symbol_list,
                                          SEXP function_arities,
                                          SEXP input_variable_list,
                                          double constant_min, double constant_max,
                                          double p_subtree, double p_constant,
                                          int depth_max) {
  if (unif_rand() < p_subtree && current_depth < depth_max) { // create inner node (subtree)
    const int function_index = random_index(length(function_symbol_list));
    const int function_arity = INTEGER(function_arities)[function_index];
    SEXP e;
    PROTECT(e = R_NilValue);
    for (int i = 0; i < function_arity; i++) { // recursively initialize actual parameters...
      SEXP new_parameter;
      PROTECT(new_parameter = initialize_expression_grow_recursive(current_depth + 1,
                                                                   function_symbol_list, function_arities,
                                                                   input_variable_list, constant_min, constant_max,
                                                                   p_subtree, p_constant,
                                                                   depth_max));
      PROTECT(e = CONS(new_parameter, e));
    }
    PROTECT(e = LCONS(install(CHAR(STRING_ELT(VECTOR_ELT(function_symbol_list, function_index), 0))), e));
    UNPROTECT(2 * function_arity + 2);
    return e;
  } else if (unif_rand() < p_constant) { // create constant terminal node
    SEXP c;
    PROTECT(c = allocVector(REALSXP, 1));
    REAL(c)[0] = runif(constant_min, constant_max);
    UNPROTECT(1);
    return c;
  } else { // create input variable terminal node
    SEXP v;
    PROTECT(v = install(CHAR(STRING_ELT(VECTOR_ELT(input_variable_list,
                                                   random_index(length(input_variable_list))), 0))));
    UNPROTECT(1);
    return v;
  }
}

SEXP initialize_expression_grow(SEXP function_symbol_list,
                                SEXP function_arities,
                                SEXP input_variable_list,
                                double constant_min, double constant_max,
                                double p_subtree, double p_constant,
                                int depth_max) {
  return initialize_expression_grow_recursive(0, function_symbol_list, function_arities,
                                              input_variable_list, constant_min, constant_max,
                                              p_subtree, p_constant,
                                              depth_max);
}

SEXP initialize_expression_grow_R(SEXP function_symbol_list,
                                  SEXP function_arities,
                                  SEXP input_variable_list,
                                  SEXP constant_min, SEXP constant_max,
                                  SEXP p_subtree, SEXP p_constant,
                                  SEXP depth_max) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = initialize_expression_grow(function_symbol_list, function_arities,
                                              input_variable_list,
                                              REAL(constant_min)[0], REAL(constant_max)[0],
                                              REAL(p_subtree)[0], REAL(p_constant)[0],
                                              INTEGER(depth_max)[0]));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

