/* mutation.c
 *
 */

#include "mutation.h"
#include <Rmath.h>
#include "initialization.h"
#include "random_utils.h"

#define MAX_FUNCTION_SYMBOLS 64


SEXP random_function_symbol_of_arity(int arity, SEXP function_symbol_list, SEXP function_arities);

SEXP mutate_constants_normal(SEXP sexp, double p, double mu, double sigma) {
  SEXP c;
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case REALSXP:
    if (unif_rand() < p) { // mutate constant with probability p
      PROTECT(c = allocVector(REALSXP, 1));
      REAL(c)[0] = REAL(sexp)[0] + rnorm(mu, sigma);
      UNPROTECT(1);
      return c;
    } else {
      return sexp;
    }
  case LANGSXP: {
    int function_arity = 0;
    SEXP tail_e, e;
    PROTECT(tail_e = R_NilValue);
    for (SEXP iterator = CDR(sexp); !isNull(iterator); iterator = CDR(iterator)) { // recurse on actual parameters
      function_arity++; // determine arity on the fly
      SEXP mutated_parameter;
      PROTECT(mutated_parameter = mutate_constants_normal(CAR(iterator), p, mu, sigma));
      PROTECT(tail_e = CONS(mutated_parameter, tail_e));
    }
    PROTECT(e = LCONS(CAR(sexp), tail_e));
    UNPROTECT(2 * function_arity + 2);
    return e;
  }
  case LISTSXP:
    error("mutate_constants_normal: unexpected LISTSXP");
  default: // base case
    return sexp; // do nothing
  }
}

SEXP mutate_constants_normal_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_constants_normal(sexp, REAL(p)[0], REAL(mu)[0], REAL(sigma)[0]));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

SEXP mutate_subtrees(SEXP sexp,
                     double p, double p_insert_delete,
                     SEXP function_symbol_list,
                     SEXP function_arities,
                     SEXP input_variable_list,
                     double constant_min, double constant_max,
                     double p_subtree, double p_constant,
                     int depth_max) {
  // Rprintf("->\n"); // DEBUG
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP:
    if (unif_rand() < p) { // mutate inner node with probability p
      if (unif_rand() < p_insert_delete) { // replace with new subtree (insert)
        // Rprintf("insert\n"); // DEBUG
        SEXP new_subtree = PROTECT(initialize_expression_grow(function_symbol_list, function_arities,
                                                              input_variable_list,
                                                              constant_min, constant_max,
                                                              p_subtree, p_constant,
                                                              depth_max));
        UNPROTECT(1);
        return new_subtree;
      } else { // replace with new leaf (delete)
        // Rprintf("delete\n"); // DEBUG
        SEXP new_leaf = PROTECT(initialize_expression_grow(function_symbol_list, function_arities,
                                                           input_variable_list,
                                                           constant_min, constant_max,
                                                           p_subtree, p_constant,
                                                           0));
        UNPROTECT(1);
        return new_leaf;
      }
    } else {
      // Rprintf("pass\n"); // DEBUG
      int function_arity = 0;
      SEXP e;
      PROTECT(e = R_NilValue);
      for (SEXP iterator = CDR(sexp); !isNull(iterator); iterator = CDR(iterator)) { // recurse on actual parameters
        function_arity++; // determine arity on the fly
        SEXP mutated_parameter;
        PROTECT(mutated_parameter = mutate_subtrees(CAR(iterator), p, p_insert_delete,
                                                    function_symbol_list, function_arities,
                                                    input_variable_list,
                                                    constant_min, constant_max,
                                                    p_subtree, p_constant,
                                                    depth_max));
        PROTECT(e = CONS(mutated_parameter, e));
      }
      PROTECT(e = LCONS(CAR(sexp), e));
      UNPROTECT(2 * function_arity + 2);
      return e;
    }
  case LISTSXP:
    error("mutate_subtrees: unexpected LISTSXP");
  default: // base case
    // Rprintf("default type %d\n", TYPEOF(sexp)); // DEBUG
    // if (REALSXP == TYPEOF(sexp)) {
    //   Rprintf("real %f\n", REAL(sexp)[0]); // DEBUG
    // }
    // if (SYMSXP == TYPEOF(sexp)) {
    //   Rprintf("symbol %s\n", CHAR(PRINTNAME(sexp))); // DEBUG
    // }
    if (unif_rand() < p) { // mutate leaf with probability p
      if (unif_rand() < p_insert_delete) { // replace with new subtree (insert)
        // Rprintf("insert at default\n"); // DEBUG
        SEXP new_subtree;
        new_subtree = PROTECT(initialize_expression_grow(function_symbol_list, function_arities,
                                                         input_variable_list,
                                                         constant_min, constant_max,
                                                         p_subtree, p_constant,
                                                         depth_max));
        UNPROTECT(1);
        return new_subtree;
      } else { // replace with new leaf (delete)
        // Rprintf("delete at default\n"); // DEBUG
        SEXP new_leaf;
        new_leaf = PROTECT(initialize_expression_grow(function_symbol_list, function_arities,
                                                      input_variable_list,
                                                      constant_min, constant_max,
                                                      p_subtree, p_constant,
                                                      0));
        UNPROTECT(1);
        return new_leaf;
      }
    } else {
      // Rprintf("pass at default\n"); // DEBUG
      return sexp; // do nothing
    }
  }
}

SEXP mutate_subtrees_R(SEXP sexp,
                       SEXP p, SEXP p_insert_delete,
                       SEXP function_symbol_list,
                       SEXP function_arities,
                       SEXP input_variable_list,
                       SEXP constant_min, SEXP constant_max,
                       SEXP p_subtree, SEXP p_constant,
                       SEXP depth_max) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_subtrees(sexp, REAL(p)[0], REAL(p_insert_delete)[0],
                                   function_symbol_list, function_arities,
                                   input_variable_list,
                                   REAL(constant_min)[0], REAL(constant_max)[0],
                                   REAL(p_subtree)[0], REAL(p_constant)[0],
                                   INTEGER(depth_max)[0]));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

SEXP mutate_functions(SEXP sexp, double p, SEXP function_symbol_list, SEXP function_arities) {
  int arity;
  switch (TYPEOF(sexp)) { // switch for speed
  case LANGSXP: {
    SEXP new_function_symbol;
    if (unif_rand() < p) { // mutate function symbol with probability p
      arity = length(sexp) - 1;
      new_function_symbol = install(CHAR(STRING_ELT(random_function_symbol_of_arity(arity, function_symbol_list, function_arities), 0)));
    } else {
      new_function_symbol = CAR(sexp);
    }
    int function_arity = 0;
    SEXP tail_e, e;
    PROTECT(tail_e = R_NilValue);
    for (SEXP iterator = CDR(sexp); !isNull(iterator); iterator = CDR(iterator)) { // recurse on actual parameters
      function_arity++; // determine arity on the fly
      SEXP mutated_parameter;
      PROTECT(mutated_parameter = mutate_functions(CAR(iterator), p, function_symbol_list, function_arities));
      PROTECT(tail_e = CONS(mutated_parameter, tail_e));
    }
    PROTECT(e = LCONS(new_function_symbol, tail_e));
    UNPROTECT(2 * function_arity + 2);
    return e;
  }
  case LISTSXP:
    error("mutate_functions: unexpected LISTSXP");
  default: // base case
    return sexp; // do nothing
  }
}

SEXP mutate_functions_R(SEXP sexp, SEXP p, SEXP function_symbol_list, SEXP function_arities) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_functions(sexp, REAL(p)[0], function_symbol_list, function_arities));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

SEXP random_function_symbol_of_arity(int arity, SEXP function_symbol_list, SEXP function_arities) {
  int matching_function_symbols[MAX_FUNCTION_SYMBOLS];
  int number_of_matching_function_symbols = 0;
  for (int i = 0; i < length(function_arities); i++) {
    if (arity == INTEGER(function_arities)[i]) {
      matching_function_symbols[number_of_matching_function_symbols] = i;
      number_of_matching_function_symbols++;
    }
  }
  if (number_of_matching_function_symbols == 0) {
    error("random_function_symbol_of_arity: no function symbol of arity %d", arity);
  }
  int selected_function_symbol = matching_function_symbols[random_index(number_of_matching_function_symbols)];
  return VECTOR_ELT(function_symbol_list, selected_function_symbol);
}

