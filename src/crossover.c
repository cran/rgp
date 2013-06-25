/* crossover.c
 *
 */

#include "crossover.h"
#include <Rmath.h>
#include "random_utils.h"
#include "sexp_utils.h"


SEXP crossover_single_point(SEXP sexp_a, SEXP sexp_b) {
  const int sexp_a_size = sexp_size(sexp_a);
  const int sexp_a_point = random_index(sexp_a_size);
  const int sexp_b_size = sexp_size(sexp_b);
  const int sexp_b_point = random_index(sexp_b_size);
  //Rprintf("sexp_a point (#%d of %d), sexp_b point: (#%d of %d)\n", sexp_a_point, sexp_a_size, sexp_b_point, sexp_b_size);
  SEXP child_sexp_a = PROTECT(replace_sexp_subtree(sexp_a, sexp_a_point,
                                                   get_sexp_subtree(sexp_b, sexp_b_point)));
  SEXP child_sexp_b = PROTECT(replace_sexp_subtree(sexp_b, sexp_b_point,
                                                   get_sexp_subtree(sexp_a, sexp_a_point)));
  SEXP result;
  PROTECT(result = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, child_sexp_a);
  SET_VECTOR_ELT(result, 1, child_sexp_b);
  UNPROTECT(3);
  return result;
}

SEXP crossover_single_point_R(SEXP sexp_a, SEXP sexp_b) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = crossover_single_point(sexp_a, sexp_b));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

