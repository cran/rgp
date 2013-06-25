/* random_utils.c
 *
 */

#include "random_utils.h"


int random_index(int max_index) {
  return (int) (unif_rand() * max_index);
}

SEXP random_index_R(SEXP max_index) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = allocVector(INTSXP, 1));
  INTEGER(result)[0] = random_index(INTEGER(max_index)[0]);
  PutRNGstate();
  UNPROTECT(1);
  return result;
}
