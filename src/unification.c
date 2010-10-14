/* unification.c
 *
 */

#include "unification.h"
#include "list_utils.h"


static SEXP extend_if_possible(const SEXP variable, const SEXP value, const SEXP sigma,
                               Rboolean (*const is_variable)(SEXP), const Rboolean contains_check);
static Rboolean depends_on(const SEXP expression, const SEXP variable, const SEXP sigma,
                           Rboolean (*const is_variable)(SEXP));
static R_INLINE Rboolean is_equal(const SEXP a, const SEXP b);
static R_INLINE Rboolean is_na_logical(const SEXP a);
static R_INLINE Rboolean is_compound_expression(const SEXP a);

SEXP unify_match(const SEXP a, const SEXP b, const SEXP sigma,
                 Rboolean (*const is_variable)(SEXP), const Rboolean contains_check) {
  if (is_na_logical(sigma))
    return sigma; // pass the fail on
  else if (is_equal(a, b))
    return sigma;
  else if (is_variable(a))
    return extend_if_possible(a, b, sigma, is_variable, contains_check);
  else if (is_variable(b))
    return extend_if_possible(b, a, sigma, is_variable, contains_check);
  else if (is_compound_expression(a) && is_compound_expression(b))
    return unify_match(CDR(a), CDR(b),
                       unify_match(CAR(a), CAR(b), sigma, is_variable, contains_check),
                       is_variable, contains_check);
  else // otherwise...
    return ScalarLogical(NA_LOGICAL); // fail
}

static SEXP extend_if_possible(const SEXP variable, const SEXP value, const SEXP sigma,
                               Rboolean (*const is_variable)(SEXP), const Rboolean contains_check) {
  if (contains_alist(variable, sigma))
    return unify_match(get_alist(variable, sigma), value, sigma, is_variable, contains_check);
  else if (is_variable(value))
    if (contains_alist(value, sigma))
      return unify_match(variable, get_alist(value, sigma), sigma, is_variable, contains_check);
    else
      return add_alist(variable, value, sigma);
  else if (contains_check && depends_on(value, variable, sigma, is_variable)) // "contains-check"
    return ScalarLogical(NA_LOGICAL); // fail
  else
    return add_alist(variable, value, sigma);
}

static Rboolean depends_on(const SEXP expression, const SEXP variable, const SEXP sigma,
                           Rboolean (*const is_variable)(SEXP)) {
  if (is_variable(expression))
    if (is_equal(variable, expression))
      return TRUE;
    else if (contains_alist(expression, sigma))
      return depends_on(get_alist(expression, sigma), variable, sigma, is_variable);
    else
      return FALSE;
  else if (isNull(expression))
    return FALSE;
  else if (is_compound_expression(expression))
    return depends_on(CAR(expression), variable, sigma, is_variable)
      || depends_on(CDR(expression), variable, sigma, is_variable);
  else return FALSE;
}

static R_INLINE Rboolean is_equal(const SEXP a, const SEXP b) {
  // return R_compute_identical(a, b, TRUE, TRUE, TRUE); // R_compute_identical is missing from Linux-R?
  error("is_equal: Not implemented."); // TODO
}

static R_INLINE Rboolean is_na_logical(const SEXP a) {
  return isLogical(a) && LOGICAL(a)[0] == NA_LOGICAL;
}

static R_INLINE Rboolean is_compound_expression(const SEXP a) {
  return isList(a) || (!isSymbol(a) && isLanguage(a));
}
