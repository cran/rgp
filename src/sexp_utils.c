/* sexp_utils.c
 *
 */

#include "sexp_utils.h"


SEXP map_sexp(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return f(LCONS(map_sexp(CAR(sexp), f),
                   map_sexp(CDR(sexp), f))); // map inner nodes, recurse
  default: // base case
    return f(sexp); // map leafs
  }
}

SEXP map_sexp_leafs(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return LCONS(map_sexp_leafs(CAR(sexp), f),
                 map_sexp_leafs(CDR(sexp), f)); // do nothing with inner nodes, recurse
  default: // base case
    return f(sexp); // map leafs
  }
}

SEXP map_sexp_inner_nodes(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return f(LCONS(map_sexp_inner_nodes(CAR(sexp), f),
                   map_sexp_inner_nodes(CDR(sexp), f))); // map inner nodes, recurse
  default: // base case
    return sexp; // do noting with leafs
  }
}

// TODO
SEXP map_sexp_shortcut(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP: {
    const SEXP mapped_sexp = f(sexp);
    if (sexp == mapped_sexp)
      return LCONS(map_sexp_shortcut(CAR(sexp), f),
                   map_sexp_shortcut(CDR(sexp), f)); // recurse
    else
      return mapped_sexp; // shortcut
  }
  default: // base case
    return f(sexp); // map leafs
  }
}

// TODO
static SEXP map_sexp_shortcut_depth_recursive(const SEXP sexp, const int current_depth,
                                              SEXP (*const f)(SEXP, int)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP: {
    const SEXP mapped_sexp = f(sexp, current_depth);
    if (sexp == mapped_sexp)
      return LCONS(map_sexp_shortcut_depth_recursive(CAR(sexp), current_depth + 1, f),
                   map_sexp_shortcut_depth_recursive(CDR(sexp), current_depth + 1, f)); // recurse
    else
      return mapped_sexp; // shortcut
  }
  default: // base case
    return f(sexp, current_depth); // map leafs
  }
}

// TODO
SEXP map_sexp_shortcut_depth(const SEXP sexp, SEXP (*const f)(SEXP, int)) {
  return map_sexp_shortcut_depth_recursive(sexp, 0, f);
}
