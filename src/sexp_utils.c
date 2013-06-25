/* sexp_utils.c
 *
 */

#include "sexp_utils.h"
#include <setjmp.h>


SEXP deep_copy_closxp(SEXP closxp) {
  SEXP copied_closxp = duplicate(closxp);
  // duplicate() by default does a shallow copy of a CLOSXP body, fix this
  SET_BODY(copied_closxp, duplicate(BODY(closxp)));
  return copied_closxp;
}

SEXP map_sexp(SEXP (*const f)(SEXP), SEXP sexp) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP:
    return f(LCONS(map_sexp(f, CAR(sexp)),
                   map_sexp(f, CDR(sexp)))); // map inner nodes, recurse
  case LISTSXP: // TODO this is buggy, as intermediates are not PROTECTED. use a for-loop instead
    return f(CONS(map_sexp(f, CAR(sexp)),
                  map_sexp(f, CDR(sexp)))); // map inner nodes, recurse
  default: // base case
    return f(sexp); // map leafs
  }
}

SEXP map_sexp_leafs(SEXP (*const f)(SEXP), SEXP sexp) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP:
    return LCONS(map_sexp_leafs(f, CAR(sexp)),
                 map_sexp_leafs(f, CDR(sexp))); // do nothing with inner nodes, recurse
  case LISTSXP: // TODO this is buggy, as intermediates are not PROTECTED. use a for-loop instead
    return CONS(map_sexp_leafs(f, CAR(sexp)),
                map_sexp_leafs(f, CDR(sexp))); // do nothing with inner nodes, recurse
  default: // base case
    return f(sexp); // map leafs
  }
}

SEXP map_sexp_inner_nodes(SEXP (*const f)(SEXP), SEXP sexp) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP:
    return f(LCONS(map_sexp_inner_nodes(f, CAR(sexp)),
                   map_sexp_inner_nodes(f, CDR(sexp)))); // map inner nodes, recurse
  case LISTSXP: // TODO this is buggy, as intermediates are not PROTECTED. use a for-loop instead
    return f(CONS(map_sexp_inner_nodes(f, CAR(sexp)),
                  map_sexp_inner_nodes(f, CDR(sexp)))); // map inner nodes, recurse
  default: // base case
    return sexp; // do noting with leafs
  }
}

// TODO
SEXP map_sexp_shortcut(SEXP (*const f)(SEXP), SEXP sexp) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: {
    SEXP mapped_sexp = f(sexp);
    if (sexp == mapped_sexp)
      return LCONS(map_sexp_shortcut(f, CAR(sexp)),
                   map_sexp_shortcut(f, CDR(sexp))); // recurse
    else
      return mapped_sexp; // shortcut
  }
  case LISTSXP: { // TODO this is buggy, as intermediates are not PROTECTED. use a for-loop instead
    SEXP mapped_sexp = f(sexp);
    if (sexp == mapped_sexp)
      return CONS(map_sexp_shortcut(f, CAR(sexp)),
                  map_sexp_shortcut(f, CDR(sexp))); // recurse
    else
      return mapped_sexp; // shortcut
  }
  default: // base case
    return f(sexp); // map leafs
  }
}

// TODO
static SEXP map_sexp_shortcut_depth_recursive(SEXP (*const f)(SEXP, int), SEXP sexp, const int current_depth) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: {
    SEXP mapped_sexp = f(sexp, current_depth);
    if (sexp == mapped_sexp) // recurse iff the current sexp was not replaced
      return LCONS(map_sexp_shortcut_depth_recursive(f, CAR(sexp), current_depth + 1),
                   map_sexp_shortcut_depth_recursive(f, CDR(sexp), current_depth + 1)); // recurse
    else
      return mapped_sexp; // shortcut
  }
  case LISTSXP: { // TODO this is buggy, as intermediates are not PROTECTED. use a for-loop instead
    SEXP mapped_sexp = f(sexp, current_depth);
    if (sexp == mapped_sexp) // recurse iff the current sexp was not replaced
      return CONS(map_sexp_shortcut_depth_recursive(f, CAR(sexp), current_depth + 1),
                  map_sexp_shortcut_depth_recursive(f, CDR(sexp), current_depth + 1)); // recurse
    else
      return mapped_sexp; // shortcut
  }
  default: // base case
    return f(sexp, current_depth); // map leafs
  }
}

// TODO
SEXP map_sexp_shortcut_depth(SEXP (*const f)(SEXP, int), SEXP sexp) {
  return map_sexp_shortcut_depth_recursive(f, sexp, 0);
}

void modify_sexp_shortcut(void (*const f)(SEXP), SEXP sexp) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP: // TODO this is buggy, as intermediates are not PROTECTED. use a for-loop instead
    f(sexp); // modify (entire) expression
    modify_sexp_shortcut(f, CAR(sexp)); // recurse on first element of expression
    modify_sexp_shortcut(f, CDR(sexp)); // recurse on rest elements of expression
  default: // base case
    f(sexp); // map leafs
  }
}

int sexp_size(SEXP sexp) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return 0;
  case LANGSXP: // fall-through to next case
  case LISTSXP: // TODO this is buggy, as intermediates are not PROTECTED. use a for-loop instead
    return sexp_size(CAR(sexp)) + sexp_size(CDR(sexp));
  default: // base case
    return 1; 
  }
}

SEXP sexp_size_R(SEXP sexp) {
  SEXP s;
  PROTECT(s = allocVector(INTSXP, 1));
  INTEGER(s)[0] = sexp_size(sexp);
  UNPROTECT(1);
  return s;
}

SEXP get_sexp_subtree_recursive(SEXP sexp, int index, int *current_index) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return R_NilValue; // NULL means "index not found"
  case LANGSXP: {
    if (index == *current_index) {
      return sexp;
    }
    int function_arity = 0;
    for (SEXP iterator = CDR(sexp); !isNull(iterator); iterator = CDR(iterator)) { // recurse on actual parameters
      *current_index += 1;
      function_arity++; // determine arity on the fly
      SEXP result;
      PROTECT(result = get_sexp_subtree_recursive(CAR(iterator), index, current_index)); 
      if (R_NilValue != result) {
        UNPROTECT(function_arity);
        return result;
      }
    }
    UNPROTECT(function_arity);
    return R_NilValue; // NULL means "index not found"
  }
  case LISTSXP:
    error("get_sexp_subtree_recursive: unexpected LISTSXP");
  default: // base case
    if (index == *current_index) {
      return sexp;
    } else {
      return R_NilValue; // nil := index not found
    }
  }
}

SEXP get_sexp_subtree(SEXP sexp, int index) {
  int current_index = 0;
  return get_sexp_subtree_recursive(sexp, index, &current_index);
}

SEXP get_sexp_subtree_R(SEXP sexp, SEXP index) {
  return get_sexp_subtree(sexp, INTEGER(index)[0]);
}

SEXP replace_sexp_subtree_recursive(SEXP sexp, int index, SEXP replacement, int *current_index) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp;
  case LANGSXP: {
    if (index == *current_index) {
      return replacement;
    }
    int function_arity = 0;
    SEXP tail_e, e;
    PROTECT(tail_e = R_NilValue);
    for (SEXP iterator = CDR(sexp); !isNull(iterator); iterator = CDR(iterator)) { // recurse on actual parameters
      *current_index += 1;
      function_arity++; // determine arity on the fly
      SEXP processed_parameter;
      PROTECT(processed_parameter = replace_sexp_subtree_recursive(CAR(iterator), index, replacement, current_index)); 
      PROTECT(tail_e = CONS(processed_parameter, tail_e));
    }
    PROTECT(e = LCONS(CAR(sexp), tail_e));
    UNPROTECT(2 * function_arity + 2);
    return e;
  }
  case LISTSXP:
    error("replace_sexp_subtree_recursive: unexpected LISTSXP");
  default: // base case
    if (index == *current_index) {
      return replacement;
    } else {
      return sexp;
    }
  }
}

SEXP replace_sexp_subtree(SEXP sexp, int index, SEXP replacement) {
  int current_index = 0;
  return replace_sexp_subtree_recursive(sexp, index, replacement, &current_index);
}

SEXP replace_sexp_subtree_R(SEXP sexp, SEXP index, SEXP replacement) {
  return replace_sexp_subtree(sexp, INTEGER(index)[0], replacement);
}

SEXP make_closure(SEXP body, SEXP formal_parameter_list, SEXP envir) {
  SEXP closure, formals;
  PROTECT(closure = allocSExp(CLOSXP));
  SET_CLOENV(closure, envir);
  const int number_of_formals = length(formal_parameter_list);
  PROTECT(formals = allocList(number_of_formals));
  SEXP formals_iterator = formals;
  for (int i = 0; i < number_of_formals; i++, formals_iterator = CDR(formals_iterator)) {
    SEXP formal = STRING_ELT(VECTOR_ELT(formal_parameter_list, i), 0);
    SET_TAG(formals_iterator, CreateTag(formal));
    SETCAR(formals_iterator, R_MissingArg);
  }
  SET_FORMALS(closure, formals);
  SET_BODY(closure, body);
  UNPROTECT(2);
  return closure;
}

