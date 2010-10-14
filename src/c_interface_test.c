#include <R.h>
#include <Rinternals.h>
#include <float.h>
#include "list_utils.h"
#include "sexp_utils.h"
#include "unification.h"

#define CHECK_ARG_IS_FUNCTION(A) \
  if (!isFunction(A)) \
    error("Argument '" #A "' is not a function.");
#define CHECK_ARG_IS_LIST(A) \
  if (!isList(A)) \
    error("Argument '" #A "' is not a list.");
#define CHECK_ARG_IS_NEW_LIST(A) \
  if (!isNewList(A)) \
    error("Argument '" #A "' is not a 'new list'.");
#define CHECK_ARG_IS_ENVIRONMENT(A) \
  if (!isEnvironment(A)) \
    error("Argument '" #A "' is not an environment.");
#define CHECK_ARG_IS_NUMERIC(A) \
  if (!isNumeric(A)) \
    error("Argument '" #A "' is not numeric.");
#define CHECK_ARG_IS_INTEGER(A)                 \
  if (!isInteger(A))                                \
    error("Argument '" #A "' is not an integer.");
#define CHECK_ARG_IS_LOGICAL(A)                 \
  if (!isLogical(A))                                \
    error("Argument '" #A "' is not a logical.");


SEXP test_hello_world(const SEXP arg) {
  Rprintf("Hello, C, this is R!\n");
  return R_NilValue;
}

SEXP test_call_function(const SEXP f, const SEXP reps) {
  PROTECT_INDEX ip;
  CHECK_ARG_IS_FUNCTION(f);
  CHECK_ARG_IS_NUMERIC(reps);
  const int repeats = (int) *REAL(reps);
  const SEXP f_call = PROTECT(lang1(f));
  SEXP last_result = R_NilValue;
  for (int i = 0; i < repeats; i++) {
      /* This should really be protected: */
      PROTECT_WITH_INDEX(last_result = eval(f_call, R_GlobalEnv), &ip); 
      /* Do something with last_result. Maby cast it... */
      REPROTECT(last_result = coerceVector(last_result, REALSXP), ip);
      /* Use s_fval ... */

      /* And unprotect it. */
      UNPROTECT(1); /* last_result */
  }
  UNPROTECT(1); /* f_call */
  return last_result;
}

Rboolean test_is_variable(const SEXP s) {
  // here, variables as symbols not bound in the global environment
  return isSymbol(s) && findVar(s, R_GlobalEnv) == R_UnboundValue;
}

SEXP test_unify_match(const SEXP a, const SEXP b, const SEXP contains_check) {
  CHECK_ARG_IS_LOGICAL(contains_check);
  return unify_match(a, b, make_alist(), test_is_variable, asLogical(contains_check));
}

SEXP make_formals(const SEXP formal_names) {
  const SEXP formals = PROTECT(allocList(length(formal_names)));
  SEXP formal = formals;
  for(int i = 0; i < length(formal_names); i++) {
    SET_TAG(formal, install(translateChar(STRING_ELT(formal_names, i))));
    SETCAR(formal, R_MissingArg);
    formal = CDR(formal);
  }

  UNPROTECT(1);
  return formals;
}

SEXP make_function(const SEXP formals, const SEXP body, const SEXP environment) {
  const SEXP function = PROTECT(allocSExp(CLOSXP)); // create an empty function (closure),...
  SET_CLOENV(function, environment); // ...set its closure environment,...
  SET_FORMALS(function, formals); // ...its formal arguments,...
  SET_BODY(function, body); // ...as well as its body

  UNPROTECT(1);
  return function;
}

SEXP make_real_vector(const double real_data[]) {
  /* FIXME: This does not work because sizeof(real_data) ==
   *   sizeof(double *) By pure luck (you are running on a 64bit
   *   system right?)  sizeof(double *) == sizeof(double) and len is
   *   always 1. You need to explicitly pass the size of real_data
   *   as a size_t.
   */
  const int len = sizeof(real_data) / sizeof(double);
  
  const SEXP s_real = PROTECT(allocVector(REALSXP, len));
  double *real = REAL(s_real);
#if defined(GO_SLOW)
  /* REAL() usually does a type check on the SEXP. No need to do the
   * same check len times. 
   */
  for (int i = 0; i < len; i++) 
    REAL(s_real)[i] = real_data[i]; 
#elif defined(GO_FASTER)
  for (int i = 0; i < len; i++) 
    real[i] = real_data[i]; 
#else
  /* memcpy() is usually hand optimized assembler. Most likely faster
   * than any for loop for small len and not slower for large len.
   */
  memcpy(real, real_data, len);
#endif
  UNPROTECT(1);
  return (SEXP) real;
}

/* test_make_function_sexp
 * Just for fun, build the R function "function(x) 1 / sin(x)" and return it.
 */
SEXP test_make_function_sexp() {
  const double one_array[] = { 1.0 };
  const SEXP one = PROTECT(make_real_vector(one_array)); // the real 1.0
  const SEXP x = PROTECT(install("x")); // the symbol "x"
  const SEXP f_body = PROTECT(LCONS(install("/"), LCONS(one, LCONS(LCONS(install("sin"), LCONS(x, R_NilValue)), R_NilValue)))); // in Lisp syntax, this just means "(/ 1.0 (sin x))"

  const SEXP f_formals = PROTECT(CONS(R_MissingArg, R_NilValue)); // formals in R are quite funny creatures
  SET_TAG(f_formals, x); // "

  const SEXP f = PROTECT(make_function(f_formals, f_body, R_GlobalEnv)); // R_GlobalEnv could also be R_BaseEnv or given as a parameter

  UNPROTECT(5);
  return f;
}

/* Probably want to look at src/main/inspect.c in the R source tree
 * for inspiration. Maby even dispatch to do_inspect()?
*/
SEXP print_sexp(const SEXP sexp) {
  // if this code was serious, it would use an ugly hairy switch statement for speed...
  if (isSymbol(sexp)) {
    Rprintf("symbol '%s'\n", CHAR(PRINTNAME(sexp)));
  } else if (isNumeric(sexp)) {
    if (isInteger(sexp)) {
      Rprintf("integer '%d'\n", *INTEGER(sexp));
    } else if (isReal(sexp)) {
      Rprintf("real '%f'\n", *REAL(sexp));
    } else {
      Rprintf("numeric\n");
    }
  } else if (isLogical(sexp)) {
    Rprintf("logical\n"); // TODO
  } else if (isString(sexp)) {
    Rprintf("string\n"); // TODO
  } else {
    Rprintf("something else (TYPE %d)\n", TYPEOF(sexp));
  }
  return sexp;
}

// BEWARE this modifies its argument list in-place! Never use this function directly from R (TODO make this static)
SEXP modify_pairlist(const SEXP list, const SEXP indexSexp, const SEXP replacementElement) {
  CHECK_ARG_IS_INTEGER(indexSexp);
  const int index = INTEGER(indexSexp)[0]; // the given index should be 0-based, contrary to R convention

  SEXP rest = list;
  int i = 0;
  for (; rest != R_NilValue; rest = CDR(rest), i++) {
    if (i == index) {
      SETCAR(rest, replacementElement);
      break;
    }
  }

  return list;
}

// You MUST call GetRNGstate() before and PutRNGstate() after a batch of calls to this function!
static R_INLINE SEXP mutate_constant_sexp(const SEXP sexp) {
  if (isNumeric(sexp)) {
    const double *real_data = REAL(sexp);
    const int len = sizeof(real_data) / sizeof(double);
    double mutant_real_data[len];
    for (int i = 0; i < len; i++)
      mutant_real_data[i] = real_data[i] + norm_rand(); // TODO add distribution parameters
    return make_real_vector(mutant_real_data);
  } else {
    return sexp;
  }
}

SEXP mutate_constant_sexps(const SEXP f) {
  CHECK_ARG_IS_FUNCTION(f);

  GetRNGstate();
  const SEXP mutant_body = PROTECT(map_sexp_leafs(mutate_constant_sexp, BODY(f)));
  PutRNGstate();

  UNPROTECT(1);
  return make_function(FORMALS(f), mutant_body, CLOENV(f));
}

typedef struct SEXP_PAIR {
  const SEXP first;
  const SEXP second;
} SEXP_PAIR;

// TODO make this "static R_INLINE"
SEXP_PAIR choose_crossover_point(const SEXP e, const SEXP selected_point) {
  if (selected_point == NULL) { // no point selected yet
    const SEXP_PAIR result = { e, selected_point }; // TODO
    return result;
  } else { // point selected, just copy the tree from now on
    switch (TYPEOF(e)) { // switch for speed
    case NILSXP: {
       const SEXP_PAIR result = { e, selected_point };
       return result;
    }
    case LANGSXP: // fall-through to next case
    case LISTSXP: {
      const SEXP_PAIR result = {  LCONS((choose_crossover_point(CAR(e), selected_point)).first,
                                        (choose_crossover_point(CDR(e), selected_point)).second),
                                  selected_point };
      return result;
    }
    default: { // base case
      const SEXP_PAIR result = { e, selected_point };
      return result;
    }
    }
  }
}

// TODO
static R_INLINE SEXP_PAIR uniform_one_point_crossover_strategy(const SEXP e0, const SEXP e1) {
  // TODO
  Rprintf("in uniform_one_point_crossover_strategy...\n"); // TODO
  const SEXP_PAIR result = { e0, e1 };
  return result;
}

SEXP crossover_functions(const SEXP f, const SEXP g, SEXP_PAIR (*const crossover_strategy)(SEXP, SEXP)) {
  CHECK_ARG_IS_FUNCTION(f);
  CHECK_ARG_IS_FUNCTION(g);

  GetRNGstate();
  const SEXP_PAIR crossover_result_bodies = crossover_strategy(BODY(f), BODY(g));
  PutRNGstate();
  const SEXP f_prime = make_function(FORMALS(f), crossover_result_bodies.first, CLOENV(f));
  const SEXP g_prime = make_function(FORMALS(g), crossover_result_bodies.second, CLOENV(g));

  const SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, f_prime);
  SET_VECTOR_ELT(result, 1, g_prime);
  UNPROTECT(1);
  return result;
}

SEXP uniform_one_point_crossover_functions(const SEXP f, const SEXP g) {
  return crossover_functions(f, g, uniform_one_point_crossover_strategy);
}

/* Call this from R via .Call("test_function_manipulation", f, PACKAGE = "rgp"),
 * where f is a R function.
 * Example: .Call("test_function_manipulation", function(x) x + 1 + sin(2))
 */
SEXP test_function_manipulation(SEXP f) {
  CHECK_ARG_IS_FUNCTION(f);
  const SEXP f_formals = FORMALS(f);
  const SEXP f_body = BODY(f);

  return map_sexp(print_sexp, f_body);
}

/*Test-function - Remove*/

// return a random interger between 0 and size-1
// You MUST call GetRNGstate() before and PutRNGstate() after a batch of calls to this function!
static R_INLINE int rand_index(const int size) {
  const double r = unif_rand() - DBL_EPSILON; // subtract DBL_EPSILON to make sure that r < 1.0
  return (int) (r * size);
}

SEXP test_rng() {
  const double test_array[] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0 };
  GetRNGstate();
  for (int i = 0; i < 100; i++) Rprintf("%f ", test_array[rand_index(sizeof(test_array) / sizeof(double))]);
  Rprintf("\n");
  PutRNGstate();

  return R_NilValue;
}

static R_INLINE SEXP build_tree_rec(int depth)
{	
	const double one_array[]= { 1.0 };
	const SEXP one= make_real_vector(one_array);
	if (depth > 0)
		return LCONS(install("+"),LCONS(build_tree_rec(depth-1),LCONS(one, R_NilValue)));
		//return LCONS(install("+"),LCONS(one,LCONS(build_tree_rec(depth-1), R_NilValue)));
	else
		return one;
}

SEXP new_sexp_tree(const SEXP reps)
{	
	SEXP rfun;
	CHECK_ARG_IS_NUMERIC(reps);
  	int repeats = (int) *REAL(reps);	
	rfun= build_tree_rec(repeats);
	return rfun;
}

