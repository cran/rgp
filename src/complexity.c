/* complexity.c
 *
 */

#include "complexity.h"


struct SexpVisitationLengthContext {
  int keepIntermediateResults;
  SEXP outIntermediateResults;
  long protectCount;
};

struct SexpVisitationLengthResult {
  int treeSize;
  int visitationLength;
};

static R_INLINE struct SexpVisitationLengthResult sexp_visitation_length_recursive(SEXP sexp, struct SexpVisitationLengthContext *context) {
  // The visitation length of a tree T is the sum of the number of nodes of all subtrees of T...
  switch (TYPEOF(sexp)) {
  case LANGSXP: {
    int childrenTreeSizesSum = 0;
    int childrenVisitationLengthsSum = 0;
    for (SEXP iterator = CDR(sexp); !isNull(iterator); iterator = CDR(iterator)) { // recurse on actual parameters
      struct SexpVisitationLengthResult childResult;
      childResult = sexp_visitation_length_recursive(CAR(iterator), context); 
      childrenTreeSizesSum += childResult.treeSize;
      childrenVisitationLengthsSum += childResult.visitationLength;
    }
    const int thisTreeSize = 1 + childrenTreeSizesSum;
    const int thisVisitationLength = thisTreeSize + childrenVisitationLengthsSum;
    struct SexpVisitationLengthResult result = { thisTreeSize, thisVisitationLength };
    if (context->keepIntermediateResults) {
      SEXP intermediateResult;
      PROTECT(intermediateResult = allocVector(INTSXP, 1));
      INTEGER(intermediateResult)[0] = result.visitationLength;
      PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
      context->protectCount += 2;
    }
    return result;
  }
  default: { // base case
    struct SexpVisitationLengthResult result = { 1, 1 };
    if (context->keepIntermediateResults) {
      SEXP intermediateResult;
      PROTECT(intermediateResult = allocVector(INTSXP, 1));
      INTEGER(intermediateResult)[0] = result.visitationLength;
      PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
      context->protectCount += 2;
    }
    return result;
  }
  }
}

SEXP sexp_visitation_length(SEXP sexp, int keepIntermediateResults) {
  struct SexpVisitationLengthContext context;
  context.keepIntermediateResults = keepIntermediateResults;
  PROTECT(context.outIntermediateResults = R_NilValue);
  context.protectCount = 0;
  struct SexpVisitationLengthResult result = sexp_visitation_length_recursive(sexp, &context);
  if (keepIntermediateResults) {
    UNPROTECT(1 + context.protectCount);
    return context.outIntermediateResults;
  } else {
    SEXP result_R;
    PROTECT(result_R = allocVector(INTSXP, 1));
    INTEGER(result_R)[0] = result.visitationLength;
    UNPROTECT(2 + context.protectCount);
    return result_R;
  }
}

SEXP sexp_visitation_length_R(SEXP sexp, SEXP keepIntermediateResults) {
  return sexp_visitation_length(sexp, asLogical(keepIntermediateResults));
}

SEXP func_visitation_length_R(SEXP func, SEXP keepIntermediateResults) {
  return sexp_visitation_length(BODY(func), asLogical(keepIntermediateResults));
}

