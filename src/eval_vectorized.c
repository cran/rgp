/* eval_vectorized.c
 *
 */

#include "eval_vectorized.h"
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>

// TODO move this function to environment_tools.c
static R_INLINE SEXP make_environment(SEXP enclosingEnvironment) { 
    SEXP env;
    PROTECT(env = allocSExp(ENVSXP));
    SET_FRAME(env, R_NilValue);
    SET_ENCLOS(env, (enclosingEnvironment)? enclosingEnvironment : R_GlobalEnv);
    SET_HASHTAB(env, R_NilValue);
    SET_ATTRIB(env, R_NilValue);
    UNPROTECT(1);
    return env; 
}

// TODO move this function to symbol_tools.c
static R_INLINE SEXP make_fresh_symbol(int idx, SEXP env) {
    // TODO this function must create a symbol that is fresh (unbound) in env
    const int freshStringSize = 8 + (int) log10(idx + 1) + 1;
    char freshString[freshStringSize];
    sprintf(freshString, "__arg%d__", idx);
    return install(freshString);
}

void eval_vectorized_recursive(SEXP rExpr, 
                               struct EvalVectorizedContext *context, 
                               double *out_result);

static R_INLINE void eval_vectorized_fallback(SEXP rExpr, 
                                              struct EvalVectorizedContext *context, 
                                              double *out_result) {
    // TODO this function is buggy
    const int samples = context->samples;
    warning("using eval_vectorized_fallback for function '%s'\n", CHAR(PRINTNAME(CAR(rExpr))));
    SEXP argItor; // argument iterator
    SEXP call; // call object, initialized with function name
    SEXP callRev;
    SEXP result;
    PROTECT(callRev = lang1(CAR(rExpr))); // call is build in reverse
    context->fallbackProtectCount++;

    // evaluate the arguments with eval_vectorized_recursive...
    for (argItor = CDR(rExpr); !isNull(argItor); argItor = CDR(argItor)) {
        SEXP argVal;
        PROTECT(argVal = allocVector(REALSXP, samples));
        context->fallbackProtectCount++;
        eval_vectorized_recursive(CAR(argItor), context, REAL(argVal));
        PROTECT(callRev = LCONS(argVal, callRev)); // add argument to function call object
        context->fallbackProtectCount++;
    }

    // build call...
    PROTECT(call = lang1(CAR(callRev)));
    context->fallbackProtectCount++;
    for (argItor = CDR(callRev); !isNull(argItor); argItor = CDR(argItor)) { // reverse callRev into call
        PROTECT(call = LCONS(CAR(argItor), call));
        context->fallbackProtectCount++;
    }
  
    // evaluate call via R's evaluator...
    PROTECT(result = eval(call, R_GlobalEnv));
    context->fallbackProtectCount++;

    double *resultData = REAL(result);
    memcpy(out_result, resultData, samples * sizeof(double));

    if (context->keepIntermediateResults) {
        SEXP intermediateResult;
        PROTECT(intermediateResult = allocVector(REALSXP, samples));
        double *intermediateResultData = REAL(intermediateResult);
        memcpy(intermediateResultData, out_result, samples * sizeof(double));
        PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
        //Rprintf("0) intermediate result: ");
        //for (int i = 0; i < samples; i++)
        //    Rprintf("%f ", out_result[i]);
        //Rprintf("\n");
    }
    return;
}

#include "evaluate_language_expression.h"

void eval_vectorized_recursive(SEXP rExpr, 
                               struct EvalVectorizedContext *context, 
                               double *out_result) {
    const int samples = context->samples;
    /* Composite R expression: */
    if (isLanguage(rExpr)) {
        int out_is_scalar_result;
        evaluate_language_expression(rExpr, context, out_result, &out_is_scalar_result);
        if (out_is_scalar_result) {
          /* expand scalar result (to fill up the entire result vector) */
          const double value = out_result[0];
          for (R_len_t i = 0; i < samples; ++i) 
              out_result[i] = value;
        }
    } else  {
        /* The following two case should only be exercised if the
         * top-level rExpr is a Symbol or
         * Numeric. evaluate_language_expression() internally avoids
         * dispatching on these two types. Instead they are special
         * cased for efficiency.
         */
        if (isNumeric(rExpr)) {
            const double value = REAL(rExpr)[0];
            for (int i = 0; i < samples; i++) 
                out_result[i] = value;
            if (context->keepIntermediateResults) {
                SEXP intermediateResult;
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                double *intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, out_result, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("1) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", out_result[i]);
                //Rprintf("\n");
            }
            return;
        } else if (isSymbol(rExpr)) {
            const char *rSymbol = CHAR(PRINTNAME(rExpr));
            /* find matching actualParameter vector and return it... */
            for (int i = 0; i < context->arity; i++) {
                if (!strcmp(rSymbol, CHAR(STRING_ELT(context->formalParameters, i)))) {
                    const R_len_t offset = i * samples;
                    for (int j = 0; j < samples; j++) {
                        out_result[j] = context->actualParameters[offset + j];
                    }
                    if (context->keepIntermediateResults) {
                        SEXP intermediateResult;
                        PROTECT(intermediateResult = allocVector(REALSXP, samples));
                        double *intermediateResultData = REAL(intermediateResult);
                        memcpy(intermediateResultData, out_result, samples * sizeof(double));
                        PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                        //Rprintf("2) intermediate result: ");
                        //for (int i = 0; i < samples; i++)
                        //    Rprintf("%f ", out_result[i]);
                        //Rprintf("\n");
                    }
                    return;
                }
            }
            error("eval_vectorized_recursive: undefined symbol '%s'.", rSymbol);
        } else
            error("eval_vectorized_recursive: unsupported R expression");
    }
}

void initialize_eval_vectorized_context(SEXP rFunction,
                                        SEXP actualParameters,
                                        int keepIntermediateResults,
                                        struct EvalVectorizedContext *contextOut) {
    SEXP rFormals, rFormalNames;

    contextOut->outIntermediateResults = R_NilValue; 

    contextOut->fallbackProtectCount = 0;
    
    contextOut->keepIntermediateResults = keepIntermediateResults;
    
    PROTECT(rFormals = FORMALS(rFunction));
    PROTECT(rFormals= coerceVector(rFormals, VECSXP));
    int arity = LENGTH(rFormals);
    contextOut->arity = arity;
    
    PROTECT(rFormalNames = getAttrib(rFormals, R_NamesSymbol));
    contextOut->formalParameters = rFormalNames;
    if (arity) {
        contextOut->samples = LENGTH(actualParameters) / arity;
    } else {
        contextOut->samples = 1;
    }
    
    PROTECT(actualParameters = coerceVector(actualParameters, REALSXP));
    contextOut->actualParameters = REAL(actualParameters);
}

SEXP eval_vectorized(SEXP rFunction, SEXP actualParameters, int keepIntermediateResults) {
  struct EvalVectorizedContext context;
  initialize_eval_vectorized_context(rFunction, actualParameters, keepIntermediateResults, &context);
  
  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, context.samples));  
  double *result = REAL(rResult);
  eval_vectorized_recursive(BODY(rFunction), &context, result);
  UNPROTECT(1 + 4); // 4 are PROTECTed in "initialize_eval_vectorized_context"
  UNPROTECT(context.fallbackProtectCount); // UNPROTECT values PROTECTed in eval_vectorized_fallback
  if (keepIntermediateResults) {
      const R_len_t numberOfIntermediateResults = length(context.outIntermediateResults);
      UNPROTECT(numberOfIntermediateResults * 2); // intermediate result vectors and cons cells are protected on creation
      return context.outIntermediateResults;
  } else
      return rResult;
}

SEXP eval_vectorized_R(SEXP rFunction, SEXP actualParameters, SEXP keepIntermediateResults) {
  SEXP result;
  PROTECT(result = eval_vectorized(rFunction,
                                   actualParameters,
                                   asLogical(keepIntermediateResults)));
  UNPROTECT(1);
  return result;
}

SEXP eval_vectorized_rmse(SEXP rFunction, SEXP actualParameters, SEXP targetValues, double *bestRMSE) {
  struct EvalVectorizedContext context;
  initialize_eval_vectorized_context(rFunction, actualParameters, 0, &context);
  double result[context.samples];
  
  eval_vectorized_recursive(BODY(rFunction), &context, result);
  
  // calculate RMSE...
  double diff, total = 0.0, rmse;
  PROTECT(targetValues = coerceVector(targetValues, REALSXP));
  
  for (int i = 0; i < context.samples; i++) {
    diff = result[i] - REAL(targetValues)[i];
    total += diff * diff;
  }
  rmse = sqrt(total / (double) context.samples);
  if(*bestRMSE > rmse) {
    *bestRMSE= rmse; }
  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, 1));
  
  if(!isnan(rmse)) {  //nan-exception error handling
  REAL(rResult)[0] = rmse;
  UNPROTECT(6);
  return rResult; } else { 
    REAL(rResult)[0] = 10000000000000; //large number TODO: set to inf
    UNPROTECT(6);
    return rResult;
  }
}

