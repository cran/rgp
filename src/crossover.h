/* crossover.h
 *
 */

#ifndef CROSSOVER_H
#define CROSSOVER_H

#include <R.h>
#include <Rinternals.h>


/* crossover_single_point 
 *
 */
SEXP crossover_single_point(SEXP sexp_a, SEXP sexp_b);
SEXP crossover_single_point_R(SEXP sexp_a, SEXP sexp_b);

#endif

