/* random_utils.h
 *
 */

#ifndef RANDOM_UTILS_H
#define RANDOM_UTILS_H

#include <R.h>
#include <Rinternals.h>


int random_index(int max_index);
SEXP random_index_R(SEXP max_index);

#endif

