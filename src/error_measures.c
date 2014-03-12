#include <R.h>
#include <Rinternals.h>

static double mae(const double *x, const double *y, R_len_t n_x, R_len_t n_y) {
    R_len_t n = (n_x > n_y) ? n_x : n_y;
    double sum = 0.0;
    R_len_t i;
    for (i = 0; i < n; ++i) {
        sum += fabs(x[i % n_x] - y[i % n_y]);
    }
    return sum / n;
}

static double sse(const double *x, const double *y, R_len_t n_x, R_len_t n_y) {
    R_len_t n = (n_x > n_y) ? n_x : n_y;
    double sum = 0.0;
    R_len_t i;
    for (i = 0; i < n; ++i) {
        const double delta = x[i % n_x] - y[i % n_y];
        sum += delta * delta;
    }
    return sum;
}

static double ssse(const double *x, const double *y, R_len_t n_x, R_len_t n_y) {
    R_len_t n = (n_x > n_y) ? n_x : n_y;
    R_len_t i;
    double sum = 0.0, mean_x = 0.0, mean_y = 0.0, var_y = 0.0, cov_xy = 0.0;
    
    for (i = 0; i < n; ++i) {
        mean_x += x[i % n_x];
        mean_y += y[i % n_y];
    }
    mean_x /= n;
    mean_y /= n;

    for (i = 0; i < n; ++i) {
        cov_xy += (x[i % n_x] - mean_x) * (y[i % n_y] - mean_y);
        var_y += (y[i % n_y] - mean_y) * (y[i % n_y] - mean_y);
    }
    cov_xy /= (n-1);
    var_y /= (n-1);

    const double b = cov_xy / var_y;
    const double a = mean_x - b * mean_y;
    
    for (i = 0; i < n; ++i) {
        const double delta = x[i % n_x] - (a + b * y[i % n_y]);        
        sum += delta * delta;
    }
    return sum;
}

/*
 * Check that the SEXP A points to a real (double) vector.
 */
#define CHECK_ARG_IS_REAL_VECTOR(A)                        \
    if (!isReal(A) || !isVector(A))                        \
        error("Argument '" #A "' is not a real vector.");

/*
 * Unpack a real vector stored in SEXP S.
 */
#define UNPACK_REAL_VECTOR(S, D, N)             \
    CHECK_ARG_IS_REAL_VECTOR(S);                \
    double *D = REAL(S);                        \
    const R_len_t N = length(S);                   

#define ERROR_MEASURE(N, F)             \
    SEXP N(SEXP s_x, SEXP s_y) {        \
    UNPACK_REAL_VECTOR(s_x, x, n_x);    \
    UNPACK_REAL_VECTOR(s_y, y, n_y);    \
    double result = F(x, y, n_x, n_y);  \
    return ScalarReal(result);          \
}

ERROR_MEASURE(do_mae, mae)

ERROR_MEASURE(do_sse, sse)

ERROR_MEASURE(do_ssse, ssse)
