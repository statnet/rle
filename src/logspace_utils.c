#include<R.h>
#include<Rmath.h>
#include<Rinternals.h>

/*
 * Compute log sum x given log(x) values logx
 *
 *     log (sum_i  exp (logx[i]) ) =
 *     log (e^M * sum_i  e^(logx[i] - M) ) =
 *     M + log( sum_i  e^(logx[i] - M)
 *
 * without causing overflows or throwing much accuracy.
 *
 * Based on logspace_sum in pgamma.c in R; unlike that implementation,
 * it does not use the long double type, sacrificing precision for a
 * speed gain.
 */
double log_sum_exp(const double *logx, int n){
  if(n == 1) return logx[0];
  if(n == 2) return logspace_add(logx[0], logx[1]);
  // else (n >= 3) :
  int i;
  // Mx := max_i log(x_i)
  double Mx = logx[0];
  for(i = 1; i < n; i++) if(Mx < logx[i]) Mx = logx[i];
  double s = 0.;
  for(i = 0; i < n; i++) s += exp(logx[i] - Mx);
  return Mx + log(s);
}


SEXP log_sum_exp_wrapper(SEXP logx, SEXP long_double){
  long_double = PROTECT(coerceVector(long_double, LGLSXP));
  logx = PROTECT(coerceVector(logx, REALSXP));
  int n = length(logx);
  SEXP out = PROTECT(allocVector(REALSXP, 1));
  if(LOGICAL(long_double)[0])
    REAL(out)[0] = logspace_sum(REAL(logx), n);
  else
    REAL(out)[0] = log_sum_exp(REAL(logx), n);
  UNPROTECT(3);
  return(out);
}


/*
 * Compute a weighted mean of x given log-weights lw
 *
 *     log (sum_i  exp (logx[i]) ) =
 *     log (e^M * sum_i  e^(logx[i] - M) ) =
 *     M + log( sum_i  e^(logx[i] - M)
 *
 * without causing overflows or throwing much accuracy.
 * Based on logspace_sum in pgamma.c in R.
 */
double logspace_wmean (const double *x, const double* logw, int n)
{
    if(n == 1) return x[0];
    // else (n >= 2) :
    int i;
    // Mw := max_i log(w_i)
    double Mw = logw[0];
    for(i = 1; i < n; i++) if(Mw < logw[i]) Mw = logw[i];
    double sw = 0., sxw = 0.;
    for(i = 0; i < n; i++){
      double w = exp(logw[i] - Mw);
      sw += w;
      sxw += w*x[i];
    }
    return (double) sxw/sw;
}


SEXP logspace_wmean_wrapper(SEXP x, SEXP logw){
  x = PROTECT(coerceVector(x, REALSXP));
  logw = PROTECT(coerceVector(logw, REALSXP));
  int n = length(x);
  if(n != length(logw)) error("Lengths of value and log-weight vectors differ.");
  SEXP out = PROTECT(allocVector(REALSXP, 1));
  REAL(out)[0] = logspace_wmean(REAL(x), REAL(logw), n);
  UNPROTECT(3);
  return(out);
}