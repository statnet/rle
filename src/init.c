/*  File src/init.c in package statnet.common, part of the Statnet suite
 *  of packages for network analysis, http://statnet.org .
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) at
 *  http://statnet.org/attribution
 *
 *  Copyright 2007-2019 Statnet Commons
 */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP sync_RLEs(SEXP, SEXP);
extern SEXP compact_RLE(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"sync_RLEs",               (DL_FUNC) &sync_RLEs,               2},
    {"compact_RLE",             (DL_FUNC) &compact_RLE,             2},
    {NULL, NULL, 0}
};

void R_init_statnet_common(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
