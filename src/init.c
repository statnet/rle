/*  File src/init.c in package rle, currently hosted at https://github.com/statnet/rle .
 *
 *  This software is distributed under the GNU General Public License Version 3 or later.
 *  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
 *
 *  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
 */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP sync_RLEs(SEXP, SEXP);
extern SEXP compress_RLE(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"sync_RLEs",               (DL_FUNC) &sync_RLEs,               2},
    {"compress_RLE",             (DL_FUNC) &compress_RLE,             3},
    {NULL, NULL, 0}
};

void R_init_statnet_common(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
