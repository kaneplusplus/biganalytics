#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP binit1BigMatrix(SEXP, SEXP, SEXP);
extern SEXP binit1RIntMatrix(SEXP, SEXP, SEXP);
extern SEXP binit1RNumericMatrix(SEXP, SEXP, SEXP);
extern SEXP binit2BigMatrix(SEXP, SEXP, SEXP, SEXP);
extern SEXP binit2RIntMatrix(SEXP, SEXP, SEXP, SEXP);
extern SEXP binit2RNumericMatrix(SEXP, SEXP, SEXP, SEXP);
extern SEXP CMaxColmain(SEXP, SEXP, SEXP, SEXP);
extern SEXP CMeanColmain(SEXP, SEXP, SEXP, SEXP);
extern SEXP CMinColmain(SEXP, SEXP, SEXP, SEXP);
extern SEXP ColCountNA(SEXP, SEXP);
extern SEXP CProdColmain(SEXP, SEXP, SEXP, SEXP);
extern SEXP CSumColmain(SEXP, SEXP, SEXP, SEXP);
extern SEXP CVarColmain(SEXP, SEXP, SEXP, SEXP);
extern SEXP kmeansBigMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP kmeansRIntMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP kmeansRNumericMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP MatrixHashRanges(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"binit1BigMatrix",      (DL_FUNC) &binit1BigMatrix,      3},
    {"binit1RIntMatrix",     (DL_FUNC) &binit1RIntMatrix,     3},
    {"binit1RNumericMatrix", (DL_FUNC) &binit1RNumericMatrix, 3},
    {"binit2BigMatrix",      (DL_FUNC) &binit2BigMatrix,      4},
    {"binit2RIntMatrix",     (DL_FUNC) &binit2RIntMatrix,     4},
    {"binit2RNumericMatrix", (DL_FUNC) &binit2RNumericMatrix, 4},
    {"CMaxColmain",          (DL_FUNC) &CMaxColmain,          4},
    {"CMeanColmain",         (DL_FUNC) &CMeanColmain,         4},
    {"CMinColmain",          (DL_FUNC) &CMinColmain,          4},
    {"ColCountNA",           (DL_FUNC) &ColCountNA,           2},
    {"CProdColmain",         (DL_FUNC) &CProdColmain,         4},
    {"CSumColmain",          (DL_FUNC) &CSumColmain,          4},
    {"CVarColmain",          (DL_FUNC) &CVarColmain,          4},
    {"kmeansBigMatrix",      (DL_FUNC) &kmeansBigMatrix,      7},
    {"kmeansRIntMatrix",     (DL_FUNC) &kmeansRIntMatrix,     7},
    {"kmeansRNumericMatrix", (DL_FUNC) &kmeansRNumericMatrix, 7},
    {"MatrixHashRanges",     (DL_FUNC) &MatrixHashRanges,     2},
    {NULL, NULL, 0}
};

void R_init_biganalytics(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
