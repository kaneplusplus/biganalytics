
#include <iostream> // hack to make sure we are using the right "length" 
                    // function

#include <Rcpp.h>

#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/bigmemoryDefines.h"
#include "bigmemory/isna.hpp"

#include <math.h>

// ---------------------------------------------------------------------------
// NEW

// NEW_INTEGER, INTEGER_DATA, etc... are macros, not functions; we provide
// simple wrappers to facilitate our use of function templates.

//SEXP NEW_INTEGERF(int i) { return(NEW_INTEGER(i)); }
//SEXP NEW_NUMERICF(int i) { return(Rf_allocVector(REALSXP, i)); }
//int* INTEGER_DATAF(SEXP x) { return(INTEGER_DATA(x)); }
//double* NUMERIC_DATAF(SEXP x) { return(REAL(x)); }

// These typedefs were needed in conjunction with the new functions, above,
// to facilitate passing these functions as arguments to functions.  This is
// just to aid readability.

//typedef SEXP (*sexpptrfun)(int);
//typedef int* (*intptrfun)(SEXP);
//typedef double* (*doubleptrfun)(SEXP);

// The following functions (tmin, tmax, etc...) were originally from
// summary.c, but template-ified.

// --------------------- min -------------------------------------------
template<typename T>
Rboolean tmin(T *x, index_type n, int *value, Rboolean narm, T NA_VALUE)
{ // Follow same logic as for Real and see!
  index_type i;
  int s = NA_INTEGER/* -Wall */;
  bool firstVal = false;
  Rboolean updated = (Rboolean)TRUE;

  for (i = 0; i < n; i++) {
    if (!isna(static_cast<T>(x[i]))) {
      if (!updated || s > x[i] || !firstVal) {
        s = x[i];
        if (!updated) updated = (Rboolean)TRUE;
        if (!firstVal) firstVal = true;
      }
    }
    else if (!narm) {
      *value = NA_INTEGER;
      return((Rboolean)TRUE);
    }
  }
  *value = s;
  return(updated);
}

Rboolean tmin(double *x, index_type n, double *value, Rboolean narm, 
              double NA_VALUE)
{
  double s = 0.0; /* -Wall */
  Rboolean updated = (Rboolean)FALSE;

  for (index_type i = 0; i < n; i++) {
    if (ISNAN(x[i])) {/* Na(N) */
      if (!narm) {
        if(!ISNA(s)) s = x[i]; /* so any NA trumps all NaNs */
        if(!updated) updated = (Rboolean)TRUE;
      } // narm = TRUE then nothing.
    } // When for all i: (ISNAN && narm) = TRUE then updated = FALSE
    else if (!updated || x[i] < s) {/* Never true if s is NA/NaN */
      s = x[i];
      if(!updated) updated = (Rboolean)TRUE;
    }
  }
  
  if (!updated) {
    // *value = NA_REAL;
    if (narm) {  // To Make consistent w/ R-min
      *value = R_PosInf;
    } else {
      *value = NA_REAL;
    }
  } else {
    *value = s;
  }
  return((Rboolean)TRUE);
}

// --------------------- max -------------------------------------------
template<typename T>
Rboolean tmax(T *x, index_type n, int *value, Rboolean narm, T NA_VALUE)
{
  index_type i;
  int s = NA_INTEGER/* -Wall */;
  bool firstVal = false;
  Rboolean updated = (Rboolean)TRUE;

  for (i = 0; i < n; i++) {
    if (!isna(static_cast<T>(x[i]))) {
      if (!updated || s < x[i] || !firstVal) {
        s = x[i];
        if(!updated) updated = (Rboolean)TRUE;
        if (!firstVal) firstVal=true;
      }
    } else if (!narm) {
      *value = NA_INTEGER;
      return((Rboolean)TRUE);
    }
  }
  *value = s;
  return(updated);
}

Rboolean tmax(double *x, index_type n, double *value, Rboolean narm,
              double NA_VALUE)
{
  double s = 0.0; /* -Wall */
  Rboolean updated = (Rboolean)FALSE;

  for (index_type i = 0; i < n; i++) {
    if (ISNAN(x[i])) {/* Na(N) */
      if (!narm) {
        if(!ISNA(s)) s = x[i]; /* so any NA trumps all NaNs */
        if(!updated) updated = (Rboolean)TRUE;
      }  // narm = TRUE then nothing.
    } // When for all i: (ISNAN && narm) = TRUE then updated = FALSE
    else if (!updated || x[i] > s) {/* Never true if s is NA/NaN */
      s = x[i];
      if(!updated) updated = (Rboolean)TRUE;
    }
  }
  
  if (!updated) {
    // *value = NA_REAL;
    if (narm) {  // To Make consistent w/ R-max
      *value = R_NegInf;
    } else {
      *value = NA_REAL;
    }
  } else {
    *value = s;
  }
  return((Rboolean)TRUE);
}

// --------------------- sum -------------------------------------------

template<typename T>
Rboolean tsum(T *x, index_type n, double *value, Rboolean narm, T NA_VALUE)
{
  LDOUBLE s = NA_REAL;
  index_type i;
  Rboolean updated = (Rboolean)TRUE;
  bool firstValue=false;

  for (i = 0; i < n; i++) {
    if (!isna(static_cast<T>(x[i]))) {
      if (!firstValue) 
      {
        s = x[i];
        firstValue=true;
      }
      else s += x[i];
    } else if (!narm) {
      *value = NA_REAL;
      return(updated);
    }
  }
  // Note the change from the standard isum, always returning double now.
  *value = s;

  return(updated);
}

Rboolean tsum(double *x, int n, double *value, Rboolean narm,
              double NA_VALUE)
{
  LDOUBLE s = NA_REAL;
  index_type i;
  Rboolean updated = (Rboolean)TRUE;
  bool firstValue=false;

  for (i = 0; i < n; i++) {
    if (!ISNAN(x[i]) || !narm) {
      if (!firstValue) 
      {
        s = x[i];
        firstValue=true;
      }
      else s += x[i];
    }
  }
  *value = s;

  return(updated);
}

// --------------------- prod -------------------------------------------

template<typename T>
Rboolean tprod(T *x, int n, double *value, Rboolean narm, T NA_VALUE)
{
  LDOUBLE s = NA_REAL;
  bool firstVal=false;
  index_type i;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
    if (!isna(static_cast<T>(x[i]))) {
      if (!firstVal) 
      {
        s = x[i];
        firstVal=true;
      }
      else s *= x[i];
 
      if(!updated) updated = (Rboolean)TRUE;
    }
    else if (!narm) {
      if(!updated) updated = (Rboolean)TRUE;
      *value = NA_REAL;
      return(updated);
    }
  }
  *value = s;

  return(updated);
}

Rboolean tprod(double *x, index_type n, double *value, Rboolean narm,
               double NA_VALUE)
{
  LDOUBLE s = NA_REAL;
  bool firstVal=false;
  index_type i;
  Rboolean updated = (Rboolean)TRUE;

  for (i = 0; i < n; i++) {
    if (!ISNAN(x[i]) || !narm) {
      if (!firstVal) 
      {
        firstVal=true;
        s = x[i];
      }
      else s *= x[i];
    }
  }
  *value = s;

  return(updated);
}

// --------------------- mean -------------------------------------------

template<typename T>
Rboolean tmean(T *x, index_type n, double *value, Rboolean narm, T NA_VALUE)
{
  LDOUBLE s = 0.0;
  index_type i;
  Rboolean updated = (Rboolean)TRUE;
  std::size_t naCount=0;
  for (i = 0; i < n; i++) {
    if (!isna(static_cast<T>(x[i])))
    { 
			s += x[i];
    }
    else if (!narm) {
      *value = NA_REAL;
      return(updated);
    }
    else
    {
      ++naCount;
    }
  }
  if (n-naCount > 0) s /= (LDOUBLE)(n-naCount);
  else s = NA_REAL;
  *value = (double) s;

  return(updated);
}

template<>
Rboolean tmean<double>(double *x, index_type n, double *value, Rboolean narm,
               double NA_VALUE)
{
  LDOUBLE s = 0.0, t = 0.0;
  index_type i;
  index_type naCount=0;
  Rboolean updated = (Rboolean)TRUE;

  for (i = 0; i < n; i++) {
    if (!ISNAN(x[i])) s += x[i];
    else if (!narm) {
      *value = NA_REAL;
      return((Rboolean)TRUE);
    }
    else ++naCount;
  }
  if (n-naCount < 1)
  {
    *value=NA_REAL;
    return(updated);
  }
  s /= (LDOUBLE)(n-naCount);
  if (R_FINITE((double)s)) {
    for (i = 0; i < n; i++) 
    {
      if (!ISNAN(x[i])) t += (x[i] - s);
    }
    s += t / ((LDOUBLE)n);
  }
  *value = (double) s;
  return(updated);
}


// --------------------- var -------------------------------------------
// If this works for all 4 types, could we simplify in other cases, too???

template<typename T>
Rboolean tvar(T *x, index_type n, double *value, Rboolean narm, T NA_VALUE)
{
  if (n < 1) 
  {
    *value = NA_REAL;
    return (Rboolean)TRUE;
  }
  tmean(x, n, value, narm, NA_VALUE);
  double avg = *value;
  
  index_type i;
  Rboolean updated = (Rboolean)TRUE;
  index_type naCount=0;
  double sum=0.0;
  T addNum;
  for (i=0; i < n; ++i) {
    addNum = static_cast<T>(x[i]);
    if (isna(addNum)) {
      if ( (Rboolean)narm == TRUE )
      {
        ++naCount;
      }
      else 
      {
        *value = NA_REAL;
        return updated;
      }
    }
    else
    {
      sum += (static_cast<double>(addNum) - avg) * 
      (static_cast<double>(addNum) - avg);
    }
  }
  if (n-naCount > 1) *value = sum/((double)(n-naCount)-1.0);
  else *value = NA_REAL;
  return(updated);

}

// This CALL_FUN macro really just does an apply on the specified columns.

#define CALL_FUN(fun)                                                        \
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);            \
  index_type i=0;                                                            \
  if (pMat->separated_columns())                                             \
  {                                                                          \
    SepMatrixAccessor<dataT> Mat(*pMat);                                     \
    for (i=0; i < nCols; ++i) {                                              \
      fun(&Mat[(index_type)pCols[i]-1][0], pMat->nrow(), &pRet[i],           \
                  (Rboolean)Rf_asLogical(narm), NA_VALUE);                  \
    }                                                                        \
  }                                                                          \
  else                                                                       \
  {                                                                          \
    MatrixAccessor<dataT> Mat(*pMat);                                        \
    for (i=0; i < nCols; ++i) {                                              \
      fun(&Mat[(index_type)pCols[i]-1][0], pMat->nrow(), &pRet[i],           \
                    (Rboolean)Rf_asLogical(narm), NA_VALUE);                \
    }                                                                        \
  }

template<typename dataT, typename retT>
void CMinCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, index_type nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tmin);
}

template<typename dataT, typename retT>
void CMaxCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, index_type nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tmax);
}

template<typename dataT, typename retT>
void CSumCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, index_type nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tsum);
}

template<typename dataT, typename retT>
void CProdCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, index_type nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tprod);
}

template<typename dataT, typename retT>
void CMeanCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, index_type nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tmean);
}

template<typename dataT, typename retT>
void CVarCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, index_type nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tvar);
}


// ----------------------------------------- externs below --------------

extern "C"
{

// The following line was taken from summary.c
#define R_INT_MIN (1+INT_MIN)

// IS THE FOLLOWING USED?
#define _(x) x

#define mainsetup()                                              \
  SEXP ret = R_NilValue;                                         \
  double *pCols = REAL(col);                             \
  index_type nCols = Rf_length(col);                            \
  int mt = Rf_asInteger(matType);

#define casesetup(TYPE, NEW_TYPE, TYPE_DATA)                     \
    ret = Rf_protect(NEW_TYPE(nCols));                              \
    TYPE *pRet = TYPE_DATA(ret);

#define NEW_NUMERIC(n)  Rf_allocVector(REALSXP,n)

#define NUMERIC_DATA(x)  REAL(x)

#define NEW_INTEGER(n)  Rf_allocVector(INTSXP,n)

#define INTEGER_DATA(x) INTEGER(x)

SEXP CMinColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  switch (mt) {
    case 1: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMinCol<char, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMinCol<short, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMinCol<int, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
        CMinCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  Rf_unprotect(1);
  return(ret);
}

SEXP CMaxColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  switch (mt) {
    case 1: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMaxCol<char, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMaxCol<short, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMaxCol<int, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
        CMaxCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  Rf_unprotect(1);
  return(ret);
}

SEXP CSumColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CSumCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        CSumCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        CSumCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        CSumCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  Rf_unprotect(1);
  return(ret);
}

SEXP CProdColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CProdCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, 
          NA_CHAR);
      } break;
    case 2: {
        CProdCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, 
          NA_SHORT);
      } break;
    case 4: {
        CProdCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, 
          NA_INTEGER);
      } break;
    case 8: {
        CProdCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, 
          NA_REAL);
      } break;
  }
  Rf_unprotect(1);
  return(ret);
}

SEXP CMeanColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CMeanCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        CMeanCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        CMeanCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        CMeanCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  Rf_unprotect(1);
  return(ret);
}

SEXP CVarColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CVarCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        CVarCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        CVarCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        CVarCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  Rf_unprotect(1);
  return(ret);
}

} // extern "C"

