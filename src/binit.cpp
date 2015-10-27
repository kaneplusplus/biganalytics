#include <iostream> // hack to make sure we are using the right "length"
                    // function
#include <Rcpp.h>
#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/bigmemoryDefines.h"
#include "bigmemory/isna.hpp"


#include <math.h>

template<typename T, typename MatrixType>
SEXP CBinIt2(MatrixType x, index_type nr, SEXP pcols,
             SEXP B1addr, SEXP B2addr)
{

  index_type i, j, k;

  double *pB1 = REAL(B1addr); 
  double *pB2 = REAL(B2addr);
  double min1 = pB1[0];
  double min2 = pB2[0];
  double max1 = pB1[1];
  double max2 = pB2[1];
  index_type nbins1 = (index_type) pB1[2];
  index_type nbins2 = (index_type) pB2[2];

  double *cols = REAL(pcols);
  index_type col1 = (index_type) cols[0] - 1;
  index_type col2 = (index_type) cols[1] - 1;

  int good;
  T *pc1 = x[col1];
  T *pc2 = x[col2];

  SEXP Rret;
  Rret = Rf_protect(Rf_allocVector(REALSXP,nbins1*nbins2));
  double *ret = REAL(Rret);

  for (i=0; i<nbins1; i++) {
    for (j=0; j<nbins2; j++) {
      ret[j*nbins1+i] = 0.0;
    }
  }

  for (k=0; k<nr; k++) {
    if ( !isna(pc1[k]) && !isna(pc2[k]) ){
      good = 1;
      if ( (((double)pc1[k])>=min1) && (((double)pc1[k])<=max1) ) {
        i = (index_type) ( nbins1 * (((double)pc1[k])-min1) / (max1-min1) );
        if (i==nbins1) i--;
      } else { good = 0; }
      if ( (((double)pc2[k])>=min2) & (((double)pc2[k])<=max2) ) {
        j = (index_type) ( nbins2 * (((double)pc2[k])-min2) / (max2-min2) );
        if (j==nbins2) j--;
      } else { good = 0; }
      if (good == 1) {
        ret[j*nbins1+i]++;
      }
    } // End only do work in there isn't an NA value
  } // End looping over all rows.

  Rf_unprotect(1);
  return(Rret);
}

template<typename T, typename MatrixType>
SEXP CBinIt1(MatrixType x, index_type nr, SEXP pcol, SEXP Baddr)
{

  index_type i, k;

  double *pB = REAL(Baddr); 
  double min = pB[0];
  double max = pB[1];
  index_type nbins = (index_type) pB[2];

  index_type col = (index_type) Rf_asReal(pcol) - 1;

  int good;
  T *pc = x[col];

  SEXP Rret;
  Rret = Rf_protect(Rf_allocVector(REALSXP,nbins));
  double *ret = REAL(Rret);

  for (i=0; i<nbins; i++) {
    ret[i] = 0.0;
  }
 
  for (k=0; k<nr; k++) {
    if ( !isna(pc[k]) ){
      good = 1;
      if ( (((double)pc[k])>=min) && (((double)pc[k])<=max) ) {
        i = (index_type) ( nbins * (((double)pc[k])-min) / (max-min) );
        if (i==(index_type)nbins) i--;
      } else { good = 0; }
      if (good == 1) {
        ret[i]++;
      }
    } // End only do work in there isn't an NA value
  } // End looping over all rows.

  Rf_unprotect(1);
  return(Rret);

}

// ----------------------------------------------------------------

extern "C"
{

SEXP binit2BigMatrix(SEXP x, SEXP cols, SEXP breaks1, SEXP breaks2)
{
  BigMatrix *pMat =  reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(x));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return CBinIt2<char>(SepMatrixAccessor<char>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
      case 2:
        return CBinIt2<short>(SepMatrixAccessor<short>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
      case 4:
        return CBinIt2<int>(SepMatrixAccessor<int>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
      case 8:
        return CBinIt2<double>(SepMatrixAccessor<double>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return CBinIt2<char>(MatrixAccessor<char>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
      case 2:
        return CBinIt2<short>(MatrixAccessor<short>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
      case 4:
        return CBinIt2<int>(MatrixAccessor<int>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
      case 8:
        return CBinIt2<double>(MatrixAccessor<double>(*pMat),
          pMat->nrow(), cols, breaks1, breaks2);
    }
  }
  return R_NilValue;
}

SEXP binit2RIntMatrix(SEXP x, SEXP cols, SEXP breaks1, SEXP breaks2)
{
  index_type numRows = static_cast<index_type>(Rf_nrows(x));
  MatrixAccessor<int> mat(INTEGER(x), numRows);
  return CBinIt2<int, MatrixAccessor<int> >(mat,
    numRows, cols, breaks1, breaks2);
}

SEXP binit2RNumericMatrix(SEXP x, SEXP cols, SEXP breaks1, SEXP breaks2)
{
  index_type numRows = static_cast<index_type>(Rf_nrows(x));
  MatrixAccessor<double> mat(REAL(x), numRows);
  return CBinIt2<double, MatrixAccessor<double> >(mat,
    numRows, cols, breaks1, breaks2);
}

SEXP binit1BigMatrix(SEXP x, SEXP col, SEXP breaks)
{
  BigMatrix *pMat =  reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(x));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return CBinIt1<char>(SepMatrixAccessor<char>(*pMat),
          pMat->nrow(), col, breaks);
      case 2:
        return CBinIt1<short>(SepMatrixAccessor<short>(*pMat),
          pMat->nrow(), col, breaks);
      case 4:
        return CBinIt1<int>(SepMatrixAccessor<int>(*pMat),
          pMat->nrow(), col, breaks);
      case 8:
        return CBinIt1<double>(SepMatrixAccessor<double>(*pMat),
          pMat->nrow(), col, breaks);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return CBinIt1<char>(MatrixAccessor<char>(*pMat),
          pMat->nrow(), col, breaks);
      case 2:
        return CBinIt1<short>(MatrixAccessor<short>(*pMat),
          pMat->nrow(), col, breaks);
      case 4:
        return CBinIt1<int>(MatrixAccessor<int>(*pMat),
          pMat->nrow(), col, breaks);
      case 8:
        return CBinIt1<double>(MatrixAccessor<double>(*pMat),
          pMat->nrow(), col, breaks);
    }
  }
  return R_NilValue;
}

SEXP binit1RIntMatrix(SEXP x, SEXP col, SEXP breaks)
{
  index_type numRows = static_cast<index_type>(Rf_nrows(x));
  MatrixAccessor<int> mat(INTEGER(x), numRows);
  return CBinIt1<int, MatrixAccessor<int> >(mat,
    numRows, col, breaks);
}

SEXP binit1RNumericMatrix(SEXP x, SEXP col, SEXP breaks)
{
  index_type numRows = static_cast<index_type>(Rf_nrows(x));
  MatrixAccessor<double> mat(REAL(x), numRows);
  return CBinIt1<double, MatrixAccessor<double> >(mat,
    numRows, col, breaks);
}


} // extern "C"
