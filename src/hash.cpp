#include <Rcpp.h>
#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/bigmemoryDefines.h"
#include "bigmemory/isna.hpp"

template<typename T, typename BMAccessorType>
SEXP MatrixHashRanges( BigMatrix *pMat, SEXP selectColumn )
{
  BMAccessorType mat(*pMat);
  index_type sc = (index_type)Rf_asReal(selectColumn)-1+pMat->col_offset();
  if (pMat->nrow()==0) return(R_NilValue);
  int uniqueValCount=1;
  T lastVal = mat[sc][0+pMat->row_offset()];
  index_type i;
  T val;
  for (i=1; i < pMat->nrow(); ++i) {
    val = mat[sc][i+pMat->row_offset()];
    if (val != lastVal) {
      lastVal = val;
      uniqueValCount += 1;
    }
  }
  SEXP ret = Rf_protect(Rf_allocVector(INTSXP, uniqueValCount*2));
  int *pRet = INTEGER(ret);
  int j=0;
  lastVal = mat[sc][0+pMat->row_offset()];
  pRet[j++]=1;
  for (i=1; i < pMat->nrow(); ++i) {
    val = mat[sc][i+pMat->row_offset()];
    if (val != lastVal) {
      pRet[j++] = i;
      pRet[j++] = i+1;
      lastVal = val;
    }
  }
  pRet[uniqueValCount*2-1] = pMat->nrow();
  Rf_unprotect(1);
  return(ret);
}


template<typename BMAccessorType>
SEXP ColCountNA( BigMatrix *pMat, SEXP column )
{
  BMAccessorType mat(*pMat);
  index_type col = (index_type)Rf_asReal(column);
  index_type i, counter;
  counter=0;
  for (i=0; i < pMat->nrow(); ++i)
  {
    if (isna(mat[col-1][i])) ++counter;
  }
  SEXP ret = Rf_protect(Rf_allocVector(REALSXP, 1));
  REAL(ret)[0] = (double)counter;
  Rf_unprotect(1);
  return(ret);
}


template<typename T1, typename BMAccessorType>
int Ckmeans2(BigMatrix *pMat, SEXP centAddr, SEXP ssAddr,
              SEXP clustAddr, SEXP clustsizesAddr,
              SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  // BIG x m
  //  T1 **x = (T1**) pMat->matrix();
  BMAccessorType x(*pMat);

  // k x m
  BigMatrix *pcent = (BigMatrix*)R_ExternalPtrAddr(centAddr);
  //double **cent = (double**) pcent->matrix();
  MatrixAccessor<double> cent(*pcent);

  // k x 1
  BigMatrix *pss = (BigMatrix*)R_ExternalPtrAddr(ssAddr);
  //double **ss = (double**) pss->matrix();
  MatrixAccessor<double> ss(*pss);

  // n x 1
  BigMatrix *pclust = (BigMatrix*)R_ExternalPtrAddr(clustAddr);
  //int **clust = (int**) pclust->matrix();
  MatrixAccessor<int> clust(*pclust);

  // k x 1
  BigMatrix *pclustsizes = (BigMatrix*)R_ExternalPtrAddr(clustsizesAddr);
  //double **clustsizes = (double**) pclustsizes->matrix();
  MatrixAccessor<double> clustsizes(*pclustsizes);

  index_type n = (index_type) Rf_asReal(nn);        // Very unlikely to need index_type, but...
  int k = Rf_asInteger(kk);                // Number of clusters
  index_type m = (index_type) Rf_asReal(mm);        // columns of data
  int maxiters = Rf_asInteger(mmaxiters); // maximum number of iterations

  int oldcluster, newcluster;           // just for ease of coding.
  int cl, bestcl;
  index_type col, j;
  double temp;
  int done = 0;
  index_type nchange;
  int iter = 0;

  vector<double> d(k);                        // Vector of distances, internal only.
  vector<double> temp1(k);
  vector<vector<double> > tempcent(m, temp1);   // For copy of global centroids k x m

  //char filename[10];
  //int junk;
  //junk = sprintf(filename, "Cfile%d.txt", Rf_asInteger(ii));
  //ofstream outFile;
  //outFile.open(filename, ios::out);
  //outFile << "This is node " << Rf_asInteger(ii) << endl;
  //outFile << "Before do: n, i, k, m, p, start:" <<
  //       n << ", " << i << ", " << k << ", " << m << ", " << p <<
  //       ", " << start << endl;


  // Before starting the loop, we only have cent (centers) as passed into the function.
  // Calculate clust and clustsizes, then update cent as centroids.

  for (cl=0; cl<k; cl++) clustsizes[0][cl] = 0;
  for (j=0; j<n; j++) {
    bestcl = 0;
    for (cl=0; cl<k; cl++) {
      d[cl] = 0.0;
      for (col=0; col<m; col++) {
        temp = (double)x[col][j] - cent[col][cl];
        d[cl] += temp * temp;
      }
      if (d[cl]<d[bestcl]) bestcl = cl;
    }
    clust[0][j] = bestcl + 1;
    clustsizes[0][bestcl]++;
    for (col=0; col<m; col++)
      tempcent[col][bestcl] += (double)x[col][j];
  }
  for (cl=0; cl<k; cl++)
    for (col=0; col<m; col++)
      cent[col][cl] = tempcent[col][cl] / clustsizes[0][cl];

  do {

    nchange = 0;
    for (j=0; j<n; j++) { // For each of my points, this is offset from hash position

      oldcluster = clust[0][j] - 1;
      bestcl = 0;
      for (cl=0; cl<k; cl++) {         // Consider each of the clusters
        d[cl] = 0.0;                   // We'll get the distance to this cluster.
        for (col=0; col<m; col++) {    // Loop over the dimension of the data
          temp = (double)x[col][j] - cent[col][cl];
          d[cl] += temp * temp;
        }
        if (d[cl]<d[bestcl]) bestcl = cl;
      } // End of looking over the clusters for this j

      if (d[bestcl] < d[oldcluster]) {           // MADE A CHANGE!
        newcluster = bestcl;
        clust[0][j] = newcluster + 1;
        nchange++;
        clustsizes[0][newcluster]++;
        clustsizes[0][oldcluster]--;
        for (col=0; col<m; col++) {
          cent[col][oldcluster] += ( cent[col][oldcluster] - (double)x[col][j] ) / clustsizes[0][oldcluster];
          cent[col][newcluster] += ( (double)x[col][j] - cent[col][newcluster] ) / clustsizes[0][newcluster];
        }
      }

    } // End of this pass over my points.

    iter++;
    if ( (nchange==0) || (iter>=maxiters) ) done = 1;

  } while (done==0);

  // Collect the sums of squares now that we're done.

// TODO: The following lines were causing the compiler error.
//    iter++;
//    if ( (nchange==0) || (iter>=maxiters) ) done = 1;
//
//  } while (done==0);

  // Collect the sums of squares now that we're done.
  for (cl=0; cl<k; cl++) ss[0][cl] = 0.0;
  for (j=0; j<n; j++) {
    for (col=0; col<m; col++) {
      cl = clust[0][j]-1;
      temp = (double)x[col][j] - cent[col][cl];
      ss[0][cl] += temp * temp;
    }
  }

  // At this point, cent is the centers, ss is the within-groups sums of squares,
  // clust is the cluster memberships, clustsizes is the cluster sizes.

  //outFile.close();
  return iter;

}

extern "C" {

SEXP MatrixHashRanges( SEXP bigMatAddr, SEXP selectColumn )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return MatrixHashRanges<char, SepMatrixAccessor<char> >(
          pMat, selectColumn);
      case 2:
        return MatrixHashRanges<short, SepMatrixAccessor<short> >(
          pMat, selectColumn);
      case 4:
        return MatrixHashRanges<int, SepMatrixAccessor<int> >(
          pMat, selectColumn);
      case 8:
        return MatrixHashRanges<double, SepMatrixAccessor<double> >(
          pMat, selectColumn);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return MatrixHashRanges<char, MatrixAccessor<char> >(
          pMat, selectColumn);
      case 2:
        return MatrixHashRanges<short, MatrixAccessor<short> >(
          pMat, selectColumn);
      case 4:
        return MatrixHashRanges<int, MatrixAccessor<int> >(
          pMat, selectColumn);
      case 8:
        return MatrixHashRanges<double, MatrixAccessor<double> >(
          pMat, selectColumn);
    }
  }
  return R_NilValue;
}


SEXP ColCountNA(SEXP address, SEXP column)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return ColCountNA< SepMatrixAccessor<char> >(pMat, column);
      case 2:
        return ColCountNA< SepMatrixAccessor<short> >(pMat, column);
      case 4:
        return ColCountNA< SepMatrixAccessor<int> >(pMat, column);
      case 8:
        return ColCountNA< SepMatrixAccessor<double> >(pMat, column);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return ColCountNA< MatrixAccessor<char> >(pMat, column);
      case 2:
        return ColCountNA< MatrixAccessor<short> >(pMat, column);
      case 4:
        return ColCountNA< MatrixAccessor<int> >(pMat, column);
      case 8:
        return ColCountNA< MatrixAccessor<double> >(pMat, column);
    }
  }
  return R_NilValue;
}

// TODO: There is a Ckmeans2 in both hash.cpp and kmeans.cpp, which
//       one should be used?
// TODO: Ckmeans2 is probably failing because I am only making
// a matrix accessor for pMat.  This needs to be done for ALL
// big matrix objects being used.
/*
SEXP Ckmeans2main(SEXP matType,
                  SEXP bigMatrixAddr, SEXP centAddr, SEXP ssAddr,
                  SEXP clustAddr, SEXP clustsizesAddr,
                  SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  SEXP ret = Rf_protect(Rf_allocVector(REALSXP, 1));
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  int iter = 0;
  if (pMat->separated_columns())
  {
    switch (Rf_asInteger(matType))
    {
      case 1:
        iter = Ckmeans2<char, SepMatrixAccessor<char> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 2:
        iter = Ckmeans2<short, SepMatrixAccessor<short> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 4:
        iter = Ckmeans2<int, SepMatrixAccessor<int> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 8:
        iter = Ckmeans2<double, SepMatrixAccessor<double> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
    }
  }
  else
  {
    switch (Rf_asInteger(matType))
    {
      case 1:
        iter = Ckmeans2<char, MatrixAccessor<char> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 2:
        iter = Ckmeans2<short, MatrixAccessor<short> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 4:
        iter = Ckmeans2<int, MatrixAccessor<int> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 8:
        iter = Ckmeans2<double, MatrixAccessor<double> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
    }
  }
  REAL(ret)[0] = (double)iter;
  Rf_unprotect(1);
  return ret;
}
*/
}
